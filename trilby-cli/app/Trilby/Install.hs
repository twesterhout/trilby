{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Trilby.Install where

import Control.Monad
import Data.List qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Language.Haskell.TH qualified as TH
import Shelly
import System.Environment (getArgs, getExecutablePath)
import System.Posix (executeFile, getEffectiveUserID, getFileStatus, isBlockDevice)
import Trilby.Config (Edition (..))
import Trilby.Options
import Trilby.Util
import Prelude hiding (error)

getDisk :: Sh Text
getDisk = do
    disk <- liftIO do
        putStrLn "Choose installation disk:"
        Text.getLine
    diskStatus <- liftIO $ getFileStatus $ Text.unpack disk
    if isBlockDevice diskStatus
        then pure disk
        else do
            liftIO $ error $ "cannot find disk: " <> disk
            getDisk

getOpts :: InstallOpts Maybe -> InstallOpts Sh
getOpts opts = do
    InstallOpts
        { efi = maybe (ask "Use EFI boot?" True) pure opts.efi
        , luks = maybe (ask "Encrypt the disk with LUKS2?" True) pure opts.luks
        , disk = maybe getDisk pure opts.disk
        , format = maybe (ask "Format the disk?" True) pure opts.format
        , edition = maybe (pure Workstation) pure opts.edition
        , host = maybe (liftIO $ putStrLn "Choose hostname:" >> Text.getLine) pure opts.host
        , username = maybe (liftIO $ putStrLn "Choose admin username:" >> Text.getLine) pure opts.username
        , reboot = maybe (ask "Reboot system?" True) pure opts.reboot
        }

efiLabel :: Text
efiLabel = "EFI"

luksLabel :: Text
luksLabel = "LUKS"

trilbyLabel :: Text
trilbyLabel = "Trilby"

luksDevice :: Text
luksDevice = "/dev/disk/by-partlabel/" <> luksLabel

luksName :: Text
luksName = "cryptroot"

luksOpenDevice :: Text
luksOpenDevice = "/dev/mapper/" <> luksName

trilbyDevice :: Text
trilbyDevice = "/dev/disk/by-label/" <> trilbyLabel

efiDevice :: Text
efiDevice = "/dev/disk/by-partlabel/" <> efiLabel

rootMount :: Text
rootMount = "/mnt"

rootVol :: Text
rootVol = rootMount <> "/root"

bootVol :: Text
bootVol = rootMount <> "/boot"

homeVol :: Text
homeVol = rootMount <> "/home"

nixVol :: Text
nixVol = rootMount <> "/nix"

trilbyDir :: Text
trilbyDir = rootMount <> "/etc/trilby"

flakeTemplate :: Text
flakeTemplate = $(TH.stringE . Text.unpack <=< TH.runIO . Text.readFile $ "assets/install/flake.nix")

hostTemplate :: Text
hostTemplate = $(TH.stringE . Text.unpack <=< TH.runIO . Text.readFile $ "assets/install/host.nix")

userTemplate :: Text
userTemplate = $(TH.stringE . Text.unpack <=< TH.runIO . Text.readFile $ "assets/install/user.nix")

applySubstitutions :: [(Text, Text)] -> Text -> Text
applySubstitutions subs str =
    Data.List.foldl' (\acc (from, to) -> Text.replace from to acc) str subs

install :: InstallOpts Maybe -> IO ()
install (getOpts -> opts) = shelly do
    unlessM ((0 ==) <$> liftIO getEffectiveUserID) do
        whenM (ask "The installer requires root permissions. Elevate to root? (sudo)" True) $ liftIO do
            exe <- getExecutablePath
            args <- getArgs
            executeFile "sudo" True (exe : args) Nothing
    unlessM ((0 ==) <$> liftIO getEffectiveUserID) do
        (errorExit "Trilby installation cannot proceed without root")
    whenM opts.format $ doFormat opts
    rootIsMounted <- do
        errExit False $ cmd "mountpoint" "-q" rootMount
        (0 ==) <$> lastExitCode
    unless rootIsMounted $ errorExit "/mnt is not a mountpoint"
    cmd "mkdir" "-p" trilbyDir
    cd $ Text.unpack trilbyDir
    host <- opts.host
    username <- opts.username
    edition <- opts.edition
    cmd "mkdir" "-p" ("hosts/" <> host) ("users/" <> username)
    let substitute =
            applySubstitutions
                [ ("$hostname", host)
                , ("$username", username)
                , ("$edition", tshow edition)
                , ("$channel", "unstable")
                ]
    liftIO do
        Text.writeFile (Text.unpack (trilbyDir <> "/flake.nix")) $
            substitute flakeTemplate
        Text.writeFile (Text.unpack (trilbyDir <> "/hosts/" <> host <> "/default.nix")) $
            substitute hostTemplate
        Text.writeFile (Text.unpack (trilbyDir <> "/users/" <> username <> "/default.nix")) $
            substitute userTemplate
    let hardwareConfig = Text.unpack (trilbyDir <> "/hosts/" <> host <> "/hardware.nix")
    cmd "nixos-generate-config" "--show-hardware-config" "--root" rootMount
        >>= Shelly.writefile hardwareConfig
    cmd "nixos-install" "--flake" (trilbyDir <> "#" <> host) "--no-root-password"
    whenM opts.reboot $ cmd "reboot"

doFormat :: InstallOpts Sh -> Sh ()
doFormat opts = do
    disk <- opts.disk
    cmd "sgdisk" "--zap-all" "-o" disk
    efi <- opts.efi
    when efi do
        cmd "sgdisk" "-n" "1:0:+1G" "-t" "1:EF00" "-c" ("1:" <> efiLabel) disk
    luks <- opts.luks
    let rootPartNum = if efi then 2 else 1
    let rootLabel = if luks then luksLabel else trilbyLabel
    cmd "sgdisk" "-n" (tshow rootPartNum <> ":0:0") "-t" (tshow rootPartNum <> ":8300") "-c" (tshow rootPartNum <> ":" <> rootLabel) disk
    cmd "partprobe"
    when efi do
        cmd "mkfs.fat" "-F32" "-n" efiLabel efiDevice
    when luks do
        cmd "cryptsetup" "luksFormat" luksDevice
        cmd "cryptsetup" "luksOpen" luksDevice luksName
    cmd "mkfs.btrfs" "-f" "-L" trilbyLabel $ if luks then luksOpenDevice else trilbyDevice
    cmd "partprobe"
    cmd "mount" trilbyDevice rootMount
    cmd "btrfs" "subvolume" "create" rootVol
    cmd "btrfs" "subvolume" "create" homeVol
    cmd "btrfs" "subvolume" "create" nixVol
    unless efi do
        cmd "btrfs" "subvolume" "create" bootVol
    cmd "unmount" rootMount
    cmd "mount" "-o" "subvol=root,compress=zstd,noatime" trilbyDevice rootMount
    cmd "mkdir" "-p" bootVol homeVol nixVol
    if efi
        then cmd "mount" efiDevice bootVol
        else cmd "mount" "-o" "subvol=boot" trilbyDevice bootVol
    cmd "mount" "-o" "subvol=home,compress=zstd" trilbyDevice homeVol
    cmd "mount" "-o" "subvol=nix,compress=zstd,noatime" trilbyDevice nixVol
