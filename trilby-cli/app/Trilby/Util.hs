module Trilby.Util where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Shelly
import System.Console.ANSI
import System.IO (hFlush, stderr, stdout)
import Prelude hiding (error)

error :: MonadIO m => Text -> m ()
error t = liftIO do
    hSetSGR stderr [SetColor Foreground Dull Red]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

warn :: MonadIO m => Text -> m ()
warn t = liftIO do
    hSetSGR stderr [SetColor Foreground Dull Yellow]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

info :: MonadIO m => Text -> m ()
info t = liftIO do
    hSetSGR stderr [SetColor Foreground Dull White]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

peval :: (Text -> IO ()) -> Sh a -> Sh a
peval f = print_commands True . print_commands_with f

ask :: String -> Bool -> Sh Bool
ask question defaultValue = do
    answer <- liftIO do
        putStr $
            question <> case defaultValue of
                True -> " [Y/n] "
                False -> " [y/N] "
        hFlush stdout
        getLine
    case answer of
        "" -> pure defaultValue
        'y' : _ -> pure True
        'Y' : _ -> pure True
        'n' : _ -> pure False
        'N' : _ -> pure False
        _ -> do
            liftIO $ error "unrecognised input"
            ask question defaultValue

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
