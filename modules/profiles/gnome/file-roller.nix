{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; with gnome; with gnomeExtensions; [
    file-roller
  ];

  xdg.mime.inverted.defaultApplications."org.gnome.FileRoller.desktop" = [
    "application/vnd.android.package-archive"
    "application/vnd.debian.binary-package"
    "application/vnd.ms-cab-compressed"
    "application/x-ace"
    "application/x-alz"
    "application/x-ar"
    "application/x-archive"
    "application/x-arj"
    "application/x-brotli"
    "application/x-bzip-brotli-tar"
    "application/x-bzip1"
    "application/x-bzip1-compressed-tar"
    "application/x-cabinet"
    "application/x-chrome-extension"
    "application/x-deb"
    "application/x-ear"
    "application/x-gtar"
    "application/x-java-archive"
    "application/x-lhz"
    "application/x-lrzip"
    "application/x-lrzip-compressed-tar"
    "application/x-lz4"
    "application/x-lz4-compressed-tar"
    "application/x-lzop"
    "application/x-lzop-compressed-tar"
    "application/x-ms-dos-executable"
    "application/x-ms-wim"
    "application/x-rar"
    "application/x-rar-compressed"
    "application/x-rzip"
    "application/x-rzip-compressed-tar"
    "application/x-source-rpm"
    "application/x-stuffit"
    "application/x-war"
    "application/x-zip"
    "application/x-zip-compressed"
    "application/x-zoo"
    "application/bzip2"
    "application/gzip"
    "application/x-7z-compressed"
    "application/x-7z-compressed-tar"
    "application/x-bzip"
    "application/x-bzip-compressed-tar"
    "application/x-compress"
    "application/x-compressed-tar"
    "application/x-cpio"
    "application/x-gzip"
    "application/x-lha"
    "application/x-lzip"
    "application/x-lzip-compressed-tar"
    "application/x-lzma"
    "application/x-lzma-compressed-tar"
    "application/x-tar"
    "application/x-tarz"
    "application/x-xar"
    "application/x-xz"
    "application/x-xz-compressed-tar"
    "application/zip"
  ];
}