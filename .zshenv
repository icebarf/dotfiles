#. "/etc/zprofile"

#REPODIR="$HOME/.local/share/kiss/repos"
#export KISS_PATH=""

# iceland
#KISS_PATH="$KISS_PATH:$REPODIR/iceland/editors"
#KISS_PATH="$KISS_PATH:$REPODIR/iceland/fonts"
#KISS_PATH="$KISS_PATH:$REPODIR/iceland/overrides"
#KISS_PATH="$KISS_PATH:$REPODIR/iceland/utils"

# repo
#KISS_PATH="$KISS_PATH:$REPODIR/repo/core"
#KISS_PATH="$KISS_PATH:$REPODIR/repo/extra"
#KISS_PATH="$KISS_PATH:$REPODIR/repo/wayland"

# community
#KISS_PATH="$KISS_PATH:$REPODIR/community/community"
#KISS_PATH="$KISS_PATH:$REPODIR/dbus/dbus"

#export KISS_MAINTAINED="$HOME/software/maintainer/community/community/"

#export KISS_SU="doas"
#export SU_PROG="doas"
#export KISS_TMPDIR="/mnt/kiss-tmp"

# build flags
#export CFLAGS="-O2 -pipe -march=native -mtune=native"
#export CXXFLAGS="$CFLAGS"
export MAKEFLAGS="-j12"

# XDG envvars
#if test -z "$XDG_RUNTIME_DIR"; then
#		export XDG_RUNTIME_DIR=""
#		XDG_RUNTIME_DIR="/run/user/$(id -u)"
#	if ! test -d "$XDG_RUNTIME_DIR"; then
#		doas mkdir --parents "$XDG_RUNTIME_DIR"
#		doas chmod 0750 "$XDG_RUNTIME_DIR"
#		doas chown -R ice:wheel "$XDG_RUNTIME_DIR"
#	fi
#fi

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_HOME"
#export XDG_DATA_DIRS="$XDG_DATA_DIRS:$XDG_DATA_HOME"

export XDG_DESKTOP_DIR="$HOME/Desktop"
export XDG_DOCUMENTS_DIR="$HOME/Documents"
export XDG_DOWNLOADS_DIR="$HOME/Downloads"
export XDG_PICTURES_DIR="$HOME/Pictures"
export XDG_VIDEOS_DIR="$HOME/Videos"

#export XDG_CURRENT_DESKTOP=sway

# other envvars
export RUSTUP_HOME="$HOME/.local/share/rustup"
export CARGO_HOME="$HOME/.local/share/cargo"
export LANG=en_US.UTF-8
#export LANG=ja_JP.UTF-8
#export GRIM_DEFAULT_DIR="$XDG_PICTURES_DIR/Screenshots"
PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/x-tools/arm-kindlepw2-linux-gnueabi/bin"
#export MOZ_WAYLAND_DRM_DEVICE=/dev/dri/renderD128
#export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

export PICO_SDK_PATH="$HOME/.pico-sdk"

## Chroot for testing arch packages
export CHROOT="$HOME/AUR/chroot"

# Japanese localisation
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS="@im=ibus"
export GLFW_IM_MODULE=ibus

# ibus specific
export MOZC_IBUS_CANDIDATE_WINDOW="ibus"
xset r rate 250 50
#set -o emacs

