. "/etc/zprofile"
#. "$HOME/.local/share/cargo/env"

REPODIR="$HOME/.local/share/kiss/repos"
export KISS_PATH=""

# iceland
KISS_PATH="$KISS_PATH:$REPODIR/iceland/core"
KISS_PATH="$KISS_PATH:$REPODIR/iceland/fonts"
KISS_PATH="$KISS_PATH:$REPODIR/iceland/overrides"
KISS_PATH="$KISS_PATH:$REPODIR/iceland/personal"
KISS_PATH="$KISS_PATH:$REPODIR/iceland/utils"

# testuser
#KISS_PATH="$KISS_PATH:$REPODIR/testuser/dbus"

# grepo
KISS_PATH="$KISS_PATH:$REPODIR/grepo/core"
KISS_PATH="$KISS_PATH:$REPODIR/grepo/extra"
KISS_PATH="$KISS_PATH:$REPODIR/grepo/wayland"

# community
KISS_PATH="$KISS_PATH:$REPODIR/community/community"

export KISS_MAINTAINED="$HOME/software/maintainer/community/community/"

export KISS_SU="doas"
export KISS_TMPDIR="/mnt/kiss-tmp"

# build flags
export CFLAGS="-O2 -pipe -march=native -mtune=native"
export CXXFLAGS="$CFLAGS"
export MAKEFLAGS="-j16"

# XDG envvars
if test -z "$XDG_RUNTIME_DIR"; then
		export XDG_RUNTIME_DIR=""
		XDG_RUNTIME_DIR="/run/user/$(id -u)"
	if ! test -d "$XDG_RUNTIME_DIR"; then
		doas mkdir --parents "$XDG_RUNTIME_DIR"
		doas chmod 0750 "$XDG_RUNTIME_DIR"
		doas chown -R ice:wheel "$XDG_RUNTIME_DIR"
	fi
fi

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_HOME"
export XDG_DATA_DIR="/usr/share:$XDG_DATA_HOME"
export XDG_PICTURES_DIR="$HOME/Pictures"

# other envvars
export RUSTUP_HOME="$HOME/.local/share/rustup"
export CARGO_HOME="$HOME/.local/share/cargo"
export LANG=en_US.UTF-8
export GRIM_DEFAULT_DIR="$XDG_PICTURES_DIR/Screenshots"
export PATH="$PATH:$HOME/.local/bin"
export MOZ_WAYLAND_DRM_DEVICE=/dev/dri/renderD128
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
#export BEMENU_OPTS="--nf #94e2d5 --af #94e2d5 --tf #f5c2e7 --hf #f5c2e7 --bindings vim"

