#!/bin/sh
REPODIR="$HOME/software/repos"
export KISS_PATH=""
KISS_PATH="$KISS_PATH:$REPODIR/iceland/core"
KISS_PATH="$KISS_PATH:$REPODIR/iceland/fonts"
KISS_PATH="$KISS_PATH:$REPODIR/iceland/personal"
KISS_PATH="$KISS_PATH:$REPODIR/iceland/utils"
KISS_PATH="$KISS_PATH:$REPODIR/grepo/core"
KISS_PATH="$KISS_PATH:$REPODIR/grepo/extra"
KISS_PATH="$KISS_PATH:$REPODIR/grepo/wayland"
KISS_PATH="$KISS_PATH:$REPODIR/community/community"

export KISS_MAINTAINED="$HOME/software/maintainer/community/community/"

. /etc/profile

export KISS_SU="doas"

export CFLAGS="-O2 -pipe -march=native -mtune=native"
export CXXFLAGS="$CFLAGS"
export MAKEFLAGS="-j16"

if test -z "$XDG_RUNTIME_DIR"; then
		export XDG_RUNTIME_DIR=""
		XDG_RUNTIME_DIR="/run/user/$(id -u)"
	if ! test -d "$XDG_RUNTIME_DIR"; then
		doas mkdir --parents "$XDG_RUNTIME_DIR"
		doas chmod 0750 "$XDG_RUNTIME_DIR"
		doas chown -R ice:wheel "$XDG_RUNTIME_DIR"
	fi
fi

export MOZ_WAYLAND_DRM_DEVICE=/dev/dri/renderD128
export LANG=en_US.UTF-8
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

eval "$(ssh-agent -s -a "$SSH_AUTH_SOCK")" 2>&1 /dev/null 

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_HOME"
export XDG_DATA_DIR="/usr/share:$XDG_DATA_HOME"
export XDG_PICTURES_DIR="$HOME/Pictures"
export GRIM_DEFAULT_DIR="$XDG_PICTURES_DIR/Screenshots"

export RUSTUP_HOME="$HOME/.local/share/rustup"
export CARGO_HOME="$HOME/.local/share/cargo"

export PATH="$PATH:$HOME/.local/usr/bin"

doas rmmod acpi_call 2>&1 /dev/null 
doas modprobe acpi_call 2>&1 /dev/null 
doas turn_off_gpu 2>&1 /dev/null 

pipewire -c "$XDG_CONFIG_HOME/pipewire/pipewire.conf" &
/usr/bin/emacs --daemon &
exec sway
