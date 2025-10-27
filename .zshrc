# Path to your oh-my-zsh installation.
#installation via script from github

export ZSH="$HOME/.local/share/oh-my-zsh"
export ZSH_COMPDUMP="$XDG_CACHE_HOME/zsh/.zcompdump-$HOST"
export HISTFILE="$HOME/.cache/zsh/zsh_history"

ZSH_THEME="half-life"

source $ZSH/oh-my-zsh.sh

plugins=(git
        colored-man-pages)

if [ $SHELL = "/usr/bin/zsh" ]; then
    export GPG_TTY=$TTY
else
    export GPG_TTY=$(tty)
fi

export SU_PROG="sudo"
export EDITOR="nvim"
#export VISUAL="emacsc"

# syntax highlighting
highlighter="/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
autosuggestions="/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
historysearch="/usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh"

if [ -e $highlighter ] ; then
	source $highlighter
fi

if [ -e $autosuggestions ] ; then
	source $autosuggestions
fi

if [ -e $historysearch ] ; then
	source $historysearch
fi

# Aliases

alias agent-steins='WINEFSYNC=1 WINEPREFIX="/home/ice/.local/share/Steam/steamapps/compatdata/412830/pfx" /home/ice/.steam/steam/compatibilitytools.d/GE-Proton10-4/files/bin/wine Programs/agent-windows/agent.exe'

alias remacs="pkill emacs && /usr/bin/emacs --daemon &"

#continue download
alias wget="wget -c"

#add new fonts
alias update-fc='fc-cache -fv'

#youtube download
alias yta-aac="yt-dlp --extract-audio --audio-format aac "
alias yta-best="yt-dlp --extract-audio --audio-format best "
alias yta-flac="yt-dlp --extract-audio --audio-format flac "
alias yta-mp3="yt-dlp --extract-audio --audio-format mp3 "
alias ytv-best="yt-dlp -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/bestvideo+besta
udio' --merge-output-format mp4 "

#gpg
#verify signature for isos
alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
alias fix-gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"

#fixes
alias fix-permissions="$SU_PROG chown -R $USER:$USER ~/.config ~/.local"

alias vim='nvim'

make_kindle_wall ()
{
    if [ -f $1 ]; then
        convert $1 -colorspace Lab -filter LanczosSharp \
        -distort Resize 1236x1648 -colorspace sRGB \
        -background black -gravity center -extent 1236x1648! \
        -grayscale Rec709Luminance -colorspace sRGB \
        -dither Riemersma \
        -remap /home/ice/Pictures/KindleWallpapers/eink_cmap.gif \
        -quality 75 png:out.png
    fi
}

# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   tar xf $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# kiss alternative
kissalt ()
{
  kiss a | grep $1 | kiss a -
}

clearGearthCache()
{
  rm -rf ~/.googleearth/Cache/
}

#remove
alias rmgitcache="rm -r ~/.cache/git"

# fan control
alias fan_control='$SU_PROG $EDITOR \
		/sys/devices/platform/asus-nb-wmi/throttle_thermal_policy'
alias swayconfig="$EDITOR $XDG_CONFIG_HOME/sway/config"

eval "$(zoxide init zsh)"
