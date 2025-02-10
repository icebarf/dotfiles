#eval "$(ssh-agent -s -a "$SSH_AUTH_SOCK")"

source $HOME/.zshenv

#export $(dbus-launch)
#export MOZ_WAYLAND_DRM_DEVICE="/dev/dri/renderD128"
#doas mkdir --parents /run/dbus/
#doas dbus-daemon --system &

#setxkbmap -option caps:swapescape

#yomitan_audio_server &

#sudo modprobe usbnet
#sudo modprobe cdc_ether

#doas rmmod acpi_call > /dev/null
#doas modprobe acpi_call > /dev/null
#doas turn_off_gpu > /dev/null

#exec wifi_cat &
#exec pipewire &
#exec pipewire-pulse &
#exec wireplumber &
#exec tlp init start &
#exec emacs --daemon &
#exec sway
exec sudo ydotoold &
