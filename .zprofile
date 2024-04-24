eval "$(ssh-agent -s -a "$SSH_AUTH_SOCK")"

source $HOME/.zshenv

#setxkbmap -option caps:swapescape

#yomitan_audio_server &

#sudo modprobe usbnet
#sudo modprobe cdc_ether

#doas rmmod acpi_call > /dev/null
#doas modprobe acpi_call > /dev/null
#doas turn_off_gpu > /dev/null

exec pipewire &
exec tlp init start &
#/usr/bin/emacs --daemon &
exec sway
