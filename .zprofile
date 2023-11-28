eval "$(ssh-agent -s -a "$SSH_AUTH_SOCK")"

source /etc/profile
source $HOME/.zshenv

#sudo modprobe usbnet
#sudo modprobe cdc_ether

#doas rmmod acpi_call > /dev/null
#doas modprobe acpi_call > /dev/null
#doas turn_off_gpu > /dev/null

#pipewire &
#/usr/bin/emacs --daemon &
#exec sway
