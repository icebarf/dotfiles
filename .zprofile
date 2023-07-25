eval "$(ssh-agent -s -a "$SSH_AUTH_SOCK")" 2>&1 /dev/null 

. ~/.zshenv

doas rmmod acpi_call 2>&1 /dev/null 
doas modprobe acpi_call 2>&1 /dev/null 
doas turn_off_gpu 2>&1 /dev/null 

pipewire -c "$XDG_CONFIG_HOME/pipewire/pipewire.conf" &
/usr/bin/emacs --daemon &
exec sway
