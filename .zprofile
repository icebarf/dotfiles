eval "$(ssh-agent -s -a "$SSH_AUTH_SOCK")" > /dev/null

. ~/.zshenv

doas rmmod acpi_call > /dev/null
doas modprobe acpi_call > /dev/null
doas turn_off_gpu > /dev/null

pipewire &
#/usr/bin/emacs --daemon &
exec sway
