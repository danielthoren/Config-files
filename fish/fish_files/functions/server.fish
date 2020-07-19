alias sfconnl="sudo sshfs -o allow_other -p 2022 daniel5908@192.168.1.100:/srv/dev-disk-by-label-storage /mnt/server ; \
               sudo sshfs -o allow_other -p 2022 daniel5908@192.168.1.100:/srv/dev-disk-by-label-download /mnt/download"

alias sfconn="sudo sshfs -o allow_other -p 2022 daniel5908@155.4.155.164:/srv/dev-disk-by-label-storage /mnt/server ;
              sudo sshfs -o allow_other -p 2022 daniel5908@155.4.155.164:/srv/dev-disk-by-label-download /mnt/download"

alias sfdiss="sudo umount -f /mnt/server ; \
              sudo umount -f /mnt/download"
              

alias swconn="~/.config/fish/functions/server_connect.sh"