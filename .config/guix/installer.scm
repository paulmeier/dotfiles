;;; installer.scm — custom Guix installation image for the Zephyrus G14.
;;;
;;; Bakes in the nonfree kernel + firmware (so the MediaTek MT7921 Wi-Fi works
;;; during installation) and git (so you can clone this repo in the installer
;;; without `guix shell`).
;;;
;;; Build on an existing Guix machine, from this repo checkout:
;;;
;;;     guix time-machine -C channels.scm -- \
;;;       system image -t iso9660 installer.scm
;;;
;;; then dd the printed /gnu/store/...-image.iso to a USB stick:
;;;
;;;     sudo dd if=/gnu/store/...-image.iso of=/dev/sdX bs=4M \
;;;       status=progress oflag=direct && sync

(use-modules (gnu)
             (gnu packages)
             (gnu system install)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(operating-system
  (inherit installation-os)
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (packages
   (cons (specification->package "git")
         (operating-system-packages installation-os))))
