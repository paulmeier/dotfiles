;;; config-vm.scm — GNU Guix System for the aarch64 VM on the MacBook (Apple M5)
;;;
;;; Companion to guix-vm-macbook-m5-daily-driver.org (Steps 2, 5, 8).
;;; The hardware here is *virtual* — UTM with the Apple Virtualization backend:
;;;
;;;   CPU/RAM  whatever you assigned the VM (Step 3: 6–8 CPUs, 8–16 GB)
;;;   GPU      virtio-gpu (virgl) — drives the Sway/Wayland session
;;;   Disk     virtio-blk, matched by GPT *label* (root + EFI), like the others
;;;   Net      virtio-net + DHCP
;;;
;;; Differences from the G14 (config.scm):
;;;   - Stock *libre* kernel (no nonguix): virtio drivers are in-tree and free,
;;;     so there is no nonfree firmware or NVIDIA stack to load inside a VM.
;;;   - GRUB-EFI straight to the VM-provided UEFI (no MUX/Optimus juggling).
;;;   - virtio modules forced into the initrd so root + net come up early.
;;;   - Plain ext4 root: the macOS host disk is already FileVault-encrypted, so
;;;     this is a defensible choice. Add LUKS only if you want defense-in-depth.
;;;   - console=hvc0 so you can read the boot from the host if the GPU misbehaves.
;;;
;;; Build the disk image (on the Reform / any aarch64 Guix box, no emulation):
;;;   guix time-machine -C channels.scm -- system image -t qcow2 config-vm.scm
;;; Day-to-day, inside the VM (Step 8):
;;;   sudo guix time-machine -C channels.scm -- system reconfigure config-vm.scm

(use-modules (gnu)
             (gnu system nss)
             (gnu system linux-initrd))      ; %base-initrd-modules

(use-service-modules base desktop greetd networking ssh)

(define %keeb (keyboard-layout "us"))

(operating-system
  (host-name "chronos")
  (timezone "America/Chicago")
  (locale "en_US.utf8")
  (keyboard-layout %keeb)

  ;; ---- Kernel / initrd (libre — virtio is in-tree) ---------------------
  ;; Force the virtio modules into the initrd so the root disk and the network
  ;; are available before the real root is mounted.
  (initrd-modules
   (append '("virtio_pci" "virtio_blk" "virtio_net" "virtio_gpu" "virtio_console")
           %base-initrd-modules))
  (kernel-arguments
   (append '("console=hvc0")               ; serial console readable from the host
           %default-kernel-arguments))

  ;; ---- Disk / boot (UEFI is provided by the VM) ------------------------
  (bootloader
   (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets (list "/boot/efi"))
     (keyboard-layout %keeb)))

  ;; Disks matched by GPT label so the image stays machine-agnostic, exactly
  ;; like the G14/Reform. Set the labels when you partition the VM disk:
  ;;   mkfs.fat -F32 -n EFI  /dev/vda1
  ;;   mkfs.ext4    -L root  /dev/vda2
  (file-systems
   (cons* (file-system
            (mount-point "/")
            (device (file-system-label "root"))
            (type "ext4"))
          (file-system
            (mount-point "/boot/efi")
            (device (file-system-label "EFI"))
            (type "vfat"))
          %base-file-systems))

  ;; ---- Users ------------------------------------------------------------
  (users
   (cons (user-account
           (name "paulmeier")                ; matches the Ubuntu bootstrap account
           (comment "Paul")
           (group "users")
           (home-directory "/home/paulmeier")
           (supplementary-groups
            '("wheel"        ; sudo
              "netdev"       ; NetworkManager
              "audio" "video" "input"
              "seat"         ; seatd (Sway)
              "kvm" "tty")))
         %base-user-accounts))

  ;; ---- Packages ---------------------------------------------------------
  ;; Same Sway/Wayland set as the G14, minus everything NVIDIA. mesa-utils adds
  ;; glxinfo so you can confirm the virgl renderer (org note Step 4).
  (packages
   (append
    (map specification->package
         '(;; Sway + Wayland essentials
           "sway" "swaylock" "swayidle" "swaybg"
           "waybar" "wofi" "foot" "mako"
           "grim" "slurp" "wl-clipboard"
           "brightnessctl" "playerctl"
           ;; audio
           "pipewire" "wireplumber" "pavucontrol"
           ;; screen-share / portals on wlroots
           "xdg-desktop-portal" "xdg-desktop-portal-wlr"
           ;; net / misc
           "network-manager-applet"
           "git" "curl" "gnu-make"
           "nss-certs"
           "mesa-utils"                      ; glxinfo (confirm virtio-gpu/virgl)
           "font-dejavu" "font-google-noto"))
    %base-packages))

  ;; ---- Services ---------------------------------------------------------
  (services
   (cons*
    ;; seatd — seat management for Sway/wlroots.
    (service seatd-service-type)

    ;; SSH — handy to reach the guest from the macOS host.
    (service openssh-service-type)

    ;; greetd -> wlgreet -> Sway (Wayland-native login), same as the G14.
    ;; This lands you in Sway after a single login tap. For the fully hands-off
    ;; "boots into Guix" feel, replace the vt1 terminal below with a true
    ;; autologin session, e.g.:
    ;;   (greetd-terminal-configuration
    ;;     (terminal-vt "1") (terminal-switch #t)
    ;;     (default-session-command
    ;;       (greetd-agreety-session             ; or a sway-launching wrapper
    ;;         (command (file-append sway "/bin/sway")) (user "paul"))))
    (service greetd-service-type
             (greetd-configuration
               (greeter-supplementary-groups (list "video" "input" "seat"))
               (terminals
                (list (greetd-terminal-configuration
                        (terminal-vt "1")
                        (terminal-switch #t)
                        (extra-shepherd-requirement '(seatd))
                        (default-session-command
                          (greetd-wlgreet-sway-session)))
                      (greetd-terminal-configuration (terminal-vt "2"))
                      (greetd-terminal-configuration (terminal-vt "3"))
                      (greetd-terminal-configuration (terminal-vt "4"))
                      (greetd-terminal-configuration (terminal-vt "5"))
                      (greetd-terminal-configuration (terminal-vt "6"))))))

    ;; %desktop-services ships GDM (X11) and the mingetty gettys; drop both
    ;; since greetd now owns the VTs.
    (modify-services %desktop-services
      (delete gdm-service-type)
      (delete mingetty-service-type))))

  (name-service-switch %mdns-host-lookup-nss))
