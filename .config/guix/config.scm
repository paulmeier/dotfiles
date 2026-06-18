;;; config.scm — GNU Guix System for ASUS ROG Zephyrus G14 (GA401QM-211)
;;;
;;; Hardware:
;;;   CPU   AMD Ryzen 9 5900HS (Zen 3 / Cezanne)
;;;   iGPU  AMD Radeon (Cezanne) — drives the internal panel
;;;   dGPU  NVIDIA GeForce RTX 3060 (Ampere) — Optimus, NO hardware MUX
;;;   WiFi  MediaTek MT7921 (Wi-Fi 6) + Bluetooth 5.2
;;;
;;; Design:
;;;   - Sway runs on the AMD iGPU (rock solid; the panel is wired to it anyway).
;;;   - The proprietary NVIDIA module is loaded for CUDA and per-app render
;;;     offload (games via Proton/Xwayland). You do NOT run the compositor on
;;;     the dGPU, which sidesteps all the wlroots-on-NVIDIA pain.
;;;   - Full-disk encryption: unencrypted EFI System Partition + LUKS root.
;;;
;;; Requires the nonguix channel (linux, linux-firmware, nvidia packages).
;;; Disk is matched by label (set in cfdisk + mkfs), so this file is
;;; machine-agnostic and safe to keep in source control — just confirm
;;; the username below before `guix system init`.

(use-modules (gnu)
             (gnu system nss)
             (nongnu packages linux)        ; linux, linux-firmware
             (nongnu system linux-initrd)   ; microcode-initrd
             (nongnu packages nvidia)        ; nvda, nvidia-module
             (nongnu services nvidia))       ; nvidia-service-type

(use-service-modules base desktop networking ssh)

(define %keeb (keyboard-layout "us"))

;; LUKS-encrypted root, matched by GPT partition label so this file stays
;; machine-agnostic (no per-install UUID edits — good for source control).
;; Set the label in cfdisk: the partition "Name" field = cryptroot.
(define cryptroot
  (mapped-device
    (source "/dev/disk/by-partlabel/cryptroot")
    (target "cryptroot")
    (type luks-device-mapping)))

(operating-system
  (host-name "zephyrus")
  (timezone "America/Chicago")
  (locale "en_US.utf8")
  (keyboard-layout %keeb)

  ;; ---- Kernel / firmware (nonfree) -------------------------------------
  (kernel linux)
  (initrd microcode-initrd)                  ; early AMD microcode load
  (firmware (list linux-firmware))           ; MT7921 Wi-Fi + amdgpu blobs
  (kernel-loadable-modules (list nvidia-module))
  (kernel-arguments
   (append '("modprobe.blacklist=nouveau"    ; proprietary module owns the dGPU
             "nvidia_drm.modeset=1"          ; KMS — needed for Wayland offload
             "amd_pstate=active")            ; Zen 3 EPP power management
           %default-kernel-arguments))

  ;; ---- Disk / boot ------------------------------------------------------
  (bootloader
   (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets (list "/boot/efi"))
     (keyboard-layout %keeb)))

  (mapped-devices (list cryptroot))

  (file-systems
   (cons* (file-system
            (mount-point "/")
            (device "/dev/mapper/cryptroot")
            (type "ext4")
            (dependencies (list cryptroot)))
          (file-system
            (mount-point "/boot/efi")
            ;; ESP by label:  mkfs.fat -F32 -n EFI /dev/nvme0n1p1
            (device (file-system-label "EFI"))
            (type "vfat"))
          %base-file-systems))

  ;; Optional swap on the encrypted volume (size >= RAM if you hibernate).
  ;; Create it first, then uncomment:
  ;;   fallocate -l 20G /swapfile && chmod 600 /swapfile && mkswap /swapfile
  ;; (swap-devices
  ;;  (list (swap-space
  ;;          (target "/swapfile")
  ;;          ;; depend on the root fs so it mounts before swapon
  ;;          (dependencies (filter (file-system-mount-point-predicate "/")
  ;;                                (operating-system-file-systems this-operating-system))))))

  ;; ---- Users ------------------------------------------------------------
  (users
   (cons (user-account
           (name "paul")                     ; PLACEHOLDER: your login
           (comment "Paul")
           (group "users")
           (home-directory "/home/paul")
           (supplementary-groups
            '("wheel"        ; sudo
              "netdev"       ; NetworkManager
              "audio" "video" "input"
              "seat"         ; seatd (Sway)
              "kvm" "tty" "lp")))
         %base-user-accounts))

  ;; ---- Packages ---------------------------------------------------------
  (packages
   (append
    (map specification->package
         '(;; Sway + Wayland essentials
           "sway" "swaylock" "swayidle" "swaybg"
           "waybar" "wofi" "foot" "mako"
           "grim" "slurp" "wl-clipboard"
           "brightnessctl" "playerctl"
           ;; audio (start the daemons from Guix Home — see notes)
           "pipewire" "wireplumber" "pavucontrol"
           ;; screen-share / portals on wlroots
           "xdg-desktop-portal" "xdg-desktop-portal-wlr"
           ;; net / misc
           "network-manager-applet" "bluez"
           "git" "curl" "gnu-make"
           "nss-certs"
           "font-dejavu" "font-google-noto"))
    (list nvda)                              ; NVIDIA user-space GL / CUDA libs
    %base-packages))

  ;; ---- Services ---------------------------------------------------------
  (services
   (cons*
    ;; Proprietary NVIDIA: loads the module, installs udev rules and the
    ;; libglvnd vendor dispatch needed for render offload.
    (service nvidia-service-type)

    ;; seatd — seat management for Sway/wlroots.
    (service seatd-service-type)

    ;; Bluetooth (MT7921 combo).
    (service bluetooth-service-type
             (bluetooth-configuration (auto-enable? #t)))

    ;; SSH for the homelab.
    (service openssh-service-type)

    ;; greetd -> wlgreet -> Sway (Wayland-native login).
    ;; vt1 launches Sway; vt2–6 are plain login shells (replacing the gettys).
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
