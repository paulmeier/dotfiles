;;; channels.scm — channels for the G14 Guix system.
;;;
;;; As committed this tracks the latest nonguix + guix, with signature
;;; introductions (required for authentication). It is valid as-is.
;;;
;;; To LOCK the system to an exact, reproducible state, capture your current
;;; channels from a known-good machine and overwrite this file:
;;;
;;;     guix describe -f channels > channels.scm
;;;
;;; (or uncomment the (commit "...") lines below and paste the hashes that
;;;  `guix describe` prints). Do NOT invent commit hashes.

(list (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       ;; (commit "PASTE-FROM-guix-describe")     ; uncomment to pin
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       ;; (commit "PASTE-FROM-guix-describe")     ; uncomment to pin
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
