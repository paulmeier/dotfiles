;;; lit-mode.el --- Pretty “lit” reading mode  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A comfy, focused writing/reading mode:
;; - variable-pitch
;; - visual-line + centered text
;; - no line numbers
;; - typewriter-style scrolling (cursor stays near middle)
;; - bump animation
;; - dim all non-current lines; highlight current line
;; - optional Evil j/k = visual-line motions
;; - hide the modeline
;; - double-spacing: RET inserts two newlines
;;

;;; Code:

(require 'color)

;; Group -----------------------------------------------------------------

(defgroup lit nil
  "Customizations for `lit-mode`."
  :group 'convenience)

;; Faces -----------------------------------------------------------------

(defface lit-dim-face
  '((t :inherit shadow))
  "Face used to dim non-current lines in `lit-mode`."
  :group 'lit)

(defface lit-focus-face
  '((t :inherit nil))
  "Face used for the current line in `lit-mode`."
  :group 'lit)

;; Customization options -------------------------------------------------

(defcustom lit-typewriter-animation t
  "If non-nil, animate recentering with a small bump scroll."
  :type 'boolean
  :group 'lit)

(defcustom lit-typewriter-animation-max-steps 5
  "Maximum number of animated scroll steps."
  :type 'integer
  :group 'lit)

(defcustom lit-typewriter-animation-delay 0.01
  "Delay between scroll steps in seconds."
  :type 'number
  :group 'lit)

(defcustom lit-typewriter-deadzone-lines 3
  "Tolerance in lines around center where no recentering happens."
  :type 'integer
  :group 'lit)

(defcustom lit-remap-evil-jk t
  "If non-nil, map j/k to visual-line motions."
  :type 'boolean
  :group 'lit)

;; ---------------- Double-spacing implementation ----------------

(defun lit-insert-double-newline ()
  "Insert two newlines instead of one."
  (interactive)
  (newline)
  (newline))

(defvar lit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lit-insert-double-newline)
    (define-key map (kbd "C-m") #'lit-insert-double-newline)
    map)
  "Keymap for `lit-mode`.")

;; Internal vars ---------------------------------------------------------

(defvar-local lit--default-face-cookie nil
  "Face remap cookie for dimming.")

(defvar-local lit--saved-mode-line nil
  "Saved modeline for restoring when disabling lit-mode.")

;; Typewriter logic ------------------------------------------------------

(defun lit--typewriter--compute-delta (win)
  "Return DELTA screen lines by which point differs from center."
  (let* ((start (window-start win))
         (cur (point))
         (line (count-screen-lines start cur t))
         (height (max 1 (window-body-height win)))
         (target (/ height 2))
         (delta (- line target)))
    (if (<= (abs delta) lit-typewriter-deadzone-lines)
        0
      delta)))

(defun lit--typewriter-recenter-animated (win delta)
  "Animate scroll in WIN by DELTA."
  (let* ((step (if (> delta 0) 1 -1))
         (steps (min (abs delta) lit-typewriter-animation-max-steps)))
    (dotimes (_ steps)
      (let ((new-start
             (save-excursion
               (goto-char (window-start win))
               (forward-line step)
               (point))))
        (set-window-start win new-start 'noforce))
      (redisplay)
      (when (> lit-typewriter-animation-delay 0)
        (sit-for lit-typewriter-animation-delay)))))

(defun lit--typewriter-recenter ()
  "Recenter point with animation + deadzone."
  (when (and lit-mode
             (not (minibufferp (current-buffer))))
    (let ((win (selected-window)))
      (when (eq (window-buffer win) (current-buffer))
        (let ((delta (lit--typewriter--compute-delta win)))
          (when (/= delta 0)
            (if lit-typewriter-animation
                (lit--typewriter-recenter-animated win delta)
              (recenter))))))))

;; Focus visuals ---------------------------------------------------------

(defun lit--enable-focus-visuals ()
  "Enable dimming and line focus."
  (setq lit--default-face-cookie
        (face-remap-add-relative 'default 'lit-dim-face))
  (setq-local hl-line-face 'lit-focus-face)
  (hl-line-mode 1))

(defun lit--disable-focus-visuals ()
  "Disable dimming and line focus."
  (when lit--default-face-cookie
    (face-remap-remove-relative lit--default-face-cookie))
  (setq lit--default-face-cookie nil)
  (hl-line-mode -1)
  (kill-local-variable 'hl-line-face))

;; Evil integration ------------------------------------------------------

(defun lit--maybe-remap-evil-jk ()
  "Buffer-local j/k → visual-line motions."
  (when (and lit-remap-evil-jk
             (bound-and-true-p evil-local-mode))
    (evil-local-set-key 'normal "j" #'evil-next-visual-line)
    (evil-local-set-key 'normal "k" #'evil-previous-visual-line)
    (evil-local-set-key 'motion "j" #'evil-next-visual-line)
    (evil-local-set-key 'motion "k" #'evil-previous-visual-line)
    ;; ALSO fix Enter in normal-state to insert 2 lines
    (evil-local-set-key 'insert (kbd "RET") #'lit-insert-double-newline)))

;; Modeline hiding -------------------------------------------------------

(defun lit--hide-mode-line ()
  "Hide the modeline for this buffer."
  (setq lit--saved-mode-line mode-line-format)
  (setq-local mode-line-format nil)
  (force-mode-line-update))

(defun lit--restore-mode-line ()
  "Restore the original modeline."
  (when lit--saved-mode-line
    (setq-local mode-line-format lit--saved-mode-line)
    (setq lit--saved-mode-line nil))
  (force-mode-line-update))

;;;###autoload
(define-minor-mode lit-mode
  "Focused writing/reading mode."
  :init-value nil
  :lighter " ✨"
  :keymap lit-mode-map

  (if lit-mode
      ;; ---------------- ENABLE ----------------
      (progn
        ;; hide modeline
        (lit--hide-mode-line)

        (when (derived-mode-p 'org-mode)
          (org-indent-mode 1))

        (variable-pitch-mode 1)
        (auto-fill-mode 0)
        (visual-line-mode 1)

        (when (fboundp 'valign-mode)
          (valign-mode 1))

        (when (fboundp 'visual-fill-column-mode)
          (setq-local visual-fill-column-center-text t)
          (visual-fill-column-mode 1))

        (display-line-numbers-mode 0)

        ;; typewriter
        (setq-local recenter-positions '(middle))
        (add-hook 'post-command-hook #'lit--typewriter-recenter nil t)

        ;; focus visuals
        (lit--enable-focus-visuals)
        (let ((fg (face-attribute 'default :foreground nil 'default))
              (bg (face-attribute 'default :background nil 'default)))
          (set-face-attribute 'lit-focus-face nil
                              :foreground fg
                              :background bg))

        ;; evil motions & enter fix
        (lit--maybe-remap-evil-jk))

    ;; ---------------- DISABLE ----------------
    (progn
      (lit--restore-mode-line)

      (when (derived-mode-p 'org-mode)
        (org-indent-mode -1))

      (variable-pitch-mode -1)
      (visual-line-mode -1)

      (when (fboundp 'valign-mode)
        (valign-mode -1))

      (when (fboundp 'visual-fill-column-mode)
        (visual-fill-column-mode -1)
        (kill-local-variable 'visual-fill-column-center-text))

      (remove-hook 'post-command-hook #'lit--typewriter-recenter t)
      (kill-local-variable 'recenter-positions)

      (lit--disable-focus-visuals))))

(provide 'lit-mode)

;;; lit-mode.el ends here
;;;
