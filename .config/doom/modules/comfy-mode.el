;;; comfy-mode.el --- Pretty “comfy” reading mode  -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode comfy-mode
  "Toggle a comfy, visually focused reading/writing mode.

Enables various visual tweaks like variable-pitch, no line numbers, etc."
  :init-value nil
  :lighter " 🌿"
  :keymap nil

  (if comfy-mode
      ;; ON: turn on all your visualization settings
      (progn
        ;; Indent & prettify Org
        (when (derived-mode-p 'org-mode)
          (org-indent-mode 1))

        (variable-pitch-mode 1)
        (auto-fill-mode 0)
        (visual-line-mode 1)

        ;; valign
        (when (fboundp 'valign-mode)
          (valign-mode 1))

        ;; visual-fill-column
        (when (fboundp 'visual-fill-column-mode)
          ;; Center text
          (setq-local visual-fill-column-center-text t)
          (visual-fill-column-mode 1))

        ;; Hide line numbers
        (display-line-numbers-mode 0))

    ;; OFF: try to revert what we changed
    (progn
      (when (derived-mode-p 'org-mode)
        (org-indent-mode -1))

      (variable-pitch-mode -1)
      (visual-line-mode -1)

      (when (fboundp 'valign-mode)
        (valign-mode -1))

      (when (fboundp 'visual-fill-column-mode)
        (visual-fill-column-mode -1)
        ;; Remove buffer-local override
        (kill-local-variable 'visual-fill-column-center-text))

      ;; We won’t try to restore line numbers or auto-fill because we
      ;; don’t know prior buffer state — could add state tracking later.
      )))

(provide 'comfy-mode)
;;; comfy-mode.el ends here
;;;
