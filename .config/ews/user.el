;;; user.el --- Personal additions to Emacs Writing Studio -*- lexical-binding: t; -*-

;; Loaded from early-init.el's after-init-hook, after upstream init.el.

;;; Paths (shared with Doom via ~/.config/doom/local.el)

(load (expand-file-name "doom/modules/denote-dirs"
                        (or (getenv "XDG_CONFIG_HOME") "~/.config"))
      nil t)
(when (bound-and-true-p my/notes-directory)
  (setq denote-directory (my/denote-directory-value)
        denote-journal-directory (expand-file-name "journal" my/notes-directory)))
(when (bound-and-true-p my/org-directory)
  (setq org-directory my/org-directory))
(when (bound-and-true-p my/elfeed-org-file)
  (setq rmh-elfeed-org-files (list my/elfeed-org-file)))

;;; macOS: EWS's dired switches need GNU ls

(when (executable-find "gls")
  (setq insert-directory-program "gls"))

;;; Look and feel

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(when (find-font (font-spec :family "SN Pro"))
  (set-face-attribute 'variable-pitch nil :family "SN Pro"))

;;; Extra packages

(use-package key-quiz
  :bind
  (("C-c w k" . key-quiz)))

;;; Jump to my profile files

(defun my/ews-open-user-el ()
  "Open this file."
  (interactive)
  (find-file (expand-file-name "user.el" user-emacs-directory)))

(defun my/ews-open-early-init ()
  "Open the profile's early-init.el."
  (interactive)
  (find-file (expand-file-name "early-init.el" user-emacs-directory)))

(keymap-global-set "C-c w u" #'my/ews-open-user-el)
(keymap-global-set "C-c w U" #'my/ews-open-early-init)

;;; user.el ends here
