;;; early-init.el --- Emacs Writing Studio profile hooks -*- lexical-binding: t; -*-

;; Upstream EWS (init.el, ews.el) is fetched untouched by bootstrap.sh.
;; Everything personal hangs off this file instead of patching upstream:
;; settings EWS reads while init.el runs are set here, and user.el loads
;; once init.el is done so its settings win over upstream's :custom blocks.

;; Share data paths with Doom (my/notes-directory, my/org-directory, ...).
(load (expand-file-name "doom/local.el"
                        (or (getenv "XDG_CONFIG_HOME") "~/.config"))
      t t)

;; Read by init.el's flyspell block, so it has to be set before init.el.
(setq ews-hunspell-dictionaries "en_US")
(when (bound-and-true-p my/references-library)
  (setq ews-bibtex-directory my/references-library))

;; macOS Emacs has no D-Bus, and init.el unconditionally requires
;; emms-mpris.  Stub it out so EMMS loads without it.
(unless (featurep 'dbusbind)
  (provide 'emms-mpris)
  (defun emms-mpris-enable () nil))

;; ProtonVPN exits get reset by elpa.gnu.org, so install GNU and NonGNU
;; packages from a GitHub mirror.  (init.el adds MELPA itself.)
(setq package-archives
      '(("gnu"    . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")
        ("nongnu" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/nongnu/")))

(add-hook 'after-init-hook
          (lambda ()
            (load (expand-file-name "user.el" user-emacs-directory) t t)))

;;; early-init.el ends here
