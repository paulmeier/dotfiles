;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Load per-machine settings early so `my/local-features' (defined in
;; local.el) can gate optional packages below. local.el is gitignored;
;; if it's missing we just continue without any opt-in features.
(let ((local-file (expand-file-name "local.el" (or (bound-and-true-p doom-user-dir)
                                                   user-emacs-directory))))
  (when (file-exists-p local-file)
    (load local-file nil 'nomessage)))

;; Org
(package! ob-mermaid)

;; Emacs
(package! valign)
(package! centered-cursor-mode)
(package! org-timeblock)

;; HTTP
(package! verb)
(package! url-http-oauth)
(package! websocket)

;; Notes
(package! denote)
(package! denote-org)
(package! denote-menu)
(package! denote-journal)
(package! consult-denote)
(package! consult-notes)
(package! citar-denote)

;; Writing
(package! olivetti)

;; Programming
(package! load-env-vars)

;; Editing
(package! drag-stuff)

;; Templating (jinja2-mode for .j2 / .j2.yml templates; see config.org "Jinja2 templates")
(package! jinja2-mode)

;; AI
(package! copilot-chat
  :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el")))
(package! copilot)
(package! gptel :recipe (:nonrecursive t))

;; Misc
(package! obsidian)

;;; Opt-in features --------------------------------------------------------
;; Add the corresponding symbols to `my/local-features' in local.el to enable.

(when (member 'gitlab (bound-and-true-p my/local-features))
  (package! lab))

(when (member 'postman (bound-and-true-p my/local-features))
  (package! impostman))
