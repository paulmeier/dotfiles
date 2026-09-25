;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

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

;; Emacs Writing Studio (modules/ews.el); the rest of its packages are
;; already pulled in by Doom modules or the sections above.
(package! spacious-padding)
(package! balanced-windows)
(package! org-fragtog)
(package! nov)
(package! biblio)
(package! org-web-tools)
(package! openwith)
(package! denote-sequence)
(package! denote-explore)
(package! titlecase)
(package! lorem-ipsum)
(package! fountain-mode)
(package! ox-epub)

;; Key Quiz
(package! key-quiz)

;; Programming
(package! load-env-vars)

;; Editing
(package! drag-stuff)

;; Templating
(package! jinja2-mode)

;; AI
(package! copilot-chat
  :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el")))
(package! copilot)
(package! gptel :recipe (:nonrecursive t))
(package! mcp-server
  :recipe (:host github :repo "rhblind/emacs-mcp-server"
           :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh")))

;; Misc
(package! obsidian)

;; Secrets. Uses the local checkout when present, else GitHub.
(package! proton-pass
  :recipe (if (file-directory-p "~/Projects/proton-pass.el")
              '(:local-repo "~/Projects/proton-pass.el" :files ("proton-pass.el"))
            '(:host github :repo "paulmeier/proton-pass.el" :files ("proton-pass.el"))))
