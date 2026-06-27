;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; AI
;;(package! gptel)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! gh-copilot-chat
  :recipe (:host github :repo "chep/gh-copilot-chat.el" :files ("*.el")))

;; Org
;;(package! org-super-agenda)
(package! ob-mermaid)
;;(package! org-view-mode)

;; Org Roam
;;(package! org-roam-ui
;;   :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
;; (package! citar-org-roam)

;; Emacs
(package! valign)
(package! centered-cursor-mode)
(package! org-timeblock)

;; HTTP
(package! verb)
(package! url-http-oauth)
(package! websocket)

;; Notes Packages
(package! denote)
(package! denote-journal)
(package! denote-org)
(package! denote-menu)
(package! consult-denote)
(package! citar-denote)
(package! consult-notes)

;; Writing Packages
(package! olivetti)

;; Programming Stuff
(package! load-env-vars)

;; Drag Lines
(package! drag-stuff)

;; AI
;;(package! gptel :recipe (:nonrecursive t))

;; Kubernetes
;;(package! kubernetes)
;;(package! kubernetes-evil)
;;(package! kubedoc)
;;(package! k8s-mode)

(package! lab) ;; Gitlab
(package! impostman) ;; Postman importing

;; Jupyter
;; (package! jupyter)
(package! mcp-server
  :recipe (:type git :host github :repo "rhblind/emacs-mcp-server"
           :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh")))
