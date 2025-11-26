;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; AI
;;(package! gptel)

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
;;(package! good-scroll) ; already included in doom

;; HTTP
(package! verb)
(package! url-http-oauth)
(package! websocket)

;; Notes Packages
(package! denote)
(package! denote-org)
(package! denote-menu)
(package! consult-denote)
(package! consult-notes)

;; Programming Stuff
(package! load-env-vars)

;; Drag Lines
(package! drag-stuff)

;; AI
;; (package! copilot-chat
;;   :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el")))
;; (package! copilot)
(package! gptel :recipe (:nonrecursive t))

;; Kubernetes
;;(package! kubernetes)
;;(package! kubernetes-evil)
;;(package! kubedoc)
;;(package! k8s-mode)

;; Loaded packages for work
(when (string= (system-name) "S427544")
  (package! lab) ;; Gitlab
  (package! impostman) ;; Postman importing
  )

;; Obsidian
(package! obsidian)

;; Jupyter
;; (package! jupyter)
