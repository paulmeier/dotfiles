;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-ref)
(package! org-super-agenda)
(package! websocket)
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

(unpin! org-roam)
;; Microsoft Exchange support
;;(package! excorporate)

(package! ob-mermaid)
(package! ejc-sql)
