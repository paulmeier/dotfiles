;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Projectile project search paths
(setq projectile-project-search-path '("~/zProjects"))

;; Exchange information
;; (setq excorporate-configuration '("paulmeier@northwesternmutual.com" . "https://outlook.office365.com/EWS/Exchange.asmx"))

;;Load Go-specific language syntax
;;For gocode use https://github.com/mdempsky/gocode

;;Other Key bindings
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Keybind
;; Export to html
(map! :leader
      (:prefix ("a" . "applications")
      :desc "Export Org to HTML"
      "e" #'org-html-export-as-html))

;; Keybinding for finding Projects in Roam
(map! :leader
      :desc "Find Roam Project"
    "n r p" #'my/org-roam-find-project)

;; Keybinding for quickly capturing notes to Roam Inbox
(map! :leader
    :desc "Capture Roam Inbox"
    "n r b" #'my/org-roam-capture-inbox)

;; Keybinding for capturing a Project Task
(map! :leader
    :desc "Capture Roam Project Task"
    "n r t" #'my/org-roam-capture-task)

;; Toggle Roam UI
(map! :leader
      :desc "Org Roam UI"
      "n r u" #'org-roam-ui-mode)

;;Compilation autoscroll
(setq compilation-scroll-output t)

(setq dired-dwim-target t)

(setq org-roam-dailies-directory "journal/")
(use-package! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/pBrain")
  (org-roam-completion-everywhere t)
    (org-roam-capture-templates
        '(("d" "default" plain "%?"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date:%U\n")
              :unarrowed t)
             ("l" "programming language" plain
                 "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                 :unnarrowed t)
             ("b" "book notes" plain
                 (file "~/pBrain/Templates/BookNoteTemplate.org")
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                 :unnarrowed t)
             ("p" "project" plain
                 (file "~/pBrain/Templates/ProjectTemplate.org")
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
                 :unnarrowed t)
             )
        )
    :config
    (org-roam-setup))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; (after! org
;;     (map! :map org-mode-map
;;         :n 'M-j' #'org-metadown
;;         :n 'M-k' #'org-metaup))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
      :templates
      '(("p" "project" plain
                    (file "~/pBrain/Templates/ProjectTemplate.org")
                    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n")
                    :unnarrowed t))))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "\n** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

;; Here is just an example that is a WIP
;; a way to tangle all .org files contained in dotfiles
;; at the moment it only does the single one
;; (defun efs/org-babel-tangle-config ()
;;   (when (seq-contains-p (file-expand-wildcards "~/.dotfiles/*.org")
;;                         '(buffer-file-name))
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/Zsh.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
