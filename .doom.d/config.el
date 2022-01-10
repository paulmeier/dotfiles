(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq compilation-scroll-output t)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

(map! :leader
      (:prefix ("a" . "applications")
      :desc "Export Org to HTML"
      "e" #'org-html-export-as-html))

(map! :leader
      :desc "Find Roam Project"
    "n r p" #'my/org-roam-find-project)

(map! :leader
    :desc "Capture Roam Inbox"
    "n r b" #'my/org-roam-capture-inbox)

(map! :leader
    :desc "Capture Roam Project Task"
    "n r t" #'my/org-roam-capture-task)

(map! :leader
      :desc "Org Roam UI"
      "n r u" #'org-roam-ui-mode)

(setq projectile-project-search-path '("~/zProjects"))

(setq dired-dwim-target t)

(setq org-directory "~/Amazon Drive/pOrg")
(setq org-use-property-inheritance t)

(setq org-roam-dailies-directory "journal/")

(use-package! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Amazon Drive/pBrain")
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
                 (file "~/Amazon Drive/pBrain/Templates/BookNoteTemplate.org")
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                 :unnarrowed t)
             ("p" "project" plain
                 (file "~/Amazon Drive/pBrain/Templates/ProjectTemplate.org")
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
                 :unnarrowed t)
             )
        )
    :config
    (org-roam-setup))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defun my/org-roam-filter-by-tag (tag-name)
(lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
(mapcar #'org-roam-node-file
        (seq-filter
        (my/org-roam-filter-by-tag tag-name)
        (org-roam-node-list))))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/Zsh.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/Doom.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-capture-task ()
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "\n** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-find-project ()
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; TEST COMMENT
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
      :templates
      '(("p" "project" plain
                    (file "~/Amazon Drive/pBrain/Templates/ProjectTemplate.org")
                    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n")
                    :unnarrowed t))))
