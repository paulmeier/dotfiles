;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Helpers

(defun my-message (format-string &rest args)
  "Display a personal message prefixed with @@@ in *Messages* buffer."
  (apply #'message (concat "@@@ " format-string) args))

(defun my/add-to-load-path (&rest paths)
  "Add each PATH in PATHS to `load-path` and log it."
  (dolist (path paths)
    (add-to-list 'load-path path)
    (my-message "Added %s to load-path" path)))

;;; Paths

(defvar my/projects-directory "~/Projects")
(defvar my/snippets-directory "~/Projects/snippets")
(defvar my/nomadnet-directory "~/Projects/nomadnet.el")

;; Paths to my data are set in $DOOMDIR/local.el, which is gitignored.
(defvar my/notes-directory "~/Notes")
(defvar my/org-directory "~/Org")
(defvar my/references-bib (expand-file-name "references/references.bib" my/notes-directory))
(defvar my/references-library (expand-file-name "references" my/notes-directory))
(defvar my/elfeed-org-file (expand-file-name "elfeed.org" my/org-directory))

(load (expand-file-name "local.el" doom-user-dir) t t)

;;; Identity
;; Email and GPG key id come from Proton Pass: `pp-sync' exports them (see
;; ~/.config/zsh/secrets.env), and GUI Emacs picks them up via `doom env'.
(setq user-full-name "Paul Meier"
      user-mail-address (or (getenv "USER_MAIL_ADDRESS") user-mail-address))

;;; Email
;; notmuch over Proton Mail Bridge (IMAP 127.0.0.1:1143, SMTP :1025). mbsync,
;; msmtp and the ~/.local/bin/mailsync script are configured outside this repo.
(when (executable-find "notmuch")
  (after! notmuch
    ;; Doom defaults to lieer (Gmail only); use the same script as the
    ;; launchd auto-sync.
    (setq +notmuch-sync-backend (expand-file-name "~/.local/bin/mailsync")
          +notmuch-home-function (lambda () (notmuch-search "tag:inbox")))
    (setq sendmail-program (executable-find "msmtp")
          message-sendmail-f-is-evil t
          message-sendmail-extra-arguments '("--read-envelope-from"))
    (setq notmuch-saved-searches
          '((:name "inbox"   :query "tag:inbox not tag:trash" :key "i")
            (:name "unread"  :query "tag:unread"              :key "u")
            (:name "flagged" :query "tag:flagged"             :key "f")
            (:name "sent"    :query "tag:sent"                :key "s")
            (:name "drafts"  :query "tag:draft"               :key "d")
            (:name "all"     :query "*"                       :key "a"))))
  (after! auth-source (auth-source-pass-enable)))

;;; Custom modules
(my/add-to-load-path (expand-file-name "modules" doom-user-dir))
(require 'isbn-to-bibtex)
(require 'my-book-cover)
(require 'book-note)
(require 'lit-mode)
(require 'comfy-mode)

;;; UI

(setq fancy-splash-image (concat doom-user-dir "alfie.png"))

;; "SN Pro" is installed manually; "Noto Sans Symbols 2" fixes unicode boxes
;; for org bullets.
(setq doom-font (font-spec :family "Fira Code" :size 15)
      doom-variable-pitch-font (font-spec :family "SN Pro" :size 15 :weight 'medium)
      doom-big-font (font-spec :family "Fira Code" :size 24))

;; Start maximized. The initial frame on the macOS port ignores
;; `default-frame-alist', so maximize it again once the window system is up.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'window-setup-hook
          (defun my/setup-initial-gui-frame-h ()
            (when (display-graphic-p)
              (menu-bar-mode 1)
              (tool-bar-mode -1)
              (set-frame-parameter nil 'fullscreen 'maximized))))

(display-battery-mode 1)

(setq warning-minimum-level :emergency)

;;; Editor

(use-package! drag-stuff
  :defer t
  :init
  (map! "<M-up>"    #'drag-stuff-up
        "<M-down>"  #'drag-stuff-down
        "<M-left>"  #'drag-stuff-left
        "<M-right>" #'drag-stuff-right))

(setq-hook! 'javascript-mode-hook     +format-with 'prettier)
(setq-hook! 'typescript-mode-hook     +format-with 'prettier)
(setq-hook! 'typescript-tsx-mode-hook +format-with 'prettier)

(defun my/check-formatter-for-mode (mode)
  "Check the formatter used for a specific MODE."
  (interactive
   (list (intern (completing-read "Mode: " obarray
                                  (lambda (m)
                                    (and (fboundp m)
                                         (string-suffix-p "-mode" (symbol-name m))))
                                  t))))
  (with-temp-buffer
    (funcall mode)
    (message "Formatter for %s: %s" mode +format-with)))

;; Only the .j2 variants; plain .yml stays with Doom's yaml mode.
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\.ya?ml\\'" . jinja2-mode))
(add-hook 'jinja2-mode-hook (lambda () (setq jinja2-enable-indent t)))

;;; Encryption

(after! epg
  (setq epg-gpg-program "/usr/local/bin/gpg"
        epg-pinentry-mode 'ask))

(when-let* ((key (getenv "GPG_KEY_ID")))
  (setq epa-file-encrypt-to (list key)
        epa-file-select-keys 'silent))

(epa-file-enable)

;;; Secrets (Proton Pass)
;; https://github.com/paulmeier/proton-pass.el. Nothing is fetched at startup.
;; Only map items whose titles are unique; a missing item errors instead of
;; falling back to ~/.authinfo.gpg.
(use-package! proton-pass
  :demand t
  :config
  (setq proton-pass-vault "Personal"
        proton-pass-auth-source-alist
        '(;; (HOST USER URI) -- USER nil matches any user.
          ;; ("api.anthropic.com" "apikey" "pass://Personal/Anthropic API/password")
          ))
  (proton-pass-auth-source-enable)
  (proton-pass-use-ssh-agent))

(map! :leader
      (:prefix ("k" . "proton pass")
       :desc "Browse vault"          "b" #'proton-pass
       :desc "View item"             "v" #'proton-pass-view
       :desc "Copy password"         "p" #'proton-pass-copy-password
       :desc "Copy username"         "u" #'proton-pass-copy-username
       :desc "Copy field"            "f" #'proton-pass-copy-field
       :desc "Copy TOTP code"        "t" #'proton-pass-totp
       :desc "Open URL"              "o" #'proton-pass-url
       :desc "Insert item"           "i" #'proton-pass-insert
       :desc "Create with generated" "c" #'proton-pass-generate
       :desc "Edit field"            "e" #'proton-pass-edit
       :desc "Rename item"           "r" #'proton-pass-rename
       :desc "Trash item"            "d" #'proton-pass-remove
       :desc "Switch vault"          "s" #'proton-pass-switch-vault
       :desc "Write password here"   "w" #'proton-pass-insert-generated-password
       :desc "Clear cache"           "x" #'proton-pass-clear-cache
       :desc "Account info"          "a" #'proton-pass-info))

;; Same letters as the leader menu. Bound per evil state rather than as an
;; overriding map, which would inherit special-mode's SPC and shadow the leader.
(map! :after proton-pass
      :map (proton-pass-mode-map proton-pass-view-mode-map)
      :n "p" #'proton-pass-copy-password
      :n "u" #'proton-pass-copy-username
      :n "f" #'proton-pass-copy-field
      :n "t" #'proton-pass-totp
      :n "o" #'proton-pass-url
      :n "i" #'proton-pass-insert
      :n "c" #'proton-pass-generate
      :n "e" #'proton-pass-edit
      :n "r" #'proton-pass-rename
      :n "d" #'proton-pass-remove
      :n "s" #'proton-pass-switch-vault
      :n "q" #'quit-window
      :map proton-pass-mode-map
      :n "RET" #'proton-pass-view
      :n "v"   #'proton-pass-view
      :n "gr"  #'proton-pass-refresh
      :map proton-pass-view-mode-map
      :n "gr"  #'proton-pass-view-refresh)

;;; Tools

(setq projectile-project-search-path (list my/projects-directory))

(setq dired-dwim-target t)

(use-package! doom-snippets
  :when (file-directory-p my/snippets-directory)
  :load-path (lambda () (list my/snippets-directory))
  :after yasnippet)

;;; Org

(defun my/org-mode-setup ()
  (setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")
  (org-display-inline-images)
  (setq evil-auto-indent nil)
  (setq org-attach-id-dir ".attach")
  (setq org-attach-use-inheritance t)
  (setq org-attach-store-link-p t)
  (setq org-attach-dir-relative t)
  (setq org-image-actual-width nil)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-use-property-inheritance t))

(after! org
  (add-hook 'org-mode-hook #'my/org-mode-setup)
  (setq org-directory my/org-directory
        org-agenda-files (list my/org-directory))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((emacs-lisp . t)
             (python     . t)
             (shell      . t)
             (js         . t)
             (mermaid    . t)
             (scheme     . t)
             (verb       . t)
             (ein        . t)
             (typescript . t)))))

(defun org-set-line-checkbox (arg)
  (interactive "P")
  (let ((n (or arg 1)))
    (when (region-active-p)
      (setq n (count-lines (region-beginning) (region-end)))
      (goto-char (region-beginning)))
    (dotimes (_ n)
      (beginning-of-line)
      (insert "- [ ] ")
      (forward-line))
    (beginning-of-line)))

(defun my/org-title-slug ()
  "Return a filesystem-safe slug based on #+title or buffer name."
  (let* ((title (or (org-get-title)
                    (file-name-base (or (buffer-file-name) "diagram"))))
         (slug  (downcase
                 (replace-regexp-in-string "[^[:alnum:]]+" "-" title))))
    slug))

(use-package! org-timeblock
  :config
  (setq org-timeblock-inbox-file "timeblock.org"))

(map! :leader
      :desc "Expand Org source block"
      "y b" #'org-babel-expand-src-block)

;;; Notes

(use-package! denote
  :config
  (setq denote-directory          my/notes-directory
        denote-directories        (list my/notes-directory)
        denote-dired-directories  (list my/notes-directory)
        denote-prompts '(subdirectory title keywords template)))

(map! :leader
      (:prefix ("e" . "denote")
       :desc "New Note"        "n" #'denote
       :desc "Insert Link"     "i" #'denote-insert-link
       :desc "Journal"         "j" #'denote-journal-new-or-existing-entry
       :desc "New Book Note"   "t" #'my/denote-book-note
       :desc "Find in Notes"   "f" #'consult-notes-search-in-all-notes
       :desc "Search Notes"    "s" #'consult-notes))

(map! :leader
      :desc "Toggle lit mode"   "t i" #'lit-mode
      :desc "Toggle comfy mode" "t o" #'comfy-mode)

;; Template bodies are files inside the notes directory.
(defun my/denote-template-from-file (filename)
  "Return contents of FILENAME (relative to `denote-directory`)."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name filename denote-directory))
    (buffer-string)))

(defun my/denote-template-non-fiction-book ()
  (my/denote-template-from-file "literature/templates/book-non-fiction.org"))

(defun my/denote-template-fiction-book ()
  (my/denote-template-from-file "literature/templates/book-fiction.org"))

(defun my/denote-template-generic ()
  "Empty generic template."
  "")

(after! denote
  (setq denote-templates
        '((non-fiction-book . my/denote-template-non-fiction-book)
          (fiction-book     . my/denote-template-fiction-book)
          (generic          . my/denote-template-generic))))

(use-package! denote-journal
  :after denote
  :commands (denote-journal-new-entry
             denote-journal-new-or-existing-entry
             denote-journal-link-or-create-entry)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  (setq denote-journal-directory   (expand-file-name "journal" denote-directory)
        denote-journal-keyword     "journal"
        denote-journal-title-format 'day-date-month-year
        denote-journal-interval    'daily))

(use-package! consult-notes
  :config
  (setq consult-notes-file-dir-sources
        `(("Notes" ?o ,my/notes-directory)))
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(use-package! citar
  :defer t
  :init
  ;; In :init so commands that read `citar-bibliography' before citar loads
  ;; (e.g. `my/denote-book-note') see it.
  (let ((lit (expand-file-name "literature" my/notes-directory)))
    (setq citar-bibliography (list my/references-bib)
          bibtex-completion-bibliography (list my/references-bib)
          citar-library-paths (list my/references-library)
          bibtex-completion-library-path (list my/references-library)
          citar-notes-paths (list lit)
          bibtex-completion-notes-path lit))
  (setq org-cite-activate-processor 'citar
        org-cite-insert-processor   'citar
        org-cite-follow-processor   'citar
        org-cite-export-processors
        '((html  . (csl "chicago-author-date.csl"))
          (latex . biblatex)
          (t     . (csl "chicago-author-date.csl")))))

(use-package! citar-denote
  :after (denote citar)
  :config
  (setq citar-denote-subdir       "literature"
        citar-denote-keyword      "bib"
        citar-denote-file-type    'org
        citar-denote-title-format 'author-year-title
        citar-denote-signature    'citekey
        citar-denote-template     t)
  (citar-denote-mode 1))

;;; HTTP

(use-package! verb
  :defer t
  :config
  (setq verb-suppress-load-unsecure-prelude-warning t)
  (after! org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

(map! :leader
      (:prefix ("v" . "verb")
       :desc "send request" "r" #'verb-send-request-on-point-other-window))

;;; Key Quiz
;; SPC o k starts a round; SPC u SPC o k is reverse mode.
(use-package! key-quiz
  :commands key-quiz
  :config
  (setq key-quiz-game-length 20))

(map! :leader
      (:prefix "o"
       :desc "Key quiz" "k" #'key-quiz))

;;; AI

(use-package! copilot-chat)

(map! :leader
      (:prefix ("l" . "co-chat")
       :desc "Chat Prompt"                     "p" #'copilot-chat-prompt
       :desc "Reset"                           "r" #'copilot-chat-reset
       :desc "Chat buffers"                    "d" #'copilot-chat-display
       :desc "Explain selected"                "e" #'copilot-chat-explain
       :desc "Review selected"                 "v" #'copilot-chat-review
       :desc "Document selected"               "c" #'copilot-chat-doc
       :desc "Fix selected"                    "f" #'copilot-chat-fix
       :desc "Optimize selected"               "o" #'copilot-chat-optimize
       :desc "Write tests for selected"        "t" #'copilot-chat-test
       :desc "Ask about selection"             "a" #'copilot-chat-custom-prompt-selection
       :desc "Current buffer to copilot chat"  "s" #'copilot-chat-add-current-buffer
       :desc "Buffer list"                     "b" #'copilot-chat-list
       :desc "Previous prompt history"         "h" #'copilot-chat-prompt-history-previous
       :desc "Next prompt history"             "n" #'copilot-chat-prompt-history-next))

;; The Anthropic key comes from auth-source (Proton Pass if mapped above,
;; else ~/.authinfo.gpg). LM Studio only works while its local server runs.
(use-package! gptel
  :defer t
  :config
  (gptel-make-openai "LM Studio"
    :host "localhost:1234"
    :protocol "http"
    :models '("openai/gpt-oss-20b")
    :stream t)
  (setq gptel-model 'claude-sonnet-4-20250514
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (auth-source-pick-first-password
                              :host "api.anthropic.com"
                              :user "apikey"))))

;; Exposes this Emacs to MCP clients over a local Unix socket at
;; ~/.config/emacs/.local/cache/emacs-mcp-server.sock (clients connect with
;; `socat - UNIX-CONNECT:<socket>').
(use-package! mcp-server
  :defer t
  :init
  (setq mcp-server-emacs-tools-enabled 'all)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (require 'mcp-server)
              (mcp-server-start-unix))))

;;; Feeds

(after! elfeed
  (setq rmh-elfeed-org-files (list my/elfeed-org-file)))

;;; Nomad Network
;; Loaded straight from the checkout (reticulum/ and nomadnet/ packages), so
;; edits apply on reload without `doom sync'. Shares ~/.nomadnetwork with the
;; terminal client; don't run both at once.
(use-package! nomadnet
  :when (file-directory-p my/nomadnet-directory)
  :load-path (lambda () (list (expand-file-name "nomadnet" my/nomadnet-directory)
                              (expand-file-name "reticulum" my/nomadnet-directory)))
  :commands (nomadnet nomadnet-conversations nomadnet-announces
             nomadnet-known-nodes nomadnet-browse nomadnet-guide
             nomadnet-conversation-new nomadnet-peer-info
             nomadnet-announce-now nomadnet-sync-messages)
  :init
  (set-popup-rule! "^\\*NomadNet" :ignore t)
  (map! :leader
        (:prefix ("r" . "reticulum")
         (:prefix ("n" . "nomadnet")
          :desc "Dashboard"        "d" #'nomadnet
          :desc "Conversations"    "c" #'nomadnet-conversations
          :desc "Announces"        "a" #'nomadnet-announces
          :desc "Known nodes"      "k" #'nomadnet-known-nodes
          :desc "Browse node URL"  "b" #'nomadnet-browse
          :desc "Guide"            "g" #'nomadnet-guide
          :desc "New conversation" "m" #'nomadnet-conversation-new))))
