;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;; Paul's Doom config.
;;
;; Layout:
;;   $DOOMDIR/init.el      Doom modules.
;;   $DOOMDIR/packages.el  Package declarations.
;;   $DOOMDIR/config.el    This file. Public.
;;   $DOOMDIR/local.el     Per-machine settings (paths, mail address, GPG key,
;;                         opt-in features). Gitignored, never committed.
;;                         See "Per-machine config (local.el)" below.
;;   $DOOMDIR/modules/     Custom local modules.
;;
;; Bootstrapping a new machine:
;;   1. Clone dotfiles, `doom sync'.
;;   2. Start Emacs. It will warn that local.el is missing.
;;   3. M-x my/write-local-template -- scaffolds local.el with every expected
;;      variable set to a blank default.
;;   4. Edit local.el, fill in the paths/keys for this machine, restart.

;;; Helpers

;; Logging, prefixed with @@@ so my own messages stand out in *Messages*.
(defun my-message (format-string &rest args)
  "Display a personal message prefixed with @@@ in *Messages* buffer."
  (apply #'message (concat "@@@ " format-string) args))

(defun my/add-to-load-path (&rest paths)
  "Add each PATH in PATHS to `load-path` and log it."
  (dolist (path paths)
    (add-to-list 'load-path path)
    (my-message "Added %s to load-path" path)))

(defun my/non-empty (s)
  "Return S if it's a non-empty string, otherwise nil."
  (and (stringp s) (not (string-empty-p s)) s))

;;; Per-machine config (local.el)
;;
;; Anything that varies between machines or shouldn't be in a public repo
;; (absolute paths, mail address, GPG key id, opt-in feature flags) lives in
;; $DOOMDIR/local.el. That file is gitignored.
;;
;; The list `my/local-vars' below is the *single source of truth* for what
;; local.el contains. To add a new per-machine knob, append an entry here and
;; reference the variable from elsewhere in this file. Empty-string or nil
;; values are treated as "feature off" by the consumers below.

(defvar my/local-vars
  '((my-location              ""    "Free-form label for this machine, e.g. \"home\" or \"work\".")
    (user-mail-address        ""    "Mail address used by Emacs (org-capture, magit commits via git config, etc.).")
    (my/notes-directory       ""    "Root of denote/consult-notes. Example: \"~/Sync/notes\".")
    (my/org-directory         ""    "Value for `org-directory'. Example: \"~/Sync/org\".")
    (my/org-extra-agenda-files nil  "Extra dirs/files appended to `org-agenda-files' beyond `my/org-directory'. List of strings.")
    (my/references-bib        ""    "Path to references.bib for citar/bibtex.")
    (my/references-library    ""    "Directory containing PDFs etc. for citar.")
    (my/projects-directory    ""    "Projectile project search root. Example: \"~/Projects\".")
    (my/snippets-directory    ""    "Path to doom-snippets clone. Example: \"~/Projects/snippets\".")
    (my/nomadnet-directory    ""    "Path to nomadnet.el checkout. Example: \"~/Projects/nomadnet.el\". Empty = off.")
    (my/elfeed-org-file       ""    "Path to elfeed-org feeds file.")
    (my/gpg-program           ""    "Absolute path to gpg binary, e.g. \"/usr/bin/gpg\".")
    (my/gpg-key-id            ""    "GPG key id used as recipient for `epa-file-encrypt-to'.")
    (my/mermaid-cli           ""    "Absolute path to mmdc, e.g. \"/usr/bin/mmdc\" (npm: @mermaid-js/mermaid-cli).")
    (my/local-features        nil  "List of opt-in feature symbols, e.g. (gitlab postman). Consumed by packages.el and elsewhere.")
    (my/mail-accounts         nil  "Personal mail accounts to enable on this machine, e.g. (proton). nil = no mail setup (e.g. the work box)."))
  "Schema for `local.el'. Each entry is (SYMBOL DEFAULT DOCSTRING).
`my/write-local-template' uses this to generate a blank skeleton.")

;; Template generator. M-x my/write-local-template scaffolds a blank local.el.
;; If one already exists, it writes to local.el.template instead so existing
;; values aren't clobbered -- merge by hand.
(defun my/local-file ()
  "Path to the per-machine local.el."
  (expand-file-name "local.el" doom-user-dir))

(defun my/write-local-template (&optional force)
  "Write a blank `local.el' scaffold derived from `my/local-vars'.
If `local.el' already exists and FORCE (prefix arg) is nil, write to
`local.el.template' instead so existing settings aren't lost."
  (interactive "P")
  (let* ((target (my/local-file))
         (out (if (and (file-exists-p target) (not force))
                  (concat target ".template")
                target)))
    (with-temp-file out
      (insert ";;; local.el --- Per-machine settings -*- lexical-binding: t; -*-\n")
      (insert ";;\n")
      (insert ";; This file is gitignored. Fill in values for this machine.\n")
      (insert ";; Empty string \"\" or nil means \"feature off on this machine\".\n")
      (insert ";; Schema is defined by `my/local-vars' in config.el.\n\n")
      (dolist (entry my/local-vars)
        (let ((sym (nth 0 entry))
              (default (nth 1 entry))
              (doc (nth 2 entry)))
          (when doc (insert (format ";; %s\n" doc)))
          (insert (format "(setq %s %S)\n\n" sym default))))
      (insert "(provide 'local)\n"))
    (message "Wrote %s" out)
    out))

;; Loading local.el. Loaded early so subsequent sections can see the
;; variables. If absent, we `setq' each var to its default and warn -- Emacs
;; still starts up usable.
;;
;; As a last resort, `my-location' comes from the DOTFILES_LOCATION
;; environment variable (set it in the untracked ~/.zshrc.local), so a
;; freshly-cloned machine still self-identifies before local.el exists. A
;; value set in local.el always wins. Hostnames stay out of this public file.
(dolist (entry my/local-vars)
  (let ((sym (nth 0 entry))
        (default (nth 1 entry)))
    (unless (boundp sym)
      (set sym default))))

(let ((local-file (my/local-file)))
  (if (file-exists-p local-file)
      (load local-file nil 'nomessage)
    (my-message "No local.el found at %s. Run M-x my/write-local-template" local-file)))

;; Environment fallback for `my-location' (only when local.el left it blank).
(unless (my/non-empty my-location)
  (setq my-location (or (getenv "DOTFILES_LOCATION") "")))

(my-message "Current location: %s" (or (my/non-empty my-location) "unknown"))

;;; Per-machine private code (conditional loading)
;;
;; Some configuration is private (work-sensitive code, credentials-adjacent
;; glue) and must never live in this public repo. Such code lives outside the
;; tree in ~/.config/<location>/my-<location>.el -- e.g. on the work machine,
;; ~/.config/work/my-work.el. Those files are gitignored and loaded here only
;; when present, keyed off `my-location'.
;;
;; This is the counterpart to local.el: local.el holds *values* (paths, keys)
;; declaratively, while this loads private *code* for the active machine. On a
;; machine with no such file (the common case) the block is a no-op.
;;
;; We `load' the file directly rather than `require' it, so the private file
;; does *not* need a trailing (provide 'my-<location>); any my-<location>.el
;; just works. The directory is still added to `load-path' so the private file
;; can `require' its own helpers if it wants to.
(let ((loc (my/non-empty my-location)))
  (when loc
    (let* ((config-dir (expand-file-name (format "~/.config/%s" loc)))
           (config-lib (format "my-%s" loc))
           (lib (progn (add-to-list 'load-path config-dir)
                       (locate-library config-lib))))
      (if lib
          (progn
            (load lib nil 'nomessage)
            (my-message "Loaded private config %s" lib))
        (my-message "No private config %s in %s (ok)." config-lib config-dir)))))

;;; Identity
;; `user-mail-address' is set via local.el.
(setq user-full-name "Paul Meier")

;;; Email
;;
;; Personal mail via notmuch (https://notmuchmail.org/): Proton Mail Bridge
;; exposes the Proton account as local IMAP/SMTP (127.0.0.1:1143 / :1025),
;; mbsync fetches it into ~/.mail/proton, notmuch indexes it, msmtp sends
;; through the Bridge -- the Bridge password comes from `pass'. Gated on
;; `my/mail-accounts' (set in local.el) so a machine that leaves the list nil
;; -- e.g. the work box -- gets no mail setup at all. The moving parts live in
;; external dotfiles, not here: ~/.mbsyncrc, ~/.notmuch-config, ~/.msmtprc,
;; the sync script ~/.local/bin/mailsync, and a launchd agent
;; (~/Library/LaunchAgents/local.mailsync.plist) that fetches every 5 minutes.
;; The Bridge app must be running and logged in; mailsync skips quietly if
;; its port isn't listening.
;;
;; Inside Emacs: SPC o m opens notmuch, <localleader> u fetches + reindexes.
;; Deleting (SPC m d, which tags `trash') moves a message to Proton's Trash on
;; the next sync: ~/.mbsyncrc is two-way (Create Both / Expunge Both) and the
;; mailsync script relocates tag:trash messages into the Trash maildir before
;; syncing, so the Inbox label is removed server-side.
(when (and (memq 'proton my/mail-accounts) (executable-find "notmuch"))
  (after! notmuch
    ;; Module default backend is 'lieer (Gmail-OAuth only). Point `<localleader> u'
    ;; at our mailsync script (trash-move step + mbsync + notmuch new) so the
    ;; manual sync runs the SAME path as the launchd auto-sync.
    (setq +notmuch-sync-backend (expand-file-name "~/.local/bin/mailsync"))
    ;; Land on the inbox search instead of the notmuch-hello buffer.
    (setq +notmuch-home-function (lambda () (notmuch-search "tag:inbox")))
    ;; Doom's notmuch module already sets message-send-mail-with-sendmail and
    ;; notmuch-fcc-dirs nil (fine for Proton: the Bridge files sent mail into
    ;; Sent itself, so no local Fcc copy is needed).
    ;; We only add the msmtp binary + From:-header account routing on top.
    (setq sendmail-program (executable-find "msmtp")
          message-sendmail-f-is-evil t
          message-sendmail-extra-arguments '("--read-envelope-from"))
    ;; Doom's default saved searches (inbox/flagged/sent/drafts) + unread/all.
    (setq notmuch-saved-searches
          '((:name "inbox"   :query "tag:inbox not tag:trash" :key "i")
            (:name "unread"  :query "tag:unread"              :key "u")
            (:name "flagged" :query "tag:flagged"             :key "f")
            (:name "sent"    :query "tag:sent"                :key "s")
            (:name "drafts"  :query "tag:draft"               :key "d")
            (:name "all"     :query "*"                       :key "a"))))
  ;; Resolve mail secrets from the `pass' store via auth-source.
  (after! auth-source (auth-source-pass-enable)))

;;; Custom modules
;; Local Elisp modules live under $DOOMDIR/modules.
(my/add-to-load-path (expand-file-name "modules" doom-user-dir))
(require 'isbn-to-bibtex)
(require 'my-book-cover)
(require 'book-note)
(require 'lit-mode)
(require 'comfy-mode)

;;; UI

;; Splash
(setq fancy-splash-image (concat doom-user-dir "alfie.png"))

;; Fonts. "Noto Sans Symbols 2" fixes unicode boxes for org bullets. On Arch /
;; CachyOS the monospace + symbol fonts come from the repos; "SN Pro" is
;; installed manually under ~/.local/share/fonts.
;;
;;   sudo pacman -S ttf-fira-code noto-fonts   # Fira Code + Noto Sans Symbols 2
(setq doom-font (font-spec :family "Fira Code" :size 15)
      doom-variable-pitch-font (font-spec :family "SN Pro" :size 15 :weight 'medium)
      doom-big-font (font-spec :family "Fira Code" :size 24))

;; Frame. Start maximized. Under Hyprland (Wayland) the compositor tiles and
;; draws its own borders, so `undecorated' drops Emacs' redundant title bar. On
;; the macOS `ns' port, though, an undecorated frame *cannot* be maximized
;; (there's no title bar for the OS to maximize against), which leaves the
;; frame stuck as a small box -- so only drop the title bar off macOS.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(unless (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; `default-frame-alist' alone is unreliable for the *initial* frame on the
;; macOS port (the frame is created before config.el runs and often stays a
;; small box), so maximize it explicitly once the window system is fully up.
;; The menu bar is re-enabled here too (Doom hides it); the icon tool bar
;; stays off. Terminal frames via `emacs -nw' keep Doom's bare look.
(add-hook 'window-setup-hook
          (defun my/setup-initial-gui-frame-h ()
            (when (display-graphic-p)
              (menu-bar-mode 1)
              (tool-bar-mode -1)
              (set-frame-parameter nil 'fullscreen 'maximized))))

;; Modeline
(display-battery-mode 1)

;; Quiet warnings
(setq warning-minimum-level :emergency)

;;; Editor

;; Drag lines with M-arrows
(use-package! drag-stuff
  :defer t
  :init
  (map! "<M-up>"    #'drag-stuff-up
        "<M-down>"  #'drag-stuff-down
        "<M-left>"  #'drag-stuff-left
        "<M-right>" #'drag-stuff-right))

;; Formatters
(setq-hook! 'javascript-mode-hook     +format-with 'prettier)
(setq-hook! 'typescript-mode-hook     +format-with 'prettier)
(setq-hook! 'typescript-tsx-mode-hook +format-with 'prettier)

;; Inspect formatter for a mode
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

;; Jinja2 templates. Jinja2-templated files -- including .j2.yml / .j2.yaml
;; (Ansible, Helm, Salt) -- open in `jinja2-mode' (package declared in
;; packages.el). Plain .yml / .yaml are intentionally left to Doom's
;; `:lang yaml +tree-sitter' mode, so this only claims the .j2 variants.
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\.ya?ml\\'" . jinja2-mode))
(add-hook 'jinja2-mode-hook (lambda () (setq jinja2-enable-indent t)))

;;; Encryption
;; `epg-gpg-program' and the recipient key come from local.el.
(after! epg
  (when (my/non-empty my/gpg-program)
    (setq epg-gpg-program my/gpg-program))
  (setq epg-pinentry-mode 'ask))

(when (my/non-empty my/gpg-key-id)
  (setq epa-file-encrypt-to (list my/gpg-key-id)
        epa-file-select-keys 'silent))

(epa-file-enable)

;;; Secrets (Proton Pass)
;;
;; Secrets come from Proton Pass through `pass-cli', via my package
;; https://github.com/paulmeier/proton-pass.el (see packages.el). Nothing is
;; fetched at startup; `pass-cli' takes a few seconds per call, so
;; auth-source hands out a lazy secret and fetched values are cached in
;; memory for an hour.
;;
;; - `proton-pass' goes first in `auth-sources': any `auth-source-search'
;;   (gptel, smtpmail, forge, ...) is answered from
;;   `proton-pass-auth-source-alist' before ~/.authinfo.gpg is consulted.
;; - In config code, use (proton-pass-get "pass://Vault/Item/field").
;; - The Proton Pass SSH agent socket is exported so magit/tramp use the SSH
;;   keys stored in Pass.
;; - SPC P P opens a pass.el-style browser of the vault; SPC P has the
;;   password-store-style commands (insert, generate, edit, rename, trash).
;;
;; Item titles aren't secret, so the mapping below lives in this public file.
;; Only map items that exist and whose titles are unique: a missing item
;; errors instead of falling back to ~/.authinfo.gpg.
(use-package! proton-pass
  :demand t ; cheap to load: no pass-cli calls until a secret is used
  :config
  (setq proton-pass-vault "Personal"
        proton-pass-auth-source-alist
        '(;; (HOST USER URI) -- USER nil matches any user.
          ;; ("api.anthropic.com" "apikey" "pass://Personal/Anthropic API/password")
          ;; ("127.0.0.1" nil "pass://Personal/Proton Mail Bridge/password")
          ))
  (proton-pass-auth-source-enable)
  (proton-pass-use-ssh-agent))

(map! :leader
      (:prefix ("P" . "proton pass")
       :desc "Browse vault"        "P" #'proton-pass
       :desc "View item"           "v" #'proton-pass-view
       :desc "Copy password"       "p" #'proton-pass-copy-password
       :desc "Copy username"       "u" #'proton-pass-copy-username
       :desc "Copy field"          "f" #'proton-pass-copy-field
       :desc "Copy TOTP code"      "t" #'proton-pass-totp
       :desc "Open URL"            "o" #'proton-pass-url
       :desc "Insert item"         "i" #'proton-pass-insert
       :desc "Generate item"       "g" #'proton-pass-generate
       :desc "Edit field"          "e" #'proton-pass-edit
       :desc "Rename item"         "r" #'proton-pass-rename
       :desc "Trash item"          "d" #'proton-pass-remove
       :desc "Password at point"   "G" #'proton-pass-insert-generated-password
       :desc "Switch vault"        "V" #'proton-pass-switch-vault
       :desc "Clear cache"         "c" #'proton-pass-clear-cache
       :desc "Session info"        "?" #'proton-pass-info))

;; pass.el-style single keys in the browser and item views. Bound for evil
;; normal state explicitly (not an overriding map, which would inherit
;; special-mode's SPC and shadow the leader); j/k/gg still move.
(map! :after proton-pass
      :map (proton-pass-mode-map proton-pass-view-mode-map)
      :n "w" #'proton-pass-copy-password
      :n "b" #'proton-pass-copy-username
      :n "f" #'proton-pass-copy-field
      :n "o" #'proton-pass-totp
      :n "U" #'proton-pass-url
      :n "e" #'proton-pass-edit
      :n "r" #'proton-pass-rename
      :n "d" #'proton-pass-remove
      :n "i" #'proton-pass-insert
      :n "I" #'proton-pass-generate
      :n "V" #'proton-pass-switch-vault
      :n "q" #'quit-window
      :n "?" #'describe-mode
      :map proton-pass-mode-map
      :n "RET" #'proton-pass-view
      :n "v"   #'proton-pass-view
      :n "gr"  #'proton-pass-refresh
      :map proton-pass-view-mode-map
      :n "gr"  #'proton-pass-view-refresh)

;;; Tools

;; Projectile
(when (my/non-empty my/projects-directory)
  (setq projectile-project-search-path (list my/projects-directory)))

;; Dired
(setq dired-dwim-target t)

;; YAS Snippets
(use-package! doom-snippets
  :when (my/non-empty my/snippets-directory)
  :load-path (lambda () (list my/snippets-directory))
  :after yasnippet)

;;; Org

;; Setup
(defun my/org-mode-setup ()
  (when (my/non-empty my/mermaid-cli)
    (setq ob-mermaid-cli-path my/mermaid-cli))
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

  (when (my/non-empty my/org-directory)
    (setq org-directory my/org-directory)
    (setq org-agenda-files (cons my/org-directory
                                 (or my/org-extra-agenda-files '()))))

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

;; Helper: convert lines to checkboxes
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

;; Helper: filename-safe slug from #+title
(defun my/org-title-slug ()
  "Return a filesystem-safe slug based on #+title or buffer name."
  (let* ((title (or (org-get-title)
                    (file-name-base (or (buffer-file-name) "diagram"))))
         (slug  (downcase
                 (replace-regexp-in-string "[^[:alnum:]]+" "-" title))))
    slug))

;; Org-timeblock
(use-package! org-timeblock
  :config
  (setq org-timeblock-inbox-file "timeblock.org"))

;; Babel auto-tangle for selected files. Saves of files whose basename is in
;; `my/org-babel-tangle-files' will auto-tangle (e.g. zshrc.org). The
;; per-machine private sources are listed too so that, on the machine where
;; they exist (e.g. zshrc-work.org / my-work.org on the work box), editing them
;; re-tangles the loadable output. Listing a file that doesn't exist on this
;; machine is harmless -- the hook only fires when such a file is actually
;; saved.
(setq my/org-babel-tangle-files
      '("zshrc.org"
        "zshrc-work.org" "zshrc-home.org"
        "my-work.org" "my-home.org" "my-desktop.org"))

(defun my/tangle-org-files-in-directory (directory)
  "Tangle all .org files in DIRECTORY."
  (interactive "DDirectory: ")
  (dolist (file (directory-files-recursively directory "\\.org\\'"))
    (my-message "Tangling %s..." file)
    (org-babel-tangle-file file)))

(defun my/org-babel-tangle-on-save ()
  "Auto-tangle if current file's basename is in `my/org-babel-tangle-files'."
  (when (and (string= (file-name-extension buffer-file-name) "org")
             (member (file-name-nondirectory buffer-file-name)
                     my/org-babel-tangle-files))
    (org-babel-tangle)))

(add-hook 'after-save-hook #'my/org-babel-tangle-on-save)

;; Keybinding: expand src block
(map! :leader
      :desc "Expand Org source block"
      "y b" #'org-babel-expand-src-block)

;;; Notes

;; Denote
(use-package! denote
  :config
  (when (my/non-empty my/notes-directory)
    (setq denote-directory          my/notes-directory
          denote-directories        (list my/notes-directory)
          denote-dired-directories  (list my/notes-directory)))
  (setq denote-prompts '(subdirectory title keywords template)))

;; Denote keybindings
(map! :leader
      (:prefix ("e" . "denote")
       :desc "New Note"        "n" #'denote
       :desc "Insert Link"     "i" #'denote-insert-link
       :desc "Journal"         "j" #'denote-journal-new-or-existing-entry
       :desc "New Book Note"   "t" #'my/denote-book-note
       :desc "Find in Notes"   "f" #'consult-notes-search-in-all-notes
       :desc "Search Notes"    "s" #'consult-notes))

(map! :leader
      :desc "Toggle lit mode"   "t W" #'lit-mode
      :desc "Toggle comfy mode" "t C" #'comfy-mode)

;; Denote templates. `denote-templates' references functions defined here.
;; The functions read template bodies from files inside `my/notes-directory'.
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

(defun my/denote-template-article ()
  (my/denote-template-from-file "literature/templates/article.org"))

(defun my/denote-template-paper ()
  (my/denote-template-from-file "literature/templates/paper.org"))

(defun my/denote-journal-daily ()
  "Daily Journal note."
  (my/denote-template-from-file "templates/journal-daily.org"))

(defun my/denote-template-generic ()
  "Empty generic template."
  "")

(after! denote
  (setq denote-templates
        '((non-fiction-book . my/denote-template-non-fiction-book)
          (fiction-book     . my/denote-template-fiction-book)
          (article          . my/denote-template-article)
          (paper            . my/denote-template-paper)
          (daily            . my/denote-journal-daily)
          (generic          . my/denote-template-generic))))

;; Denote journal
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

;; Consult-notes
(use-package! consult-notes
  :config
  (when (my/non-empty my/notes-directory)
    (setq consult-notes-file-dir-sources
          `(("Notes" ?o ,my/notes-directory))))
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

;; Citar
(use-package! citar
  :defer t
  :init
  ;; Path-based settings must be `:init` so they apply before citar is
  ;; autoloaded -- otherwise commands that read `citar-bibliography'
  ;; (e.g. `my/denote-book-note') see nil until citar happens to load.
  (when (my/non-empty my/references-bib)
    (setq citar-bibliography (list my/references-bib)
          bibtex-completion-bibliography (list my/references-bib)))
  (when (my/non-empty my/references-library)
    (setq citar-library-paths (list my/references-library)
          bibtex-completion-library-path (list my/references-library)))
  (when (my/non-empty my/notes-directory)
    (let ((lit (expand-file-name "literature" my/notes-directory)))
      (setq citar-notes-paths (list lit)
            bibtex-completion-notes-path lit)))

  (setq org-cite-activate-processor 'citar
        org-cite-insert-processor   'citar
        org-cite-follow-processor   'citar
        org-cite-export-processors
        '((html  . (csl "chicago-author-date.csl"))
          (latex . biblatex)
          (t     . (csl "chicago-author-date.csl")))))

;; Citar-Denote
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

;; Verb
;; Key Quiz: practice keybindings as a game. SPC o k starts a round;
;; SPC u SPC o k starts reverse mode (given a key, name the command).
(use-package! key-quiz
  :commands key-quiz
  :config
  (setq key-quiz-game-length 20))

(map! :leader
      (:prefix "o"
       :desc "Key quiz" "k" #'key-quiz))

(use-package! verb
  :defer t
  :config
  (setq verb-suppress-load-unsecure-prelude-warning t)
  (after! org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

(map! :leader
      (:prefix ("v" . "verb")
       :desc "send request" "r" #'verb-send-request-on-point-other-window))

;;; AI

;; Copilot Chat
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
       :desc "Custom prompt with selection"    "P" #'copilot-chat-custom-prompt-selection
       :desc "Current buffer to copilot chat"  "s" #'copilot-chat-add-current-buffer
       :desc "Buffer list"                     "b" #'copilot-chat-list
       :desc "Previous prompt history"         "h" #'copilot-chat-prompt-history-previous
       :desc "Next prompt history"             "n" #'copilot-chat-prompt-history-next))

;; GPTel. The Anthropic key is read from auth-source: Proton Pass if mapped in
;; `proton-pass-auth-source-alist', else ~/.authinfo.gpg. LM
;; Studio backend is configured but only useful when the local server is
;; running.
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

;; Emacs MCP Server. https://github.com/rhblind/emacs-mcp-server exposes this
;; Emacs to MCP clients (e.g. Claude) over a local Unix socket, letting an LLM
;; read/modify buffers, eval Elisp, and drive org. Package is declared in
;; packages.el. Clients need the `socat' binary on PATH to connect.
;;
;; The server listens on a local-only socket at
;; ~/.config/emacs/.local/cache/emacs-mcp-server.sock. It ships its own
;; security layer that prompts before dangerous operations (see the repo's
;; mcp-server-security-* variables). Manage it with M-x mcp-server-status /
;; mcp-server-stop / mcp-server-restart.
;;
;; A client connects by piping the socket through socat, e.g. in an MCP client
;; config:
;;
;;   { "command": "socat",
;;     "args": ["-", "UNIX-CONNECT:~/.config/emacs/.local/cache/emacs-mcp-server.sock"] }
(use-package! mcp-server
  :defer t
  :init
  ;; Settings in `:init' so they apply before the package is pulled in
  ;; (same reasoning as the citar block above).
  (setq mcp-server-emacs-tools-enabled 'all)
  ;; Start the local Unix-socket server once Emacs has finished starting.
  ;; `require' explicitly so this works regardless of autoload cookies.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (require 'mcp-server)
              (mcp-server-start-unix))))

;;; Feeds

;; Elfeed
(after! elfeed
  (when (my/non-empty my/elfeed-org-file)
    (setq rmh-elfeed-org-files (list my/elfeed-org-file))))

;;; Nomad Network
;;
;; Reticulum/LXMF client (https://github.com/markqvist/NomadNet) for Emacs,
;; loaded straight from the nomadnet.el checkout (the checkout holds two
;; packages: reticulum/ the protocol library and nomadnet/ the client), so
;; edits there take effect on the next reload; no `doom sync' needed. Pure
;; Emacs Lisp; shares ~/.nomadnetwork with the terminal client, so do not run
;; both at once.
(use-package! nomadnet
  :when (my/non-empty my/nomadnet-directory)
  :load-path (lambda () (list (expand-file-name "nomadnet" my/nomadnet-directory)
                              (expand-file-name "reticulum" my/nomadnet-directory)))
  :commands (nomadnet nomadnet-conversations nomadnet-announces
             nomadnet-known-nodes nomadnet-browse nomadnet-guide
             nomadnet-conversation-new nomadnet-peer-info
             nomadnet-announce-now nomadnet-sync-messages)
  :init
  ;; nomadnet is a full-window application, not a popup: keep Doom's popup
  ;; system away from its buffers so they open in the main window.
  (set-popup-rule! "^\\*NomadNet" :ignore t)
  ;; SPC r is the Reticulum namespace; nomadnet lives under SPC r n so other
  ;; Reticulum tools can get their own sub-prefixes later.
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

;;; Debug
;; Uncomment to get backtraces.
;; (setq debug-on-error t)

;;; Platform
;;
;; On Wayland the GTK/X11 Emacs build runs under XWayland, where the compositor
;; does *not* deliver Alt as Meta -- M-x (and other M- chords) silently do
;; nothing. Use the native Wayland (PGTK) build instead. On Arch/CachyOS:
;;
;;   sudo pacman -S extra/emacs-wayland   # PGTK build; replaces `emacs', same version
;;
;; The guard below warns at startup if a non-PGTK GUI build is running under a
;; Wayland session, so the broken-M-x symptom can't return unnoticed.
(when (and (display-graphic-p)
           (not (featurep 'pgtk))
           (string= (or (getenv "XDG_SESSION_TYPE") "") "wayland"))
  (display-warning
   'config
   (concat "Non-PGTK Emacs under Wayland: Alt/Meta (M-x) is likely broken "
           "via XWayland. Install the PGTK build (e.g. `emacs-wayland').")
   :warning))
