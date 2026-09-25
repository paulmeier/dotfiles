;;; ews.el --- Emacs Writing Studio as a Doom minor mode  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `ews-mode' turns this Doom setup into Emacs Writing Studio
;; (https://github.com/pprevos/emacs-writing-studio): evil off, stock Emacs
;; keys, EWS's `C-c w' keymap, its packages, hooks, theme and settings.
;; Turning it off restores every variable, global mode, buffer mode and theme
;; it changed, and turns evil back on.
;;
;;   SPC t e   turn EWS on (from Doom/evil)
;;   C-c w q   turn EWS off (back to Doom/evil)
;;   M-SPC     Doom's leader while EWS is on
;;
;; Differences from stock EWS: backups, custom-file, elfeed's database and the
;; scratch buffer stay Doom's; screenshots use macOS `screencapture' when
;; `maim' is missing; theme is the built-in modus-themes.
;;
;; The helper commands are adapted from EWS's ews.el,
;; Copyright (C) 2024-2025 Peter Prevos, GPL-3.0-or-later.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;; Customisation

(defgroup ews ()
  "Emacs Writing Studio."
  :group 'files
  :link '(url-link :tag "Homepage" "https://lucidmanager.org/tags/emacs/"))

(defcustom ews-bibtex-directory
  (expand-file-name "library" (getenv "HOME"))
  "Location of BibTeX files and attachments."
  :type 'directory)

(defcustom ews-denote-para-keywords
  '("projects" "areas" "resources" "archives")
  "List of keywords to use for implementing the PARA method with Denote."
  :type '(repeat string))

(defcustom ews-hunspell-dictionaries "en_US"
  "Comma-separated list of Hunspell dictionaries."
  :type 'string)

(defcustom ews-org-heading-level-capitalise nil
  "Minimum level of Org headings to be capitalised.
Nil implies all levels are capitalised."
  :type '(choice (const :tag "All headings" nil)
                 (integer :tag "Highest level" 1)))

(defcustom ews-theme 'modus-operandi-tinted
  "Theme loaded when `ews-mode' turns on."
  :type 'symbol)

(defvar ews-bibtex-files nil
  "List of BibTeX files. Use `ews-bibtex-register' to configure.")

;;; External software

;;;###autoload
(defun ews-missing-executables (&optional prog-list)
  "Identify missing executables in PROG-LIST.
Sublists indicate that one of the entries is required.  Interactively,
check the programs EWS uses."
  (interactive)
  (let ((missing '()))
    (dolist (exec (or prog-list
                      '(("gs" "mutool") "pdftotext" "soffice" "zip" "ddjvu"
                        "curl" ("mpg321" "ogg123" "mplayer" "mpv" "vlc")
                        ("grep" "ripgrep") ("convert" "gm") "dvipng" "latex"
                        "hunspell" "git")))
      (if (listp exec)
          (unless (cl-some #'executable-find exec)
            (push (format "(%s)" (mapconcat #'identity exec " or ")) missing))
        (unless (executable-find exec)
          (push exec missing))))
    (if missing
        (message "Missing executable files(s): %s"
                 (mapconcat #'identity missing ", "))
      (message "No missing executable files."))))

;;; Bibliography

(defun ews--bibtex-files ()
  "Return the BibTeX files in `ews-bibtex-directory'."
  (when (file-exists-p ews-bibtex-directory)
    (directory-files ews-bibtex-directory t "^[A-Za-z0-9].+\\.bib$")))

;;;###autoload
(defun ews-bibtex-register ()
  "Register the contents of the `ews-bibtex-directory' with `ews-bibtex-files'.
Use when adding or removing a BibTeX file from or to `ews-bibtex-directory'."
  (interactive)
  (let ((bib-files (ews--bibtex-files)))
    (setq ews-bibtex-files bib-files
          org-cite-global-bibliography bib-files
          citar-bibliography bib-files))
  (message "Registered:\n%s" (mapconcat #'identity ews-bibtex-files "\n")))

(defun ews--bibtex-combined-biblio-lookup ()
  "Combine `biblio-lookup' and `biblio-doi-insert-bibtex'."
  (require 'biblio)
  (let* ((dbs (biblio--named-backends))
         (db-list (append dbs '(("DOI" . biblio-doi-backend))))
         (db-selected (biblio-completing-read-alist "Backend:" db-list)))
    (if (eq db-selected 'biblio-doi-backend)
        (biblio-doi-insert-bibtex (read-string "DOI: "))
      (biblio-lookup db-selected))))

;;;###autoload
(defun ews-bibtex-biblio-lookup ()
  "Insert Biblio search results into current buffer or select BibTeX file."
  (interactive)
  (if-let* ((ews-bibtex-files)
            (bibfile (cond ((length= ews-bibtex-files 1) (car ews-bibtex-files))
                           ((derived-mode-p 'bibtex-mode) (buffer-file-name))
                           (t (completing-read "Select BibTeX file:"
                                               ews-bibtex-files)))))
      (progn (find-file bibfile)
             (goto-char (point-max))
             (ews--bibtex-combined-biblio-lookup)
             (save-buffer))
    (message "No BibTeX file(s) defined.")))

(defun ews--bibtex-extract-attachments ()
  "Extract attachment file names from BibTeX files in `ews-bibtex-directory'."
  (ews-bibtex-register)
  (let ((attachments '()))
    (dolist (bibtex-file ews-bibtex-files)
      (with-temp-buffer
        (insert-file-contents bibtex-file)
        (goto-char (point-min))
        (while (re-search-forward "file.*=.*{\\([^}]+\\)}" nil t)
          (dolist (file-path (split-string (match-string 1)
                                           "[[:space:]]*;[[:space:]]*"))
            (push (expand-file-name (string-trim file-path)
                                    ews-bibtex-directory)
                  attachments)))))
    attachments))

(defun ews--bibtex-extract-files ()
  "List files recursively in `ews-bibtex-directory', excluding .bib and .csl."
  (seq-remove (lambda (file)
                (or (string-suffix-p ".bib" file)
                    (string-suffix-p ".csl" file)))
              (mapcar #'expand-file-name
                      (directory-files-recursively ews-bibtex-directory ""))))

;;;###autoload
(defun ews-bibtex-missing-files ()
  "List BibTeX attachments not listed in a BibTeX file entry."
  (interactive)
  (let* ((attachments (ews--bibtex-extract-attachments))
         (missing (cl-remove-if (lambda (f) (member f attachments))
                                (ews--bibtex-extract-files))))
    (message "%s files not registered in bibliography" (length missing))
    (dolist (file missing)
      (message "%s" file))))

;;;###autoload
(defun ews-bibtex-missing-attachments ()
  "List BibTeX file entries with missing attachment(s)."
  (interactive)
  (let* ((files (ews--bibtex-extract-files))
         (missing (cl-remove-if (lambda (f) (member f files))
                                (ews--bibtex-extract-attachments))))
    (message "%s BibTeX files without matching attachment." (length missing))
    (dolist (file missing)
      (message "%s" file))))

;;; Denote

;;;###autoload
(defun ews-denote-assign-para ()
  "Move your note to either Project, Area, Resource or Archive (PARA).
Configure the PARA names with `ews-denote-para-keywords'."
  (interactive)
  (if-let* ((file (buffer-file-name))
            ((denote-filename-is-note-p file))
            (all-keywords (string-split
                           (or (denote-retrieve-filename-keywords file) "") "_" t))
            (keywords (seq-remove (lambda (keyword)
                                    (member keyword ews-denote-para-keywords))
                                  all-keywords))
            (para (completing-read "Select category: " ews-denote-para-keywords)))
      (denote-rename-file
       file
       (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
       (cons para keywords)
       (denote-retrieve-filename-signature file))
    (message "Current buffer is not a Denote file.")))

(defun ews-denote-link-description-title-case (file file-type)
  "Return link description for FILE of FILE-TYPE.

If the region is active, use it as the description.
The title is formatted with the `titlecase' package.

This function is useful as the value of `denote-link-description-function' to
generate links in titlecase for attachments."
  (require 'titlecase)
  (let ((title (denote-retrieve-title-or-filename file file-type)))
    (cond
     ((denote--get-active-region-content))
     ((or (null title) (string-blank-p title)) "")
     ((string-match-p " " title) title)
     (t (titlecase--string
         (replace-regexp-in-string "\\([a-zA-Z0-9]\\)-\\([a-zA-Z0-9]\\)"
                                   "\\1 \\2" title)
         titlecase-style)))))

;;; Distraction-free writing

(defvar ews-olivetti-point nil
  "Stores the point position before enabling Olivetti mode.")

;;;###autoload
(defun ews-olivetti ()
  "Distraction-free writing environment enhancing Olivetti mode.

Stores the window configuration when enabling Olivetti mode.
Restores the previous configuration when exiting Olivetti mode
and moves point to the last location."
  (interactive)
  (require 'olivetti)
  (if olivetti-mode
      (progn
        (when (length= (window-list) 1)
          (jump-to-register 1)
          (goto-char ews-olivetti-point))
        (olivetti-mode 0)
        (text-scale-set 0))
    (setq ews-olivetti-point (point))
    (window-configuration-to-register 1)
    (delete-other-windows)
    (text-scale-set 1)
    (olivetti-mode t)))

;;; Org mode

;;;###autoload
(defun ews-org-insert-notes-drawer ()
  "Generate or open a NOTES drawer under the current heading.
If a drawer exists for this section, a new line is created at the end of the
current note."
  (interactive)
  (push-mark)
  (org-previous-visible-heading 1)
  (forward-line)
  (if (looking-at-p "^[ \t]*:NOTES:")
      (progn
        (org-fold-hide-drawer-toggle 'off)
        (re-search-forward "^[ \t]*:END:" nil t)
        (forward-line -1)
        (org-end-of-line)
        (org-return))
    (org-insert-drawer nil "NOTES"))
  (org-unlogged-message "Press <C-u C-SPC> to return to the previous position."))

;;;###autoload
(defun ews-org-count-words ()
  "Add word count to each heading property drawer in an Org mode buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((start (point))
            (end (save-excursion (org-end-of-subtree)))
            (word-count (count-words start end)))
       (org-set-property "WORDCOUNT" (number-to-string word-count))))))

;;;###autoload
(defun ews-org-insert-screenshot ()
  "Take a screenshot and insert it as an Org mode link.
Uses maim when installed, else macOS screencapture."
  (interactive)
  (let ((filename (read-file-name "Enter filename for screenshot: "
                                  default-directory)))
    (unless (string-equal "png" (file-name-extension filename))
      (setq filename (concat (file-name-sans-extension filename) ".png")))
    (setq filename (expand-file-name filename))
    (if (executable-find "maim")
        (call-process "maim" nil nil nil "--select" filename)
      (call-process "screencapture" nil nil nil "-i" filename))
    (insert (format "#+caption: %s\n" (read-from-minibuffer "Caption: ")))
    (insert (format "[[file:%s]]" (file-relative-name filename)))
    (org-redisplay-inline-images)))

;;;###autoload
(defun ews-org-headings-titlecase (&optional arg)
  "Cycle through all headings in an Org buffer and convert them to title case.
When used with universal argument (ARG) converts to sentence case.
Customise `titlecase-style' for styling."
  (interactive "P")
  (require 'titlecase)
  (let ((style (if arg 'sentence titlecase-style)))
    (message "Converting headings to '%s' style" style)
    (org-map-entries
     (lambda ()
       (let* ((heading (substring-no-properties (org-get-heading t t t t)))
              (new-heading (titlecase--string (downcase heading) style)))
         (when (<= (org-current-level) (or ews-org-heading-level-capitalise 999))
           (org-edit-headline new-heading)))))))

;;; Keys

(defun ews--only-in (modes command)
  "Return a key binding that runs COMMAND only in buffers derived from MODES.
Elsewhere the key falls through to the buffer's own bindings."
  `(menu-item ,(symbol-name command) ,command
              :filter ,(lambda (cmd) (and (derived-mode-p modes) cmd))))

(defvar ews-mode-map
  (define-keymap
    ;; Back to Doom
    "C-c w q"   #'ews-mode
    ;; Themes
    "C-c w t t" #'modus-themes-toggle
    "C-c w t m" #'modus-themes-select
    "C-c w t s" #'consult-theme
    ;; Help
    "C-h f"     #'helpful-function
    "C-h x"     #'helpful-command
    "C-h k"     #'helpful-key
    "C-h v"     #'helpful-variable
    ;; Spelling and grammar
    "C-c w s s" #'ispell
    "C-;"       #'flyspell-auto-correct-previous-word
    "C-c w s d" #'dictionary-lookup-definition
    "C-c w s r" #'writegood-reading-ease
    "C-c w s t" #'titlecase-dwim
    "C-c w s c" #'ews-org-headings-titlecase
    "C-c w s i" #'lorem-ipsum-insert-paragraphs
    ;; Bibliographic
    "C-c w b r" #'ews-bibtex-register
    "C-c w b b" #'ews-bibtex-biblio-lookup
    "C-c w b o" #'citar-open
    "C-c w b c" #'citar-create-note
    "C-c w b n" #'citar-denote-open-note
    "C-c w b x" #'citar-denote-nocite
    "C-c w b k" (ews--only-in '(org-mode) #'citar-denote-add-citekey)
    "C-c w b K" (ews--only-in '(org-mode) #'citar-denote-remove-citekey)
    "C-c w b d" (ews--only-in '(org-mode) #'citar-denote-dwim)
    "C-c w b e" (ews--only-in '(org-mode) #'citar-denote-open-reference-entry)
    ;; Inspiration
    "C-c w e"   #'elfeed
    "C-c w w"   #'org-web-tools-insert-link-for-url
    "C-c w m b" #'emms-browser
    "C-c w m e" #'emms
    "C-c w m p" #'emms-play-playlist
    "<XF86AudioPrev>" #'emms-previous
    "<XF86AudioNext>" #'emms-next
    "<XF86AudioPlay>" #'emms-pause
    ;; Org
    "C-c c"     #'org-capture
    "C-c l"     #'org-store-link
    "C-c a"     #'org-agenda
    "C-c w n"   (ews--only-in '(org-mode) #'ews-org-insert-notes-drawer)
    "C-c w p"   (ews--only-in '(org-mode) #'ews-org-insert-screenshot)
    "C-c w c"   (ews--only-in '(org-mode) #'ews-org-count-words)
    "C-c w h"   #'consult-org-heading
    "C-c w g"   #'consult-grep
    ;; Denote
    "C-c w d b" #'denote-find-backlink
    "C-c w d d" #'denote-date
    "C-c w d l" #'denote-find-link
    "C-c w d i" #'denote-link-or-create
    "C-c w d k" #'denote-rename-file-keywords
    "C-c w d n" #'denote
    "C-c w d r" #'denote-rename-file
    "C-c w d R" #'denote-rename-file-using-front-matter
    "C-c w d h" #'denote-org-link-to-heading
    "C-c w d f" #'consult-notes
    "C-c w d g" #'consult-notes-search-in-all-notes
    ;; Explore
    "C-c w x c" #'denote-explore-count-notes
    "C-c w x C" #'denote-explore-count-keywords
    "C-c w x b" #'denote-explore-barchart-keywords
    "C-c w x e" #'denote-explore-barchart-filetypes
    "C-c w x r" #'denote-explore-random-note
    "C-c w x l" #'denote-explore-random-link
    "C-c w x k" #'denote-explore-random-keyword
    "C-c w x x" #'denote-explore-random-regex
    "C-c w x d" #'denote-explore-identify-duplicate-notes
    "C-c w x z" #'denote-explore-zero-keywords
    "C-c w x s" #'denote-explore-single-keywords
    "C-c w x o" #'denote-explore-sort-keywords
    "C-c w x w" #'denote-explore-rename-keyword
    "C-c w x n" #'denote-explore-network
    "C-c w x v" #'denote-explore-network-regenerate
    "C-c w x D" #'denote-explore-barchart-degree
    ;; Writing
    "C-c w o"   #'ews-olivetti
    "C-M-/"     #'vundo
    ;; Files
    "C-c w r"   #'recentf-open
    "C-x r d"   #'bookmark-delete
    "C-c w I"   #'image-dired
    "C-c w v"   #'customize-variable
    ;; Doom's leader, which evil provided
    "M-SPC"     #'doom/leader
    "."          (ews--only-in '(dired-mode) #'dired-omit-mode)
    "C-<return>" (ews--only-in '(dired-mode) #'image-dired-dired-display-external)
    "k"          (ews--only-in '(image-mode) #'image-kill-buffer)
    "<right>"    (ews--only-in '(image-mode) #'image-next-file)
    "<left>"     (ews--only-in '(image-mode) #'image-previous-file)
    "C-<right>"  (ews--only-in '(image-dired-thumbnail-mode) #'image-dired-display-next)
    "C-<left>"   (ews--only-in '(image-dired-thumbnail-mode) #'image-dired-display-previous))
  "Keymap for `ews-mode'.")

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements ews-mode-map
    "C-c w"   "Emacs Writing Studio"
    "C-c w b" "Bibliographic"
    "C-c w d" "Denote"
    "C-c w m" "Multimedia"
    "C-c w s" "Spelling and Grammar"
    "C-c w t" "Themes"
    "C-c w x" "Explore"
    "C-c w q" "Quit Writing Studio"))

;;; Saving and restoring state

(defvar ews--saved-values nil
  "Alist of (SYMBOL . DEFAULT-VALUE) that `ews-mode' changed.")

(defvar ews--saved-modes nil
  "Alist of (GLOBAL-MODE . WAS-ON) that `ews-mode' toggled.")

(defvar ews--saved-themes nil
  "Themes enabled before `ews-mode' turned on.")

(defvar ews--saved-link-slant nil
  "Slant of `denote-faces-link' before `ews-mode' turned on.")

(defvar ews--evil-was-on nil
  "Non-nil when `ews-mode' turned `evil-mode' off.")

(defvar-local ews--buffer-modes nil
  "Alist of (BUFFER-MODE . WAS-ON) that `ews-mode' toggled in this buffer.")

(defun ews--set (&rest pairs)
  "Set the default value of each SYMBOL VALUE in PAIRS, remembering the old one."
  (while pairs
    (let ((sym (pop pairs))
          (val (pop pairs)))
      (unless (assq sym ews--saved-values)
        (push (cons sym (if (boundp sym) (default-value sym) 'ews--unbound))
              ews--saved-values))
      (set-default sym val))))

(defun ews--global-mode (mode arg)
  "Call global MODE with ARG, remembering whether it was on."
  (when (fboundp mode)
    (unless (assq mode ews--saved-modes)
      (push (cons mode (and (boundp mode) (symbol-value mode))) ews--saved-modes))
    (funcall mode arg)))

(defun ews--buffer-mode (mode arg)
  "Call buffer-local MODE with ARG, remembering whether it was on."
  (when (fboundp mode)
    (unless (assq mode ews--buffer-modes)
      (push (cons mode (and (boundp mode) (symbol-value mode)))
            ews--buffer-modes))
    (funcall mode arg)))

(defun ews--refresh-org-modern ()
  "Re-apply `org-modern' settings in the current buffer."
  (when (bound-and-true-p org-modern-mode)
    (org-modern-mode -1)
    (org-modern-mode 1)))

;;; Buffer setup

(defun ews--text-setup ()
  "EWS settings for a `text-mode' buffer."
  (ews--buffer-mode 'visual-line-mode 1)
  (ews--buffer-mode 'display-line-numbers-mode -1)
  ;; EWS spell-checks with flyspell; Doom's default is spell-fu.
  (ews--buffer-mode 'spell-fu-mode -1)
  (ews--buffer-mode 'flyspell-mode 1)
  (ews--buffer-mode 'writegood-mode 1)
  (ews--buffer-mode 'abbrev-mode 1))

(defun ews--org-setup ()
  "EWS settings for an `org-mode' buffer."
  ;; Buffer-local, because `my/org-mode-setup' resets the global value.
  (setq-local org-image-actual-width '(450))
  (ews--buffer-mode 'org-indent-mode 1)
  (ews--buffer-mode 'mixed-pitch-mode 1)
  (ews--buffer-mode 'org-appear-mode 1)
  (ews--buffer-mode 'org-fragtog-mode 1)
  (if (bound-and-true-p org-modern-mode)
      (ews--refresh-org-modern)
    (ews--buffer-mode 'org-modern-mode 1)))

(defun ews--dired-setup ()
  "EWS settings for a `dired-mode' buffer."
  (ews--buffer-mode 'denote-dired-mode 1)
  (ews--buffer-mode 'dired-omit-mode 1))

(defun ews--any-setup ()
  "EWS settings for every buffer: no traces of Doom's evil UI."
  ;; Doom's `~' markers after the end of the buffer.
  (ews--buffer-mode 'vi-tilde-fringe-mode -1)
  ;; Evil-snipe's local modes outlive `evil-mode'.
  (ews--buffer-mode 'evil-snipe-local-mode -1)
  (ews--buffer-mode 'evil-snipe-override-local-mode -1)
  (when (bound-and-true-p olivetti-mode)
    (ews--olivetti-mode-line)))

(defun ews--olivetti-mode-line ()
  "Hide the mode line while `olivetti-mode' is on."
  (ews--buffer-mode 'hide-mode-line-mode (if olivetti-mode 1 -1)))

(defconst ews--hooks
  '((after-change-major-mode-hook . ews--any-setup)
    (text-mode-hook  . ews--text-setup)
    (org-mode-hook   . ews--org-setup)
    (dired-mode-hook . ews--dired-setup)
    (olivetti-mode-hook . ews--olivetti-mode-line))
  "Hooks `ews-mode' adds while it is on.")

(defun ews--setup-buffer ()
  "Apply EWS settings to the current, already-open buffer."
  ;; Drop the cursor shape evil left behind.
  (kill-local-variable 'cursor-type)
  (ews--any-setup)
  (when (derived-mode-p 'text-mode) (ews--text-setup))
  (when (derived-mode-p 'org-mode) (ews--org-setup))
  (when (derived-mode-p 'dired-mode) (ews--dired-setup)))

(defun ews--teardown-buffer ()
  "Undo EWS settings in the current buffer."
  (pcase-dolist (`(,mode . ,was-on) ews--buffer-modes)
    (when (fboundp mode)
      (funcall mode (if was-on 1 -1))))
  (kill-local-variable 'ews--buffer-modes)
  (kill-local-variable 'org-image-actual-width))

;;; Enable and disable

(defconst ews--features
  '(org org-capture org-agenda ox ox-org ox-latex ox-epub oc oc-natbib oc-csl
    ob-dot bibtex citar citar-denote denote denote-org denote-journal
    denote-sequence consult consult-notes org-modern flyspell ispell
    doc-view ediff bookmark dired dired-x image-dired dictionary lorem-ipsum
    elfeed openwith)
  "Features loaded before `ews-mode' sets their options.
Setting an option before its package loads would stop the package's own
default from taking effect after the mode is turned off.")

(defconst ews--epub-mode '("\\.epub\\'" . nov-mode)
  "`auto-mode-alist' entry `ews-mode' adds.")

(defun ews--enable ()
  "Turn Emacs Writing Studio on."
  (when (bound-and-true-p evil-mode)
    (setq ews--evil-was-on t)
    (evil-mode -1))
  (dolist (feature ews--features)
    (require feature nil t))
  (require-theme 'modus-themes t)
  (setq ews-bibtex-files (ews--bibtex-files))

  ;; Look and feel
  (ews--set 'use-short-answers t
            'line-spacing 3
            'split-width-threshold 120
            'split-height-threshold nil
            'modus-themes-italic-constructs t
            'modus-themes-bold-constructs t
            'modus-themes-mixed-fonts t
            'modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  (setq ews--saved-themes custom-enabled-themes)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme ews-theme t)
  (ews--global-mode 'menu-bar-mode -1)
  (ews--global-mode 'tool-bar-mode -1)
  (ews--global-mode 'scroll-bar-mode -1)
  (ews--global-mode 'spacious-padding-mode 1)
  (ews--global-mode 'balanced-windows-mode 1)
  (ews--global-mode 'vertico-mode 1)
  (ews--global-mode 'savehist-mode 1)
  (ews--global-mode 'marginalia-mode 1)
  (ews--global-mode 'which-key-mode 1)
  (when (display-graphic-p)
    (ews--global-mode 'context-menu-mode 1))

  ;; Text
  (ews--set 'sentence-end-double-space nil
            'scroll-error-top-bottom t
            'save-interprogram-paste-before-kill t)
  (ews--global-mode 'delete-selection-mode 1)
  ;; Evil-escape stays on without evil and reads ahead after every key.
  (ews--global-mode 'evil-escape-mode -1)
  (ews--global-mode 'evil-snipe-override-mode -1)
  (ews--global-mode 'evil-snipe-mode -1)
  (when (executable-find "hunspell")
    (ews--set 'ispell-program-name "hunspell"
              'ispell-dictionary ews-hunspell-dictionaries)
    (ispell-set-spellchecker-params)
    (when (string-search "," ews-hunspell-dictionaries)
      (ispell-hunspell-add-multi-dic ews-hunspell-dictionaries)))
  (ews--set 'flyspell-mark-duplications-flag nil
            'lorem-ipsum-list-bullet "- "
            'lorem-ipsum-sentence-separator " "
            'dictionary-server "dict.org")

  ;; Org
  (ews--set 'org-startup-indented t
            'org-hide-emphasis-markers t
            'org-startup-with-inline-images t
            'org-pretty-entities t
            'org-use-sub-superscripts "{}"
            'org-id-link-to-org-use-id t
            'org-fold-catch-invisible-edits 'show
            'org-startup-with-latex-preview nil
            'org-format-latex-options
            (thread-first (copy-sequence org-format-latex-options)
                          (plist-put :scale 2)
                          (plist-put :foreground 'auto)
                          (plist-put :background 'auto))
            ;; Most org-modern features are off in EWS.
            'org-modern-table nil
            'org-modern-keyword nil
            'org-modern-timestamp nil
            'org-modern-priority nil
            'org-modern-checkbox nil
            'org-modern-tag nil
            'org-modern-block-name nil
            'org-modern-footnote nil
            'org-modern-internal-target nil
            'org-modern-radio-target nil
            'org-modern-statistics nil
            'org-modern-progress nil
            'org-capture-templates
            '(("f" "Fleeting note" item
               (file+headline org-default-notes-file "Notes")
               "- %?")
              ("p" "Permanent note" plain
               (file denote-last-path)
               #'denote-org-capture
               :no-save t
               :immediate-finish nil
               :kill-buffer t
               :jump-to-captured t)
              ("t" "New task" entry
               (file+headline org-default-notes-file "Tasks")
               "* TODO %i%?"))
            'org-agenda-custom-commands
            '(("e" "Agenda, next actions and waiting"
               ((agenda "" ((org-agenda-overriding-header "Next three days:")
                            (org-agenda-span 3)
                            (org-agenda-start-on-weekday nil)))
                (todo "NEXT" ((org-agenda-overriding-header "Next Actions:")))
                (todo "WAIT" ((org-agenda-overriding-header "Waiting:")))))))

  ;; Inspiration
  (ews--set 'doc-view-resolution 300
            'large-file-warning-threshold (* 50 (expt 2 20))
            'elfeed-show-entry-switch #'display-buffer
            'openwith-associations nil)
  (ews--global-mode 'openwith-mode 1)
  (add-to-list 'auto-mode-alist ews--epub-mode)

  ;; Bibliographies
  (ews--set 'bibtex-user-optional-fields
            '(("keywords" "Keywords to describe the entry" "")
              ("file"     "Relative or absolute path to attachments" ""))
            'bibtex-align-at-equal-sign t
            'citar-bibliography ews-bibtex-files
            'citar-open-always-create-notes t
            'org-cite-global-bibliography ews-bibtex-files
            'org-cite-insert-processor 'citar
            'org-cite-follow-processor 'citar
            'org-cite-activate-processor 'citar)

  ;; Denote
  (ews--set 'denote-sort-keywords t
            'denote-link-description-function
            #'ews-denote-link-description-title-case
            'consult-notes-denote-display-keywords-indicator "_"
            'consult-preview-allowed-hooks
            (cons 'visual-line-mode (bound-and-true-p consult-preview-allowed-hooks)))
  (ews--global-mode 'denote-rename-buffer-mode 1)
  (ews--global-mode 'consult-notes-denote-mode 1)
  (ews--global-mode 'citar-denote-mode 1)
  (when (facep 'denote-faces-link)
    (setq ews--saved-link-slant (face-attribute 'denote-faces-link :slant))
    (set-face-attribute 'denote-faces-link nil :slant 'italic))

  ;; Publication
  (ews--set 'org-export-with-drawers nil
            'org-export-with-todo-keywords nil
            'org-export-with-toc nil
            'org-export-with-smart-quotes t
            'org-export-date-timestamp-format "%e %B %Y"
            'org-latex-pdf-process
            '("pdflatex -interaction nonstopmode -output-directory %o %f"
              "bibtex %b"
              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
            'org-latex-logfiles-extensions
            '("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm"
              "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl"
              "bbl" "tex" "bcf")
            'org-latex-classes
            (cons '("ews"
                    "\\documentclass[11pt, twoside, hidelinks]{memoir}
        \\setstocksize{9.25in}{7.5in}
        \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
        \\setlrmarginsandblock{1.5in}{1in}{*}
        \\setulmarginsandblock{1in}{1.5in}{*}
        \\checkandfixthelayout
        \\layout
        \\setcounter{tocdepth}{0}
        \\renewcommand{\\baselinestretch}{1.25}
        \\setheadfoot{0.5in}{0.75in}
        \\setlength{\\footskip}{0.8in}
        \\chapterstyle{bianchi}
        \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
        \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
        \\setsubsubsecheadstyle{\\normalfont\\centering}
        \\pagestyle{myheadings}
        \\usepackage[font={small, it}]{caption}
        \\usepackage{ccicons}
        \\usepackage{ebgaramond}
        \\usepackage[authoryear]{natbib}
        \\bibliographystyle{apalike}
        \\usepackage{svg}
\\hyphenation{mini-buffer}"
                    ("\\chapter{%s}" . "\\chapter*{%s}")
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                  (assoc-delete-all "ews" (copy-sequence org-latex-classes)))
            'ediff-keep-variants nil
            'ediff-split-window-function #'split-window-horizontally
            'ediff-window-setup-function #'ediff-setup-windows-plain)
  (ews--set 'org-babel-load-languages
            (cons '(dot . t) (assq-delete-all 'dot (copy-sequence org-babel-load-languages))))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

  ;; Files
  (ews--set 'delete-by-moving-to-trash t
            'dired-dwim-target t
            'dired-omit-files "^\\.[a-zA-Z0-9]+"
            'bookmark-save-flag 1
            'image-dired-external-viewer "gimp")
  ;; --group-directories-first needs GNU ls; Doom uses gls on macOS if found.
  (when (string-match-p "gls\\'" (or insert-directory-program ""))
    (ews--set 'dired-listing-switches
              "-goah --group-directories-first --time-style=long-iso"))
  (ews--global-mode 'recentf-mode 1)

  (pcase-dolist (`(,hook . ,fn) ews--hooks)
    (add-hook hook fn 90))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (ews--setup-buffer))))

(defun ews--disable ()
  "Turn Emacs Writing Studio off and restore what it changed."
  (pcase-dolist (`(,hook . ,fn) ews--hooks)
    (remove-hook hook fn))
  (pcase-dolist (`(,sym . ,val) ews--saved-values)
    (if (eq val 'ews--unbound)
        (makunbound sym)
      (set-default sym val)))
  (setq ews--saved-values nil)
  (setq auto-mode-alist (delete ews--epub-mode auto-mode-alist))
  (when (featurep 'ispell)
    (ispell-kill-ispell t)
    (ispell-set-spellchecker-params))
  (pcase-dolist (`(,mode . ,was-on) ews--saved-modes)
    (funcall mode (if was-on 1 -1)))
  (setq ews--saved-modes nil)
  (when (and ews--saved-link-slant (facep 'denote-faces-link))
    (set-face-attribute 'denote-faces-link nil :slant ews--saved-link-slant)
    (setq ews--saved-link-slant nil))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when ews--buffer-modes
        (ews--teardown-buffer))
      (when (derived-mode-p 'org-mode)
        (ews--refresh-org-modern))))
  (mapc #'disable-theme custom-enabled-themes)
  (dolist (theme (reverse ews--saved-themes))
    (load-theme theme t))
  (setq ews--saved-themes nil)
  (when ews--evil-was-on
    (setq ews--evil-was-on nil)
    (evil-mode 1)))

(defvar ews-mode nil
  "Non-nil if Emacs Writing Studio is on.
A plain variable, not a user option: an interactive toggle of a
customizable global mode records it in the `user' theme, and every
`load-theme' then re-runs the mode, which loads a theme again.")

(defvar ews--toggling nil
  "Non-nil while `ews-mode' is turning on or off.")

;;;###autoload
(define-minor-mode ews-mode
  "Emacs Writing Studio: stock Emacs keys and EWS's writing setup.

Turns evil off and binds EWS's keys under \\`C-c w'.  Turning the mode off
restores Doom's settings, modes and theme and turns evil back on.

\\{ews-mode-map}"
  :global t
  :variable ews-mode
  :lighter " EWS"
  :keymap ews-mode-map
  :group 'ews
  (cond
   (ews--toggling)
   (ews-mode
    (let ((ews--toggling t))
      (condition-case err
          (progn (ews--enable)
                 (message "Emacs Writing Studio on; C-c w q returns to Doom"))
        (error
         (setq ews-mode nil)
         (ews--disable)
         (signal (car err) (cdr err))))))
   (t
    (let ((ews--toggling t))
      (ews--disable))
    (message "Emacs Writing Studio off"))))

(provide 'ews)
;;; ews.el ends here
