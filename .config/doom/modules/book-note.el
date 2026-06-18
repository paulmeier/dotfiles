;;; book-note.el --- Create a Denote book note from an ISBN -*- lexical-binding: t; -*-
;;
;; `M-x my/denote-book-note' (bound to SPC e t):
;;   1. Prompts for an ISBN.
;;   2. Looks the book up on OpenLibrary.
;;   3. Appends a @book entry to the first file in `citar-bibliography'
;;      (skipped if an entry with the same ISBN or citekey already exists).
;;   4. Downloads the cover to <denote-directory>/<subdir>/covers/<isbn>.jpg.
;;   5. Prompts for a template (e.g. fiction vs non-fiction) from
;;      `my/book-note-template-keys' — these must be keys in `denote-templates'.
;;   6. Calls `denote' to create the note in `my/book-note-subdir'.
;;   7. Adds a citar-denote `#+reference:' line so `citar-denote' commands
;;      treat the new note as the bibliographic note for that citekey.
;;   8. Inserts the cover image after the front matter.
;;
;; Depends on `isbn-to-bibtex' for the OpenLibrary fetch helpers.

;;; Code:

(require 'url)
(require 'denote)
(require 'isbn-to-bibtex)

(defgroup my/book-note nil
  "Create Denote book notes from an ISBN."
  :group 'denote)

(defcustom my/book-note-subdir "literature"
  "Subdirectory of `denote-directory' where book notes are created."
  :type 'string
  :group 'my/book-note)

(defcustom my/book-note-keywords '("book")
  "Default Denote keywords for book notes.
`citar-denote-keyword' (typically \"bib\") is appended automatically
so citar-denote recognizes the note."
  :type '(repeat string)
  :group 'my/book-note)

(defcustom my/book-note-template-keys '(non-fiction-book fiction-book)
  "Keys eligible as templates for new book notes.
Each must exist in `denote-templates'."
  :type '(repeat symbol)
  :group 'my/book-note)

(defcustom my/book-note-cover-url-format
  "https://covers.openlibrary.org/b/isbn/%s-L.jpg"
  "Format string for the OpenLibrary cover URL.  %s is replaced with the ISBN."
  :type 'string
  :group 'my/book-note)

;; -------------------------------------------------------------------- helpers

(defun my/book-note--covers-dir ()
  "Return the absolute path to the covers directory, creating it if needed."
  (let* ((lit (expand-file-name my/book-note-subdir denote-directory))
         (dir (expand-file-name "covers" lit)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun my/book-note--download-cover (isbn)
  "Download the OpenLibrary cover for ISBN.
Return the absolute path on success, or nil if no cover was returned.
OpenLibrary returns a 1×1 placeholder when no cover exists, so we
delete files that come back implausibly small."
  (let* ((path (expand-file-name (format "%s.jpg" isbn) (my/book-note--covers-dir)))
         (url  (format my/book-note-cover-url-format isbn)))
    (condition-case err
        (progn
          (url-copy-file url path t)
          (let ((size (file-attribute-size (file-attributes path))))
            (cond
             ((null size)
              (delete-file path) nil)
             ((< size 1024)            ; placeholder / 404 image
              (delete-file path) nil)
             (t path))))
      (error
       (message "Cover download failed: %S" err)
       nil))))

(defun my/book-note--ensure-bib-entry (isbn meta)
  "Append a @book entry for ISBN to the first file in `citar-bibliography'.
META is the OpenLibrary alist returned by `my/isbn--fetch-metadata'.
If an entry with the same ISBN or citekey already exists, reuse it.
Return the citekey to use."
  (let* ((bibfile (expand-file-name (car citar-bibliography)))
         (title   (my/isbn--alist-get "title" meta))
         (authors (my/isbn--join-names (my/isbn--alist-get "authors" meta)))
         (year    (my/isbn--extract-year (my/isbn--alist-get "publish_date" meta)))
         (key     (my/isbn--make-key authors year title)))
    (with-current-buffer (find-file-noselect bibfile)
      (bibtex-mode)
      (or
       ;; Existing entry by ISBN — pull its key.
       (save-excursion
         (goto-char (point-min))
         (when (re-search-forward
                (format "isbn[[:space:]]*=[[:space:]]*[{\"]%s[}\"]"
                        (regexp-quote isbn))
                nil t)
           (bibtex-beginning-of-entry)
           (when (re-search-forward "@[A-Za-z]+{\\([^,]+\\)," nil t)
             (message "Reusing existing bib entry for ISBN %s" isbn)
             (match-string 1))))
       ;; Existing entry with the same citekey — reuse.
       (save-excursion
         (goto-char (point-min))
         (when (re-search-forward
                (format "@[A-Za-z]+{%s," (regexp-quote key)) nil t)
           (message "Reusing existing bib entry %s" key)
           key))
       ;; Otherwise insert a new one.
       (progn
         (goto-char (point-max))
         (unless (bolp) (insert "\n\n"))
         (insert (my/isbn--format-bibtex-book key isbn meta))
         (bibtex-beginning-of-entry)
         (bibtex-fill-entry)
         (save-buffer)
         (message "Added @book{%s} to %s" key bibfile)
         key)))))

(defun my/book-note--prompt-template ()
  "Prompt for a book template key from `my/book-note-template-keys'."
  (let ((choices (mapcar #'symbol-name my/book-note-template-keys)))
    (intern (completing-read "Book template: " choices nil t nil nil
                             (car choices)))))

(defun my/book-note--reference-format ()
  "Reference-line format string for citar-denote, falling back to a sane default."
  (or (and (boundp 'citar-denote-file-types)
           (plist-get (alist-get 'org citar-denote-file-types) :reference-format))
      "#+reference:  %s\n"))

(defun my/book-note--add-reference (citekey)
  "Insert a citar-denote `#+reference:' line for CITEKEY into the front matter."
  (save-excursion
    (goto-char (point-min))
    (let ((last-front-matter-line (point-min)))
      (while (looking-at "^#\\+")
        (setq last-front-matter-line (line-end-position))
        (forward-line 1))
      (goto-char last-front-matter-line)
      (insert "\n" (format (my/book-note--reference-format) citekey)))))

(defun my/book-note--insert-after-front-matter (text)
  "Insert TEXT just after the Org front matter in the current buffer."
  (save-excursion
    (goto-char (point-min))
    ;; Walk past consecutive `#+key: ...' lines and following blank lines.
    (while (looking-at "^#\\+") (forward-line 1))
    (while (looking-at "^[[:space:]]*$") (forward-line 1))
    (insert text)))

(defun my/book-note--validate-isbn (isbn)
  "Strip dashes/whitespace from ISBN and validate length. Return cleaned string."
  (let ((clean (replace-regexp-in-string "[^0-9Xx]" "" isbn)))
    (unless (or (= (length clean) 10) (= (length clean) 13))
      (user-error "ISBN must be 10 or 13 digits (got %d): %s" (length clean) isbn))
    clean))

;; ---------------------------------------------------------------- entry point

;;;###autoload
(defun my/denote-book-note (isbn)
  "Create a Denote book note from ISBN.

Fetches metadata from OpenLibrary, ensures a @book entry exists in
`citar-bibliography', downloads the cover image, then creates a new
Denote note under `my/book-note-subdir' that references the bib entry
and embeds the cover.

Prompts for which template to use (see `my/book-note-template-keys')."
  (interactive
   (list (read-string "ISBN: " (thing-at-point 'word t))))
  (unless (and (boundp 'citar-bibliography) citar-bibliography)
    (user-error "`citar-bibliography' is not configured"))
  (unless (and (boundp 'denote-directory) denote-directory)
    (user-error "`denote-directory' is not configured"))
  (let* ((isbn       (my/book-note--validate-isbn isbn))
         (meta       (my/isbn--fetch-metadata isbn))
         (title      (my/isbn--alist-get "title" meta))
         (subtitle   (my/isbn--alist-get "subtitle" meta))
         (full-title (if (and subtitle (not (string-empty-p subtitle)))
                         (format "%s: %s" title subtitle)
                       title))
         (citekey    (my/book-note--ensure-bib-entry isbn meta))
         (cover-path (my/book-note--download-cover isbn))
         (template   (my/book-note--prompt-template))
         (lit-dir    (file-name-as-directory
                      (expand-file-name my/book-note-subdir denote-directory)))
         (bib-kw     (and (boundp 'citar-denote-keyword) citar-denote-keyword))
         (keywords   (delete-dups
                      (append my/book-note-keywords
                              (and bib-kw (list bib-kw)))))
         (note-path  (denote full-title keywords 'org lit-dir nil template)))
    (with-current-buffer (find-file-noselect note-path)
      (my/book-note--add-reference citekey)
      (when cover-path
        (let* ((rel (file-relative-name cover-path (file-name-directory note-path)))
               (img (format "#+caption: Cover\n#+attr_org: :width 300\n[[file:%s]]\n\n"
                            rel)))
          (my/book-note--insert-after-front-matter img)))
      (save-buffer)
      (when (called-interactively-p 'any)
        (pop-to-buffer (current-buffer))))
    note-path))

(provide 'book-note)
;;; book-note.el ends here
