;;; isbn-to-bibtex.el --- Simple ISBN → BibTeX helper using OpenLibrary -*- lexical-binding: t; -*-

(require 'url)
(require 'json)
(require 'bibtex)

(defvar my/isbn-openlibrary-url
  "https://openlibrary.org/api/books?bibkeys=ISBN:%s&jscmd=data&format=json"
  "Format string for OpenLibrary Books API URL.  %s is the ISBN.") ;; :contentReference[oaicite:1]{index=1}

(defun my/isbn--alist-get (key alist)
  "Get KEY from ALIST where keys are strings."
  (alist-get key alist nil nil #'string=))

(defun my/isbn--fetch-metadata (isbn)
  "Fetch metadata for ISBN from OpenLibrary, return an alist.
Signals an error if nothing is found."
  (let* ((url (format my/isbn-openlibrary-url isbn))
         ;; timeout 10 seconds, inhibit-cookies t, no-redirect t
         (buf (url-retrieve-synchronously url t t 10)))
    (unless buf
      (error "Failed to retrieve data from OpenLibrary"))
    (unwind-protect
        (with-current-buffer buf
          ;; Skip HTTP headers
          (goto-char (point-min))
          (re-search-forward "\n\n" nil 'move)
          (let* ((json (json-parse-buffer
                        :object-type 'alist
                        :array-type 'list
                        :null-object nil
                        :false-object nil))
                 (entry (my/isbn--alist-get (format "ISBN:%s" isbn) json)))
            (unless entry
              (error "No data found for ISBN %s" isbn))
            entry))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun my/isbn--extract-year (publish-date)
  "Extract a 4-digit year from PUBLISH-DATE string, or nil."
  (when (and publish-date
             (string-match "\\([0-9][0-9][0-9][0-9]\\)" publish-date))
    (match-string 1 publish-date)))

(defun my/isbn--join-names (items)
  "Join the \"name\" field from a list of OpenLibrary ITEMS with “ and ”."
  (when items
    (mapconcat (lambda (it)
                 (or (my/isbn--alist-get "name" it) ""))
               items
               " and ")))

(defun my/isbn--slug (s)
  "Return a lowercase slug of S with non-letters stripped."
  (let ((s (or s "")))
    (downcase (replace-regexp-in-string "[^[:alpha:]]" "" s))))

(defun my/isbn--make-key (authors year title)
  "Construct a simple citation key from AUTHORS, YEAR, TITLE."
  (let* ((first-author (car (and authors (split-string authors " and "))))
         (surname (car (last (and first-author (split-string first-author " ")))))
         (first-word (car (and title (split-string title " "))))
         (surname-slug (my/isbn--slug surname))
         (title-slug (my/isbn--slug first-word))
         (year (or year "nd")))
    (format "%s%s%s" surname-slug year title-slug)))

(defun my/isbn--format-bibtex-book (key isbn meta)
  "Format a @book BibTeX entry from META (OpenLibrary alist)."
  (let* ((title (my/isbn--alist-get "title" meta))
         (subtitle (my/isbn--alist-get "subtitle" meta))
         (full-title (if (and subtitle (not (string-empty-p subtitle)))
                         (format "%s: %s" title subtitle)
                       title))
         (authors (my/isbn--join-names (my/isbn--alist-get "authors" meta)))
         (publishers (my/isbn--join-names (my/isbn--alist-get "publishers" meta)))
         (publish-date (my/isbn--alist-get "publish_date" meta))
         (year (my/isbn--extract-year publish-date))
         (url (my/isbn--alist-get "url" meta)))
    (setq year (or year ""))
    (format "@book{%s,\n  author    = {%s},\n  title     = {%s},\n  publisher = {%s},\n  year      = {%s},\n  isbn      = {%s},\n  url       = {%s},\n}\n"
            key (or authors "") (or full-title "") (or publishers "") year isbn (or url ""))))

;;;###autoload
(defun my/isbn-add-book-to-bib (isbn)
  "Fetch metadata for ISBN and append a @book entry to the .bib file specified in `citar-bibliography`.

When called interactively, prompt for ISBN and use `citar-bibliography` for the .bib file."
  (interactive
   (list (read-string "ISBN (10 or 13 digits): " (thing-at-point 'word t))))
  (let* ((bibfile (expand-file-name (car citar-bibliography)))  ;; Use the first path in `citar-bibliography`
         (meta (my/isbn--fetch-metadata isbn))
         (title (my/isbn--alist-get "title" meta))
         (authors (my/isbn--join-names (my/isbn--alist-get "authors" meta)))
         (year (my/isbn--extract-year (my/isbn--alist-get "publish_date" meta)))
         (key (my/isbn--make-key authors year title))
         (bibtex-entry (my/isbn--format-bibtex-book key isbn meta)))
    (with-current-buffer (find-file-noselect bibfile)
      (bibtex-mode)
      ;; Check if ISBN already present
      (goto-char (point-min))
      (if (re-search-forward (regexp-quote isbn) nil t)
          (message "ISBN %s already exists in %s" isbn bibfile)
        ;; Check if key already present
        (goto-char (point-min))
        (if (re-search-forward (format "@[A-Za-z]+{%s," (regexp-quote key)) nil t)
            (message "Key %s already exists in %s" key bibfile)
          ;; Insert at end
          (goto-char (point-max))
          (unless (bolp) (insert "\n\n"))
          (insert bibtex-entry)
          ;; Optionally clean/format the entry
          (bibtex-beginning-of-entry)
          (bibtex-fill-entry)
          (save-buffer)
          (message "Added @book{%s} to %s" key bibfile))))))

(provide 'isbn-to-bibtex)
;;; isbn-to-bibtex.el ends here
