;;; my-book-cover.el --- Insert book cover from ISBN -*- lexical-binding: t; -*-

(require 'url)
(require 'org)

(defun my/insert-book-cover (&optional isbn)
  "Fetch a book cover image from Open Library using ISBN and insert it into the Org buffer.

If ISBN is provided, use that. Otherwise:
- Try to read ISBN from current heading's :ISBN: property
- If not found, prompt the user

The cover is saved to a .attach directory beside the org file
and an Org link + caption is inserted at point."
  (interactive)
  (let* ((isbn (or isbn
                   (org-entry-get nil "ISBN" t)
                   (read-string "ISBN: ")))
         ;; Validate
         (_ (unless (string-match-p "[0-9Xx]" isbn)
              (user-error "Invalid ISBN: %s" isbn)))

         (buf-file (buffer-file-name))
         (base-dir (if buf-file
                       (file-name-directory buf-file)
                     default-directory))

         (attach-dir (expand-file-name ".attach" base-dir))
         (img-file-name (format "%s-cover.jpg" isbn))
         (img-path (expand-file-name img-file-name attach-dir))

         ;; Open Library cover URL
         (cover-url (format "https://covers.openlibrary.org/b/isbn/%s-L.jpg" isbn)))

    ;; Ensure .attach exists
    (unless (file-directory-p attach-dir)
      (make-directory attach-dir t))

    ;; Download image
    (message "Downloading cover from %s..." cover-url)
    (url-copy-file cover-url img-path t) ;; overwrite = t

    ;; Insert link relative to file
    (let ((rel-path (file-relative-name img-path base-dir)))
      (insert (format "#+CAPTION: Cover for ISBN %s\n" isbn))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (format "[[file:%s]]\n" rel-path))
      (message "Inserted cover image: %s" rel-path))))

(provide 'my-book-cover)

;;; my-book-cover.el ends here
