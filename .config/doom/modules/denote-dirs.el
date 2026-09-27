;;; denote-dirs.el --- Several Denote directories  -*- lexical-binding: t; -*-

(require 'seq)

(defvar my/notes-directory)
(defvar denote-directory)
(defvar my/extra-notes-directories nil
  "Directories searched and linked by Denote after `my/notes-directory'.")

(defun my/denote-directory-value ()
  "Return the value for `denote-directory'.
A string when there are no extra directories, else a list whose first
element, `my/notes-directory', is where new notes go."
  (if my/extra-notes-directories
      (cons my/notes-directory my/extra-notes-directories)
    my/notes-directory))

(defun my/denote-directory-containing (file dirs)
  "Return the element of DIRS that contains FILE, else the first one."
  (let ((file (expand-file-name file))
        (dirs (mapcar (lambda (d) (file-name-as-directory (expand-file-name d))) dirs)))
    (or (seq-find (lambda (d) (string-prefix-p d file)) dirs)
        (car dirs))))

(defun my/file-relative-name-denote-list (args)
  "Resolve a list DIRECTORY in ARGS for `file-relative-name'."
  (pcase-let ((`(,file ,dir) args))
    (if (consp dir)
        (list file (my/denote-directory-containing file dir))
      args)))

(defun my/consult-notes-with-denote-list (fn &rest args)
  "Call FN with ARGS while `file-relative-name' accepts a list directory."
  (advice-add 'file-relative-name :filter-args #'my/file-relative-name-denote-list)
  (unwind-protect
      (apply fn args)
    (advice-remove 'file-relative-name #'my/file-relative-name-denote-list)))

(defun my/consult-notes-new-note-in-first-directory (fn &rest args)
  "Call FN with ARGS with `denote-directory' bound to its first directory."
  (let ((denote-directory (car (denote-directories))))
    (apply fn args)))

(defvar consult-notes-file-dir-sources)
(defvar consult-notes-denote-mode)

(defun my/consult-notes-search-denote-directories (fn &rest args)
  "Call FN with ARGS searching every Denote directory as a file source."
  (if (listp denote-directory)
      (let ((consult-notes-file-dir-sources
             (append consult-notes-file-dir-sources
                     (mapcar (lambda (d) (list "Denote" ?d (directory-file-name d)))
                             (denote-directories))))
            (consult-notes-denote-mode nil))
        (apply fn args))
    (apply fn args)))

(with-eval-after-load 'consult-notes
  (advice-add 'consult-notes :around #'my/consult-notes-with-denote-list)
  (advice-add 'consult-notes-search-in-all-notes :around #'my/consult-notes-search-denote-directories))

(with-eval-after-load 'consult-notes-denote
  (advice-add 'consult-notes-denote--new-note :around #'my/consult-notes-new-note-in-first-directory))

(provide 'denote-dirs)
;;; denote-dirs.el ends here
