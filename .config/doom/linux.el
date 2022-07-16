(defun copy-to-clipboard ()
  "Copies selection to x-clipboard"
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  "Pastes from x-clipboard"
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )

(map! :leader
      :desc "copy-to-clipboard"
      "o y" #'copy-to-clipboard)

(map! :leader
      :desc "paste-from-clipboard"
      "o p" #'paste-from-clipboard)
