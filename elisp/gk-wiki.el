;;; Gkroam  to Gkwiki
(defun gkroam-file-content (title)
  (let* ((page (gkroam--get-page title))
         (file (when page (gkroam--get-file page))))
    (if file
        (with-current-buffer (find-file-noselect file)
          (save-restriction
            (gkroam--narrow-to-content)
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "#\\+TITLE:.+" nil t)
                (string-trim (buffer-substring-no-properties (point) (point-max)))))))
      (user-error "No such gkroam page: %s" title))))

(defun gkroam-to-gkwiki (roam-title wiki-title)
  (let ((roam-content (gkroam-file-content roam-title))
        (wiki-file (md-wiki-page-file wiki-title)))
    (with-file-buffer wiki-file
      (let (beg (end (point-max)))
        (if (re-search-forward (concat "^# " roam-title) nil t)
            (progn
              (setq beg (line-beginning-position))
              (save-excursion
                (when (re-search-forward "^# .+" nil t)
                  (setq end (line-beginning-position))))
              (delete-region beg end)
              (insert "# " roam-title "\n" roam-content "\n\n"))
          (when (re-search-forward "^---.*" nil t 4)
            (insert "\n# " roam-title "\n" roam-content "\n")))))))

(defun daily-page-to-mdwiki (&optional date)
  (interactive)
  (let ((title (or date (format-time-string "%b %d, %Y"))))
    (gkroam-to-gkwiki title "Daily Page")))


;;; FIIXME
;; 1. force to regenerate all pages, from a start date
;; 2. generate a specific date with by choosing in calendar.

;; (daily-page-to-mdwiki "Aug 26, 2022")
