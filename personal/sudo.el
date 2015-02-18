(defun su ()
  "Reopen current file as root."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/su::" buffer-file-name))))

(defun sudo ()
  "Reopen current file as sudoer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root."
  :group 'basic-faces)

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (let* ((warning "WARNING: EDITING FILE AS ROOT!")
           (space (+ 6 (- (window-width) (length warning))))
           (bracket (make-string (/ space 2) ?-))
           (warning (concat bracket warning bracket)))
      (setq header-line-format
            (propertize  warning 'face 'find-file-root-header-face)))))

(defun find-alternative-file-with-sudo ()
  "Open file as superuser."
  (interactive)
  (let ((bname (expand-file-name (or buffer-file-name
                                     default-directory)))
        (pt (point)))
    (setq bname (or (file-remote-p bname 'localname)
                    (concat "/sudo::" bname)))
      ;; FIXME mostly works around, but not quite
      (cl-flet ((server-buffer-done
              (buffer &optional for-killing)
              nil))
        (find-alternate-file bname))
      (goto-char pt)))

;; normally this is bound to find-file-read-only
;; use M-x toggle-read-only instead
(global-set-key (kbd "C-x M-s") 'find-alternative-file-with-sudo)

(add-hook 'find-file-hook 'find-file-root-header-warning)
(add-hook 'dired-mode-hook 'find-file-root-header-warning)
