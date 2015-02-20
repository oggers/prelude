;;; god.el --- Summary
;;; This is a global minor mode for entering Emacs commands without
;;; modifier keys. It's similar to Vim's separation of commands and
;;; insertion mode.

;;; Commentary:
;; Mapping

;; This library defines the following mapping:

;; All commands are assumed to be C-<something> unless otherwise indicated.
;; Examples:
;; a → C-a
;; s → C-s
;; akny → C-a C-k C-n C-y
;; xs → C-x C-s
;; x s → C-x s

;; Note the use of space to produce C-x s.

;; g is a special key to indicate M-<something>.  This means that there
;; is no way to write C-g in this mode, you must therefore type C-g directly.
;; Examples:
;; gf → M-f
;; gx → M-x

;; G is a special key to indicate C-M-<something>.
;; Example:
;; Gx → C-M-x

;; Digit arguments:
;; 12f → M-12 C-f

;; Repetition (with . keybinding):
;; gf.. → M-f M-f M-f

;; Universal boolean argument:
;; uco → C-u C-c C-o

;;; Code:
(require 'god-mode)

; toggle between God mode and non-God mode using ESC
(global-set-key (kbd "<escape>") 'god-mode-all)
;;(global-set-key (kbd "<escape>") 'god-local-mode)

; Cursor style to indicate mode-line
(defun ogg/update-cursor ()
  "Update cursor."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(add-hook 'god-mode-enabled-hook 'ogg/update-cursor)
(add-hook 'god-mode-disabled-hook 'ogg/update-cursor)
(set-default 'cursor-type 'bar)

;; Change modeline color
(defvar ogg/mode-line-background)
(defvar ogg/mode-line-inactive-background)
(defun ogg/god-mode-update-cursor ()
  "Change modeline depending on god-mode is enabled."
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode
           (progn
             (unless (boundp 'ogg/mode-line-background)
               (progn (setq ogg/mode-line-background (face-background 'mode-line))
                      (setq ogg/mode-line-inactive-background (face-background 'mode-line-inactive))))
             (set-face-background 'mode-line (if limited-colors-p "white" "Slateblue4"))
             (set-face-background 'mode-line-inactive (if limited-colors-p "white" "SlateBlue3"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" ogg/mode-line-background))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" ogg/mode-line-inactive-background)))))))
(add-hook 'god-mode-enabled-hook 'ogg/god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'ogg/god-mode-update-cursor)

; handy keybindings
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(provide 'god)
;;; god.el ends here
