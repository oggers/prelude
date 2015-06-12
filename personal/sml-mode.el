;;; sml-mode.el --- Config for smart-mode-line

;;; Commentary:

;;; Code:
(if (window-system)
    (progn
      (require 'smart-mode-line)
      (sml/setup)

      (sml/apply-theme 'powerline)))

(provide 'sml-mode)
;;; sml-mode.el ends here
