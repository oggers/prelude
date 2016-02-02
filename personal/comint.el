;;; comint.el --- Personal settings for comint
;;; Commentary:

;;; Code:
(defvar comint-password-prompt-regexp)
(setq comint-password-prompt-regexp
      ;; Add [SUDO] in order to hide password prompt for docker
      "\\(^ *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|Kerberos\\|LDAP\\|New\\|Old\\|Repeat\\|UNIX\\|\\[sudo]\\|[SUDO]\\|enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|login\\|new\\|old\\) +\\)\\(?:Pass\\(?: phrase\\|phrase\\|word\\)\\|Response\\|pass\\(?: phrase\\|phrase\\|word\\)\\)\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?\\(?: for [^:]+\\)?:\\s *\\'"
      )
(provide 'custom)
;;; comint.el ends here
