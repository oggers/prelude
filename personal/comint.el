;;; comint.el --- Personal settings for comint
;;; Commentary:

;;; Code:
(defvar comint-password-prompt-regexp)
(setq comint-password-prompt-regexp

      (concat
       "\\("
       "^ *"
       "\\|"
       "\\( SMB"
       "\\|"
       "'s"
       "\\|"
       "Bad"
       "\\|"
       "CVS"
       "\\|"
       "Enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?"
       "\\|"
       "Kerberos"
       "\\|"
       "LDAP"
       "\\|"
       "New"
       "\\|"
       "Old"
       "\\|"
       "Repeat"
       "\\|"
       "UNIX"
       "\\|"
       "\\[sudo]"
       "\\|"
       "\\[SUDO]"  ;; '[SUDO]' prompt for ansible-playbook --ask-become-pass
       "\\|"
       "SUDO"
       "\\|"
       "SSH"  ;; 'SSH password:' prompt for ansible-playbook --ask-pass
       "\\|"
       "enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?"
       "\\|"
       "login"
       "\\|"
       "new"
       "\\|"
       "old\\) +\\)"
       "\\("
       "?:Pass\\(?: phrase\\|phrase\\|word\\)"
       "\\|"
       "Response"
       "\\|"
       "pass\\(?: phrase\\|phrase\\|word\\)\\)"
       "\\("
       "?:\\(?:, try\\)? *again"
       "\\|"
       " (empty for no passphrase)"
       "\\|"
       " (again)"
       "\\|"
       "\\[defaults to SSH password]" ;; 'SUDO password[defaults to SSH password]:' prompt for ansible-playbook --ask-become-pass
       "\\)?\\(?: for [^:]+\\)?:\\s *\\'"  ))

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
)

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

; make completion buffers disappear after 3 seconds.
(add-hook 'completion-setup-hook
          (lambda () (run-at-time 3 nil
                                  (lambda () (delete-windows-on "*Completions*")))))

(provide 'custom)
;;; comint.el ends here
