(when (file-exists-p "~/Dropbox/org")
  (setq org-directory "~/Dropbox/org")

  (setq org-agenda-start-on-weekday 1)

  (defun gtd()
    (interactive)
    (find-file "~/Dropbox/org/gtd.org"))

  ;; put all log into a drawer
  (setq org-log-into-drawer "LOGBOOK")

  ;; todos
  (setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)" "PHONE")
              (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))
  (setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("OPEN" :foreground "blue" :weight bold)
              ("CLOSED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

  ; Tags with fast selection keys
  (setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            (:endgroup)
                            ("PHONE" . ?p))))

  ;; Capture
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
      '(("t" "Tarea" entry (file+headline "" "Tareas")
         "* TODO %?\n  %i\n  Creado el %U")
        ("p" "Proyecto" entry (file+headline "" "Proyectos")
         "* %?\nCreado el %U\n  %i\n  %a")))

  ;; Agenda
  ;(setq org-agenda-files (quote ("~/Dropbox/org")))
  ;(setq org-agenda-files (file-expand-wildcards "~/Dropbox/org/*.org"))
  (setq org-agenda-files (list "~/Dropbox/org/gtd.org"
                 "~/Dropbox/org/from-mobile.org"
                 "~/Dropbox/org/finances.org"
                 "~/Dropbox/org/notes.org"
                 "~/Dropbox/org/someday.org"))
  ;; Default priority. [#A] -> 65, [#B] -> 66, [#C] -> 67
  (setq org-default-priority 68)
  ;; Always highlight the current agenda line
  (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)) 'append)
  (setq org-agenda-custom-commands
      '(("n" "Next actions" todo "NEXT"
     ((org-agenda-todo-ignore-scheduled nil)
      (org-agenda-todo-ignore-deadlines nil)
      (org-agenda-todo-ignore-with-date nil)
      (org-agenda-todo-ignore-timestamp nil)))
    ("i" "Inbox items"
     ((tags "+CATEGORY=\"Entrada\"+LEVEL>=2|+CATEGORY=\"Entrada móvil\""
        ((org-agenda-overriding-header "Tareas a procesar")))))
    ("O" "Office block agenda"
     ((agenda ""
          ((org-agenda-ndays 7)         ;; overview of appointments
           (org-agenda-start-on-weekday nil)) ;; calendar begins today
          )
      (tags "+CATEGORY=\"Entrada\"+LEVEL>=2|+CATEGORY=\"Entrada móvil\""
        ((org-agenda-overriding-header "Tareas a procesar")))
      (tags-todo "-@home-@errand-CATEGORY=\"Proyectos\""
           ;; getting all with tags @office and <no tag>
             ((org-agenda-overriding-header "Tareas")
              (org-agenda-todo-ignore-scheduled t)
              (org-agenda-todo-ignore-deadlines t)
              (org-agenda-todo-ignore-with-date t)
              (org-agenda-todo-ignore-timestamp t)))
      (tags-todo "-@home-@errand+CATEGORY=\"Proyectos\"/+NEXT|+WAITING"
           ;; getting all with tags @office and <no tag>
             ((org-agenda-overriding-header "Proyectos")
              (org-agenda-todo-ignore-scheduled t)
              (org-agenda-todo-ignore-deadlines t)
              (org-agenda-todo-ignore-with-date t)
              (org-agenda-todo-ignore-timestamp t)))
      )
     (;(org-agenda-compact-blocks t)
      (org-agenda-tags-todo-honor-ignore-options t)))
    ("H" "Home/Personal agenda"
     ((agenda ""
          ((org-agenda-ndays 7)         ;; overview of appointments
           (org-agenda-start-on-weekday nil)) ;; calendar begins today
          )
      (tags "+CATEGORY=\"Entrada\"+LEVEL>=2|+CATEGORY=\"Entrada móvil\""
        ((org-agenda-overriding-header "Tareas a procesar")))
      (tags-todo "-@office-@errand-CATEGORY=\"Proyectos\""
           ;; getting all with tags @home  and <no tag>
             ((org-agenda-overriding-header "Tareas")
              (org-agenda-todo-ignore-scheduled t)
              (org-agenda-todo-ignore-deadlines t)
              (org-agenda-todo-ignore-with-date t)
              (org-agenda-todo-ignore-timestamp t)))
      (tags-todo "-@office-@errand+CATEGORY=\"Proyectos\"/+NEXT|+WAITING"
           ;; getting all with tags @home and <no tag>
             ((org-agenda-overriding-header "Proyectos")
              (org-agenda-todo-ignore-scheduled t)
              (org-agenda-todo-ignore-deadlines t)
              (org-agenda-todo-ignore-with-date t)
              (org-agenda-todo-ignore-timestamp t)))
      )
     (;(org-agenda-compact-blocks t)
      (org-agenda-tags-todo-honor-ignore-options t)))
    ("P" "Agenda impresa"
     ((agenda "" ((org-agenda-ndays 7)         ;; overview of appointments
              (org-agenda-start-on-weekday nil) ;; calendar begins today
              (org-agenda-repeating-timestamp-show-all t)
              (org-agenda-entry-types '(:timestamp :sexp))
              ))
      (agenda "" ((org-agenda-ndays 1)       ;; daily agenda
              (org-deadline-warning-days 7) ;; 7 day advanced warning for deadlines
              (org-agenda-todo-keyword-format "[ ]")
              (org-agenda-scheduled-leaders '("" ""))
              (org-agenda-prefix-format "%t%s")))
      (todo "TODO"                ;; todos sorted by context
        ((org-agenda-prefix-format "[ ] %T: ")
         (org-agenda-sorting-strategy '(tag-up priority-down))
         (org-agenda-todo-keyword-format "")
         (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
     ((org-agenda-with-colors nil)
      (org-agenda-compact-blocks t)
      (org-agenda-remove-tags t)
      (ps-number-of-columns 2)
      (ps-landscape-mode t))
     ("~/agenda.pdf"))
    ("g" . "GTD contexts")
    ("go" "Office" tags-todo "@office")
    ("gh" "Home" tags-todo "@home")
    ("ge" "Errands" tags-todo "@errands")))
  (setq org-stuck-projects
      '("+LEVEL=2+CATEGORY=\"Proyectos\"" ("NEXT" "WAITING") nil ""))


  ;; Targets include this file and any file contributing to the agenda - up to 2 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 1)
                                 (org-agenda-files :maxlevel . 1))))

  ;;
  (add-hook 'org-mode-hook 'turn-on-font-lock 'turn-on-auto-fill)
  (setq org-hide-leading-stars t) ; A cleaner outline view

  ;; MobileOrg
  (setq org-mobile-directory "~/Dropbox/MobileOrg")
  (setq org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")
  (setq org-mobile-files
      (quote ("~/Dropbox/org/mybookmarks.org"
          "~/Dropbox/org/gtd.org"
          "~/Dropbox/org/control.org"
          "~/Dropbox/org/finances.org"
          "~/Dropbox/org/notes.org"
          "~/Dropbox/org/from-mobile.org"
          "~/Dropbox/org/someday.org")))
  ;(setq org-mobile-files (cons "~/Dropbox/org/mysecrets.org.gpg"
  ;              (org-agenda-files)))
  ;(setq org-mobile-files '(org-agenda-files))

  ;; copy gpg files after pushing to MobileOrg
  ;; if gpg files are included in org-mobile-files
  ;; you must enter the passphrase whenever org-moble-push
  ;; is executed
  (add-hook 'org-mobile-post-push-hook
      (lambda ()
        (dolist (file (file-expand-wildcards "~/Dropbox/org/*.gpg"))
  ; copy *.org.gpg file
          (copy-file file
             (concat (file-name-as-directory org-mobile-directory)
                 (file-name-nondirectory file))
             'ok-if-exists 't)
  ; fill index.org
          (let ((filename (file-relative-name file "~/Dropbox/org"))

            (check (shell-command-to-string
                (concat org-mobile-checksum-binary " "
                    (shell-quote-argument (expand-file-name file))))))
  ; fill checksums.dat
        (append-to-file
         (format "* [[file:%s][%s]]\n" filename filename)
         nil (concat (file-name-as-directory org-mobile-directory) "index.org"))

        (when (string-match "[a-fA-F0-9]\\{30,40\\}" check)
          (append-to-file (format "%s  %s\n" (match-string 0 check) filename)
                  nil (expand-file-name "checksums.dat" org-mobile-directory)))
          ))))

  ; execute org-mobile-push after saving a org file
  ;;(defun org-mobile-push-after-save ()
  ;; (if (member buffer-file-name (directory-files org-directory t))
  ;;     (org-mobile-push)))
  ;(add-hook 'after-save-hook 'org-mobile-push-after-save)
  ;; execute org-mobile-pull every 30 minutes
  ;;(run-at-time 30 (* 30 60) 'org-mobile-pull)
  ;; execute org-mobile-push every 30 minutes
  ;;(run-at-time "15 min" (* 30 60) 'org-mobile-push)
  ;; execute org-mobile-pull just before killing emacs
  ;;(add-hook 'kill-emacs-hook 'org-mobile-pull)
  ;; execute org-mobile-push just before killing emacs
  ;;(add-hook 'kill-emacs-hook 'org-mobile-push)
)

;; Calendar
(setq european-calendar-style t)
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["Domingo" "Lunes" "Martes"
       "Miercoles" "Jueves" "Viernes" "Sábado"]
      calendar-month-name-array
      ["Enero" "Febrero" "Marzo" "Abril"
       "Mayo" "Junio" "Julio" "Agosto" "Septiembre"
       "Octubre" "Noviembre" "Diciembre"])

; open freeplane attach
;(add-to-list 'org-file-apps '("\\.mm\\'" . "freeplane %s"))
(add-hook 'org-mode-hook
    '(lambda ()
       (setq org-file-apps
          (append '(
               ("\\.mm\\'" . default)
                    ) org-file-apps ))))


;; org-crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(setq org-crypt-key nil)
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

;(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-



;; in org-mode, by default, long lines disappear into the right window edge.
;;  If you prefer them wrapped, you can add this to your init file:
; deactivated (add-hook 'org-mode-hook 'soft-wrap-lines)
(defun soft-wrap-lines ()
  "Make lines wrap at window edge and on word boundary,
in current buffer."
  (interactive)
  (setq truncate-lines nil)
  (setq word-wrap t)
  )


;; EasyPGP
;; Switch to ASCII-armored files for Android APG.
;; https://github.com/matburt/mobileorg-android/issues/110
(setq epa-armor t)

(global-set-key (kbd "C-c C-g") 'gtd)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c l") 'org-store-link)
