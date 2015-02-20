;;package-manager
(require 'package)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(defvar my-packages)
(setq my-packages '(
                    ;;ac-cider
                    ;;ac-js2
                    ;;auto-complete
                    ;;bundler
                    ;;company-inf-ruby
                    ;;company-ghc
                    ;;elixir-mix
                    ;;elixir-mode
                    ;;flymake-easy
                    ;;helm-ack
                    ;;helm-company
                    ;;helm-gist
                    ;;helm-git
                    ;;helm-rails
                    ;;helm-robe
                    ;;js3-mode
                    monky
                    ;;(org (20140210))
                    ;;pomodoro
                    ;;popwin
                    ;;powerline
                    ;;rbenv
                    ;;robe
                    ;;rspec-mode
                    ;;rubocop
                    ;;smex
                    smart-mode-line
                    smart-mode-line-powerline-theme
                    ;;tidy
                    ;;twittering-mode
                    ;;window-number
                    ))
(defun install-package (package min-version)
  (unless (package-installed-p package min-version)
    (package-install package)))
(defun install-my-packages ()
  (dolist (p my-packages)
    (if (listp p)
        (let ((pkg (car p))
              (min-version (cadr p)))
          (install-package pkg min-version))
      (install-package p nil))))
(install-my-packages)
;;end package management
