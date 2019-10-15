;; Settings path
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
;; Functions dir
(setq funs-dir
      (expand-file-name "funs" user-emacs-directory))

;; Add to load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path funs-dir)

;; Require packages without dependencies.
(require 'base-settings)
(require 'font-settings)

(load-library "url-handlers")

;; Init package manager.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; Theme.
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)

(require 'solaire-mode)

(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
(add-hook 'after-revert-hook #'turn-on-solaire-mode)
(add-hook 'minibuffer-setup-hook #'turn-on-solaire-mode)

(solaire-mode-swap-bg)

;; General functions.
(require 'general-funs)

;; Init minor modes.
(require 'projectile-init)
(require 'ivy-init)
(require 'company-init)
(require 'irony-init)
(require 'compile-init)
(require 'neotree-init)

;; Init major modes.
(require 'elisp-init)
(require 'c-c++-init)
(require 'python-init)
(require 'latex-init)
(require 'markdown-init)
(require 'org-init)

;; Key bindings.
(require 'key-bindings)

;; Highlight line numbers.
(linum-mode)


;;javascript autocomplete
(require 'company)
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;; Flydiff
(defun my-diff-hook ()
  (magit-auto-revert-mode -1)
  (diff-hl-mode)
  (diff-hl-flydiff-mode))

(add-hook 'prog-mode-hook 'my-diff-hook)

;; Dashboard
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 'logo)

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; GMPL
(add-to-list 'auto-mode-alist '("\\.mod\\'" . gmpl-mode))

;; tramp
(setq tramp-default-method "ssh")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cmake-ide-cmake-opts "-DCMAKE_BUILD_TYPE=Debug")
 '(cmake-ide-make-command "make --no-print-directory -j4")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(diff-hl-flydiff-mode t)
 '(irony-additional-clang-options nil)
 '(package-selected-packages
   (quote
    (auctex cuda-mode helm-pages vue-mode vue-html-mode web-mode evil-smartparens smartparens rainbow-delimiters company-tern auto-complete-c-headers indium ac-js2 company go-mode counsel-projectile projectile column-marker aggressive-indent dashboard use-package yaml-mode vcl-mode subr+ solarized-theme solaire-mode rich-minority nlinum neotree material-theme markdown-mode magit latex-preview-pane jedi hlinum flycheck-irony flx evil dumb-jump doom-themes diff-hl counsel company-jedi company-irony-c-headers company-irony company-c-headers color-theme-sanityinc-tomorrow cmake-project cmake-ide arduino-mode)))
 '(safe-local-variable-values (quote ((TeX-master . t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
