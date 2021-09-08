
;; Can be used if signature of gnu key fails
;; (setq package-check-signature 'nil)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;; Settings path
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
;; Functions dir
(setq funs-dir
      (expand-file-name "funs" user-emacs-directory))

;; Add to load path
(add-to-list 'load-path funs-dir)
(add-to-list 'load-path settings-dir)

;; Require packages without dependencies.
(require 'font-settings)
(require 'base-settings)

(load-library "url-handlers")

;; Init package manager.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t)
  (add-to-list
   'package-archives
   '("gnu" . "https://elpa.gnu.org/packages/")
   t)
  (package-initialize))

;; Automatically update packages
(when (not package-archive-contents)
  (package-refresh-contents))

;;Package management
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (setq neo-theme 'icons)
;;  (doom-themes-neotree-config)
  )

(use-package solaire-mode
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'turn-on-solaire-mode)
  (solaire-global-mode +1)
  )

(use-package dumb-jump
  :bind (("M-S-i" . dumb-jump-go-other-window)
         ("M-i"   . dumb-jump-go)
	 ("C-M-i" . xref-pop-marker-stack))
  ;;("M-" . dumb-jump-go-prompt))
  :ensure t)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;;windows only stuff

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (when (string-equal system-type "windows-nt")
;;   (progn
;;     (setq exec-path (append exec-path '("c:\cygwin64\bin" "C:\apps\GnuWin32\gnuwin32\bin")))

    ;; (setq cygwin-bin "c:\cygwin64\bin")
    ;; (setq gnu-bin "C:\apps\GnuWin32\gnuwin32\bin")
    ;; (setenv "PATH"
    ;; 	    (concat cygwin-bin ";" gnu-bin ";"))
    ;; (setq exec-path
    ;; 	  '(cygwin-bin gnu-bin)
    ;; 	  )
  ;;   )
  ;; )

;; General functions.
(require 'general-funs)

;; Init minor modes.
(use-package projectile
  :ensure t)
(use-package ivy
  :ensure t)
(use-package irony
  :ensure t)
(use-package compile
  :ensure t)
(use-package neotree
  :ensure t)
(use-package dap-mode
  :ensure t)
(use-package counsel
  :ensure t)
(use-package dashboard
  :ensure t)
;; (use-package magit
;;   :ensure t)
(use-package diff-hl
  :ensure t)
(use-package aggressive-indent
  :ensure t)
(use-package company
:ensure t)
(use-package git
  :ensure t)
(use-package cmake-ide
  :ensure t)
(use-package jedi
  :ensure t)
(use-package flycheck
  :ensure t
  :init (flycheck-mode))
(use-package grep
  :ensure t)


;; Init major modes.
(require 'elisp-init)
(require 'c-c++-init)
(require 'python-init)
(require 'latex-init)
(require 'markdown-init)
(require 'org-init)
(require 'java-init)
(require 'company-init)
(require 'key-bindings)
(require 'csharp-init)

;; Highlight line numbers.
(linum-mode)


(use-package tern-auto-complete
  :ensure t)

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;; Flydiff
;; (defun my-diff-hook ()
;;   (magit-auto-revert-mode -1)
;;   (diff-hl-mode)
;;   (diff-hl-flydiff-mode))

(add-hook 'prog-mode-hook 'my-diff-hook)

;; Dashboard
(when (>= emacs-major-version 26)
  (dashboard-setup-startup-hook))

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
   '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))
 '(diff-hl-flydiff-mode t)
 '(irony-additional-clang-options nil)
 '(package-selected-packages
   '(exec-path-from-shell xref-js2 git-grep grep-a-lot tree-sitter-indent csharp-mode python flymake-python-pyflakes python-mode ## lsp-ui flycheck-pyflakes major-mode-icons modern-cpp-font-lock all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich spaceline-all-the-icons all-the-icons-dired treemacs-all-the-icons all-the-icons-gnus all-the-icons ace-flyspell latex-preview-pane digitalocean-helm org-ref json-mode counsel-projectile use-package-ensure-system-package nlinum use-package-hydra virtualenv company-jedi jedi use-package rg rtags ag company-ctags org-edit-latex flycheck-clang-analyzer helm-lsp dap-mode company-lsp auto-yasnippet java-snippets org-bullets ctags-update counsel-etags spinner lsp-java jdee neotree neon-mode ac-clang flycheck-bashate company-irony-c-headers dumb-jump ace-jump-zap aggressive-indent flycheck-irony flycheck diff-hl magit company solaire-mode projectile ivy irony doom-themes dash cmake-ide cmake-font-lock))
 '(safe-local-variable-values '((TeX-master . t))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
