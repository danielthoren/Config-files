
;; Can be used if signature of gnu key fails
;; (setq package-check-signature 'nil)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Prefer to recompile config files rather than using old binary files
(setq load-prefer-newer t)

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

;; Init theme
(require 'theme-init)


;;windows only stuff

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; General functions.
(require 'general-funs)

;; Init minor modes.


(use-package which-key
  :ensure t)
(use-package ivy
  :ensure t)
(use-package irony
  :ensure t)
(use-package compile
  :ensure t)
(use-package neotree
  :ensure t
  :config (setq neo-window-fixed-size nil)
  )
(use-package counsel
  :ensure t)
(use-package dashboard
  :ensure t)
(use-package magit
  :ensure t)
(use-package diff-hl
  :ensure t)
(use-package aggressive-indent
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
(use-package smooth-scrolling
  :ensure t)
(use-package tern-auto-complete
  :ensure t)



;; Init major modes.
(require 'elisp-init)
(require 'c-c++-init)
(require 'python-init)
(require 'lsp-init)
(require 'latex-init)
;; (require 'markdown-init)
(require 'org-init)
;; (require 'java-init)
(require 'company-init)
(require 'key-bindings)
;; (require 'csharp-init)

;; Highlight line numbers.
(linum-mode)

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

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
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(diff-hl-flydiff-mode t)
 '(irony-additional-clang-options nil)
 '(package-selected-packages
   (quote
    (exec-path-from-shell xref-js2 git-grep grep-a-lot tree-sitter-indent csharp-mode python flymake-python-pyflakes python-mode ## lsp-ui flycheck-pyflakes major-mode-icons modern-cpp-font-lock all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich spaceline-all-the-icons all-the-icons-dired treemacs-all-the-icons all-the-icons-gnus all-the-icons ace-flyspell latex-preview-pane digitalocean-helm org-ref json-mode counsel-projectile use-package-ensure-system-package nlinum use-package-hydra virtualenv company-jedi jedi use-package rg rtags ag company-ctags org-edit-latex flycheck-clang-analyzer helm-lsp dap-mode company-lsp auto-yasnippet java-snippets org-bullets ctags-update counsel-etags spinner lsp-java jdee neotree neon-mode ac-clang flycheck-bashate company-irony-c-headers dumb-jump ace-jump-zap aggressive-indent flycheck-irony flycheck diff-hl magit company solaire-mode projectile ivy irony doom-themes dash cmake-ide cmake-font-lock)))
 '(safe-local-variable-values (quote ((TeX-master . t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
