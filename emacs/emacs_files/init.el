
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

;; TODO: Test gendoxy in other languages than c/c++
;; If it does not work, move to c-c++-init
(require 'gendoxy)
(global-set-key (kbd "C-c d h") 'gendoxy-header)
(global-set-key (kbd "C-c d g") 'gendoxy-group)
(global-set-key (kbd "C-c d t") 'gendoxy-tag)

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
(use-package flycheck
  :ensure t
  :init (flycheck-mode))
(use-package grep
  :ensure t)
(use-package smooth-scrolling
  :ensure t)
(use-package tern-auto-complete
  :ensure t)

(use-package projectile
  :ensure t
  :config (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  )

;; Require base packages
(require 'font-settings)
(require 'base-settings)

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
;; (require 'csharp-init) //TODO: Fix csharp-init, not working atm
(require 'powerShell-init)

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
 '(package-selected-packages
   (quote
    (jedi-direx irony-eldoc yasnippet powershell csharp-mode tree-sitter-indent tree-sitter-langs tree-sitter org-bullets dap-mode ccls lsp-ui company-jedi lsp-pyright which-key use-package tern-auto-complete spinner solaire-mode smooth-scrolling projectile neotree markdown-mode magit lv jedi irony ht hl-todo highlight-indent-guides git flycheck doom-themes diff-hl dashboard counsel company cmake-ide all-the-icons aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
