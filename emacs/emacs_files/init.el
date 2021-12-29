
;; Can be used if signature of gnu key fails
;; (setq package-check-signature 'nil)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Prefer to recompile config files rather than using old binary files
(setq load-prefer-newer t)

;; Settings path
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
;; Gendoxy dir
(setq gendoxy-dir
      (expand-file-name "gendoxy" user-emacs-directory))

;; Add to load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path gendoxy-dir)

(require 'package) ;; Emacs builtin

;; set package.el repositories
(setq package-archives
'(
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
))

;; initialize built-in package management
(package-initialize)

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

;; TODO: Test gendoxy in other languages than c/c++
;; If it does not work, move to c-c++-init
(require 'gendoxy)
(global-set-key (kbd "C-c d h") 'gendoxy-header)
(global-set-key (kbd "C-c d g") 'gendoxy-group)
(global-set-key (kbd "C-c d t") 'gendoxy-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init minor modes.
(use-package dtrt-indent ;; Auto detect indentation strategy in file
  :ensure t
  :config
  (setq dtrt-indent-run-after-smie t) ;; Run even if SMIE is active
  :init
  (add-hook 'prog-mode-hook 'dtrt-indent-mode)
  )

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  :init
  (add-hook 'prog-mode-hook 'multiple-cursors-mode)
  )
(use-package which-key ;; Helps with key bindings
  :ensure t)
(use-package ivy
  :ensure t)
(use-package irony
  :ensure t)
(use-package neotree
  :ensure t
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 40)
  )
(use-package counsel
  :ensure t
  :init
  (global-set-key (kbd "M-x") 'counsel-M-x))
(use-package dashboard
  :ensure t)
(use-package magit
  :ensure t)
;; (use-package diff-hl
;;   :ensure t)
(use-package flycheck
  :ensure t)
(use-package grep
  :ensure t)
(use-package smooth-scrolling
  :ensure t)
(use-package tern-auto-complete
  :ensure t
  :config
  ;; Disable completion keybindings, as we use xref-js2 instead
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  )
(use-package company
  :ensure t
  :bind ("C-<return>" . company-complete-common)
  :config
  ;; General settings
  (setq company-idle-delay             nil
        company-minimum-prefix-length   0
        company-show-numbers            nil
        company-tooltip-limit           10
        company-dabbrev-downcase        nil
        company-backends (delete 'company-semantic company-backends))
  (use-package company-quickhelp
    :ensure t
    :hook company)
  )

(use-package projectile
  :ensure t
  ;; :hook prog-mode ;; TODO: Fix projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add external modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Require base packages
(require 'base-settings)

;; Init major modes.
(require 'elisp-init)
(require 'c-c++-init)
(require 'python-init)
(require 'lsp-init)
(require 'org-init)
(require 'key-bindings)
;; (require 'csharp-init) //TODO: Fix csharp-init, not working atm
(require 'powerShell-init)

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
    (multiple-cursors dtrt-indent jedi-direx irony-eldoc yasnippet powershell csharp-mode tree-sitter-indent tree-sitter-langs tree-sitter org-bullets dap-mode ccls lsp-ui company-jedi lsp-pyright which-key use-package tern-auto-complete spinner solaire-mode smooth-scrolling projectile neotree markdown-mode magit lv jedi irony ht hl-todo highlight-indent-guides git flycheck doom-themes diff-hl dashboard counsel company cmake-ide all-the-icons aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
