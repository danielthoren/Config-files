
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
;; functions dir
(setq functions-dir
      (expand-file-name "functions" user-emacs-directory))

;; ;; Add to load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path gendoxy-dir)
(add-to-list 'load-path functions-dir)

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

;;windows only stuff
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize local files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init theme
(require 'theme-init)

;; Init settings without dependencies
(require 'base-settings)

;; Init key bindings without dependencies
(require 'key-bindings)

;; Init comment functions
(require 'commentFunctions)

;; Init org mode
(require 'org-init)

;; (require 'centering-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;; General packages
;

(use-package dtrt-indent ;; Auto detect indentation strategy in file
  :ensure t
  :hook (prog-mode . dtrt-indent-mode)
  :config
  (setq dtrt-indent-run-after-smie t) ;; Run even if SMIE is active
  )
(use-package multiple-cursors
  :ensure t
  :hook (prog-mode . multiple-cursors-mode)
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)
         ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)
         )
  )
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-s" . swiper))
  )
(use-package neotree
  :ensure t
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 40)
  )
(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook)
  :init (setq dashboard-startup-banner 'logo)
  )
(use-package magit
  :ensure t
  :defer t)
(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode)
  )
(use-package grep
  :ensure t
  :defer t)

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

(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode)
  )

;
;; Prog mode packages
;

(use-package projectile
  :ensure t
  ;; :hook prog-mode ;; TODO: Fix projectile TODO: Investigate if this package is necessary
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  )

(use-package company-mode
  :ensure company
  :bind ("C-<return>" . company-complete-common)
  :hook (c-mode
         c++-mode
         python-mode
         lisp-mode
         lsp-mode
         )
  :init(company-mode)
    (setq company-idle-delay              nil)
    (setq company-minimum-prefix-length   0)
    (setq company-show-numbers            nil)
    (setq company-tooltip-limit           10)
    (setq company-dabbrev-downcase        nil)
    (setq company-backends (delete 'company-semantic company-backends)
          )
  :config
  (use-package company-quickhelp
    :ensure t
    :hook company-mode)
  )

(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("M-i" . xref-find-definitions)
              ("M-I" . xref-find-definitions-other-window)
              ("C-M-i" . xref-pop-marker-stack)
              ("C-c r" . lsp-rename))
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (python-mode . lsp)
  (java-mode . lsp)

  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  ;; Set memory thresholds higher to increase performance
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-enable-snippet nil)
  :config
  (define-key lsp-mode-map (kbd lsp-keymap-prefix) lsp-command-map)
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode company-mode
  :bind (:map lsp-ui-mode-map
              ("C-c R" . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-doc-enable nil)       ;; Disable on hover dialogs to speed up emacs
  (lsp-ui-sideline-enable nil)       ;; Disable sideline code actions
  (setq lsp-prefer-flymake nil)
  (setq lsp-signature-render-documentation nil) ;; Remove signature help
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-completion-provider :company)
  )

(use-package ccls
  :ensure t
  :after lsp-ui company-mode
  :hook (c-mode
         c++-mode
         objc-mode)
  :init
  (lsp-deferred)
  :config
  (setq ccls-sem-highlight-method 'font-lock)
  (when (string-equal system-type "windows-nt")
  (progn
    (message "Windows ccls settings")
    )
  (setq ccls-executable "C:\ProgramData\chocolatey\lib\ccls\tools")
  )
  )

(use-package dap-mode
  :ensure t
  :after lsp-mode lsp-ui
  :hook python
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  )

;; Python language server used with lsp
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred


(use-package highlight-indent-guides
  :ensure t
  :hook python)

(use-package powershell
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language specific settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gendoxy)
(defun my-c-mode-hook ()
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

  (local-set-key (kbd "C-M-k") 'c-block-comment)
  (local-set-key (kbd "C-M-j") 'c-doc-comment)

  (local-set-key (kbd "C-c d h") 'gendoxy-header)
  (local-set-key (kbd "C-c d g") 'gendoxy-group)
  (local-set-key (kbd "C-c d t") 'gendoxy-tag)

  (setq c-default-style "linux")
  )

(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode 'my-c-mode-hook)


(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-k") 'my/python-block-comment)
            (local-set-key (kbd "C-M-j") 'my/python-doc-comment)
            ;; Replace 'py-hugry-delete-backwards' with traditional 'backwards-kill-word'
            (define-key python-mode-map (kbd "<C-backspace>") 'backward-kill-word)
            ;; Disable auto-complete-mode since it interferes with company
            (auto-complete-mode nil)
            )
          )

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c i") 'indent-buffer)
            )
          )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown")
         (use-package markdown-preview-mode
           :ensure t
           :hook markdown
           )
         )

(use-package csharp-mode
  :ensure t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of user configurable section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;; tramp
;; (setq tramp-default-method "ssh")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-preview-mode company-mode xref-rst which-key virtualenvwrapper virtualenv use-package tree-sitter-langs tree-sitter-indent tern-auto-complete solaire-mode smooth-scrolling python-mode python pyenv-mode-auto powershell org-bullets neotree multiple-cursors magit lsp-ui lsp-pyright lsp-java lsp-ivy js2-mode jedi hl-todo highlight-indent-guides helm-lsp grep-a-lot git-grep git flymake-python-pyflakes flycheck-irony exec-path-from-shell elpy dumb-jump dtrt-indent doxy-graph-mode doom-themes diff-hl dashboard csharp-mode cquery counsel company-quickhelp company-jedi cmake-mode cmake-ide ccls all-the-icons aggressive-indent ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
