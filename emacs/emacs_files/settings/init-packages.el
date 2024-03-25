
;; General packages

(use-package buttercup
  :ensure t)

;;NOTE: Must run M-x 'all-the-icons-install-fonts' for this to work
(use-package all-the-icons
  :ensure t
  :init (all-the-icons-install-fonts)
  :if (display-graphic-p)
  :hook doom-themes
  )

(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (setq neo-theme 'icons)
   (doom-themes-neotree-config)
  )

(use-package solaire-mode
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'turn-on-solaire-mode)
  (solaire-global-mode +1)
  )

(use-package dired-ranger
  :ensure t
  :after dired
  :hook dired
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste))
  )

(use-package dtrt-indent ;; Auto detect indentation strategy in file
  :ensure t
  :config
  (setq dtrt-indent-run-after-smie t) ;; Run even if SMIE is active
  (dtrt-indent-global-mode)
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
  :bind (
         ("M-x" . counsel-M-x)
         ("C-s" . swiper)
         ("C-c C-y" . counsel-yank-pop)
         ;; ("C-x C-f" . counsel-find-file)
         )
  )

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-x t") 'neotree-toggle)
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
  :bind (
         ("C-c g" . grep-find)
         )
  :config
  (grep-apply-setting 'grep-find-command
                      '("find . -type f \\( ! -iname \"\.\*\" ! -iname \"\*\.so\" ! -iname \"\*\.bin\" ! -iname \"\*\.o\" ! -iname \"\*~\" ! -iname \"\*\.#\" ! -path \"\*/\.\*/\*\" ! -path \"\*/x86/\*\" ! -path \"\*/build/\*\" \\) -exec grep -inH -e  \\{\\} +" . 187))
  )

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("todo"       warning bold)
          ("Todo"       warning bold)
          ("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5)
  )

(use-package dockerfile-mode
  :ensure t)

;; Spell correction tool
(use-package flyspell
  :ensure t
  :hook
  (prog-mode . flyspell-prog-mode)
  (org-mode . flyspell-mode)
  (markdown-mode . flyspell-mode)
  (text-mode . flyspell-mode)     ;; Mode used for git commit
  :config
  ;; Disable default keybindings
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-c $") nil)
  (define-key flyspell-mode-map (kbd "C-c $") nil)

  (use-package flyspell-correct
    :ensure t
    :config
    (use-package flyspell-correct-popup
      :ensure t
      :bind ("<f8>" . flyspell-correct-wrapper)
      :init (setq flyspell-correct-interface #'flyspell-correct-popup)
      )
    )
  )

;
;; Prog mode packages
;

(use-package projectile
  :ensure t
  :bind (
         ("C-c c" . projectile-compile-project)
         )
  :config
  ;; Make compilation buffer scroll with output
  (setq compilation-scroll-output t)
  ;; If a compilation buffer is already open, use that instead of
  ;; opening a new buffer
  (add-to-list 'display-buffer-alist
               (cons "\\`\\*compilation\\*\\'"
                     (cons 'display-buffer-reuse-window
                           '((reusable-frames . visible)
                             (inhibit-switch-frame . nil)))))
  )

(use-package company-mode
  :after lsp-mode
  :ensure company
  :bind ("C-<return>" . company-complete-common)
  :hook (c-mode
         c++-mode
         lisp-mode
         lsp-mode
         emacs-lisp-mode
         python-mode-hook
         )
  :init
    (company-mode)
    (setq company-idle-delay              nil)
    (setq company-minimum-prefix-length   0)
    (setq company-show-numbers            nil)
    (setq company-tooltip-limit           10)
    (setq company-dabbrev-downcase        nil)
    (setq company-backends (delete 'company-semantic company-backends))
  :config
  (use-package company-quickhelp
    :ensure t
    :hook company-mode)
  )

;; Use company-jedi instead of company for python mode
;; Using company causes issues
(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("M-i" . xref-find-definitions)
              ("M-I" . xref-find-definitions-other-window)
              ("C-M-i" . xref-pop-marker-stack)
              ("C-c r" . lsp-rename))
  :hook (
         (c-mode . lsp)
         (c++-mode . lsp)
         (python-mode . lsp)
         (java-mode . lsp)
         )

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
  (setq lsp-lens-enable nil)                  ;; Disable lsp lenses to speed up emacs
  :config
  (define-key lsp-mode-map (kbd lsp-keymap-prefix) lsp-command-map)
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode company-mode
  :bind (:map lsp-ui-mode-map
              ("C-c R" . lsp-ui-peek-find-references))
  :config
  (setq lsp-enable-symbol-highlighting t)     ;; Symbol highlightning

  ;;(setq lsp-ui-doc-enable nil)              ;; Doc on cursor hover
  (setq lsp-ui-doc-show-with-cursor nil)      ;; Doc on mouse hover

  (setq lsp-headerline-breadcrumb-enable t)   ;; Headerline

  (setq lsp-ui-sideline-enable t)             ;; Sideline
  (setq lsp-ui-sideline-show-code-actions nil);; sideline code actions
  (setq lsp-ui-sideline-show-hover nil)       ;; Sideline hover symbols
  (setq lsp-ui-sideline-show-diagnostics t)   ;; Sideline diagnostics

  (setq lsp-modeline-code-actions-enable t)   ;; Enable modline actions

  (setq lsp-signature-auto-activate nil)        ;; Signature help
  (setq lsp-signature-render-documentation nil) ;; Signature documentation help

  (setq lsp-prefer-flymake nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-diagnostics-provider :auto) ;; Prefer flycheck, fall back to flymake

  (setq lsp-completion-provider :company)
  (setq lsp-completion-show-detail nil)  ;; Show completion detail
  (setq lsp-completion-show-kind t)
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
    (message "Windows ccls settings")
    (setq ccls-executable "C:\ProgramData\chocolatey\lib\ccls\tools")
    )
  )

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :init (global-flycheck-mode)
  )

(use-package dap-mode
  :ensure t
  :after lsp-mode lsp-ui
  :hook python-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  )

(use-package powershell
  :ensure t)

(use-package fish-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package ansi-color
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max))
    )

  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  )

(use-package clang-format
  :ensure t
  )

;; (use-package clang-format+
;;   :ensure t
;;   :after clang-format
;;   :hook (c-mode
;;          c++-mode)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""     Set up white space mode      """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bmw/color-dim (steps)
  (apply 'color-rgb-to-hex
         (car (color-gradient
               (color-name-to-rgb (face-attribute 'default :background))
               (color-name-to-rgb (face-attribute 'default :foreground))
               steps))))

(defun bmw/theme-whitespace ()
  "Apply my own face-attribute changes after loading a custom theme"
  (set-face-attribute 'whitespace-indentation nil
                      :background
                      (face-attribute 'error :background)
                      :foreground
                      (face-attribute 'error :foreground))
  (set-face-attribute 'whitespace-tab nil
                      :background
                      (face-attribute 'font-lock-comment-face :background)
                      :foreground
                      (face-attribute 'font-lock-comment-face :foreground))
  (set-face-attribute 'whitespace-space nil
                      :background
                      (face-attribute 'font-lock-comment-face :background)
                      :foreground
                      (bmw/color-dim 3)))

(use-package whitespace
  :ensure t
  :preface
  (defun bmw/whitespace-mode ()
    (unless (eq buffer-file-name nil)
      (progn
        (whitespace-mode)
        (bmw/theme-whitespace))))
  :custom
  (unless (or (eq major-mode 'org-mode)
              (string-match "\\.mk\\'" buffer-file-name)
              (string-match "\\.json\\'" buffer-file-name)
              (string-match "\\.md\\'" buffer-file-name)
              (string-match "\\.txt\\'" buffer-file-name))
    (whitespace-action '(auto-cleanup))
    )
  (whitespace-line-column 81)
  (whitespace-style
   '(face trailing empty spaces indentation space-mark tab-mark)) ;; lines
  :hook
  ((prog-mode text-mode) . bmw/whitespace-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""              Python              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Python language server used with lsp
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred


(use-package highlight-indent-guides-mode
  :ensure highlight-indent-guides
  :hook python-mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""              markdown            """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'init-packages)
