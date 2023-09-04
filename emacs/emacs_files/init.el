;; Can be used if signature of gnu key fails
;; (setq package-check-signature 'nil)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; NOTE During first setup, after running install script "conf.sh":
;; * M-x 'package-refresh-contents'
;; * M-x 'all-the-icons-install-fonts'

(setq debug-on-error t)

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

;; Block comment mode dir
(setq block-comment-mode-dir
      (expand-file-name "Block-Comment-Mode" user-emacs-directory))

;; ;; Add to load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path gendoxy-dir)
(add-to-list 'load-path functions-dir)
(add-to-list 'load-path block-comment-mode-dir)

(require 'package) ;; Emacs builtin

;; set package.el repositories
(setq package-archives
      '(
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ))

;; No signature check for use with internal melpa mirror
(setq package-check-signature nil)

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
  (exec-path-from-shell-initialize)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Initialize local files                          """
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

;; Init block comment mode
(require 'block-comment-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                           Initialize packages                            """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'image-types 'svg)

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
  :config
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-c $") nil)
  (define-key flyspell-mode-map (kbd "C-c $") nil)
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
  :init(company-mode)
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

(use-package clang-format+
  :ensure t
  :after clang-format
  :hook (c-mode
         c++-mode)
  )

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
    (unless (or (eq major-mode 'org-mode)
                (eq buffer-file-name nil)
                (string-match "\\.json\\'" buffer-file-name)
                (string-match "\\.md\\'" buffer-file-name)
                (string-match "\\.txt\\'" buffer-file-name)
                )
      (progn
        (whitespace-mode)
        (bmw/theme-whitespace))))
  :custom
  (whitespace-action '(auto-cleanup))
  (whitespace-line-column 81)
  (whitespace-style
   '(face trailing empty spaces indentation space-mark tab-mark)) ;; lines
  :hook
  ((prog-mode text-mode) . bmw/whitespace-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                        Language specific settings                        """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""              C/C++               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gendoxy)
(defun my-c-mode-hook ()
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

  ;; Set indentation for new { statement to 0 (ex after if statement)
  (setq c-default-style "linux")
  (c-set-offset 'substatement-open 0)
  ;; Fixes indentation between ()
  (c-set-offset 'arglist-intro '+)
  ;; Fixed indentation of switch statements
  (c-set-offset 'case-label '+)


  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'comment-intro 0)
  ;; (c-set-offset 'defun-block-intro 'tab-width)
  ;; (c-set-offset 'statement-block-intro 'tab-width)
  ;; (c-set-offset 'substatement 'tab-width)
  (c-set-offset 'topmost-intro 0)
  ;; (c-set-offset 'statement-cont 'tab-width)
  (c-set-offset 'func-decl-cont 0)
  (c-set-offset 'brace-list-open 0)
  ;; (c-set-offset 'brace-list-intro 'tab-width)
  (c-set-offset 'brace-list-entry 0)


  (setq c-basic-offset 2)
  (setq c-indent-level 2)

  (local-set-key (kbd "C-M-j") 'c-doc-comment)

  (block-comment--set-comment-style 80
                                     "***"
                                     " "
                                     "***"

                                     "/*"
                                     "*"
                                     "*"

                                     "*"
                                     "*"
                                     "*/")

  (local-set-key (kbd "C-M-k") 'block-comment-start)

  (local-set-key (kbd "C-c d h") 'gendoxy-header)
  (local-set-key (kbd "C-c d g") 'gendoxy-group)
  (local-set-key (kbd "C-c d t") 'gendoxy-tag)

  (defun toggle-indent ()
    """  Toggles between 2 and 4 spaces of indentation    """
    (interactive)
    (if (equal c-basic-offset 2)
        (progn
          (message "Set indent to 4")
          (setq c-basic-offset 4))
      (progn
        (message "Set indent to 2")
        (setq c-basic-offset 2))
      )
    )
  )

(defun my-c++-mode-hook ()
  ;; Disable namespace indent
  (c-set-offset 'innamespace 0)
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""              Python              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-j") 'my/python-doc-comment)

            (block-comment--set-comment-style 80
                                               "\"\"\""
                                               " "
                                               "\"\"\""
                                               "#"
                                               "#"
                                               "#" )
            (local-set-key (kbd "C-M-k") 'block-comment-start)

            ;; Replace 'py-hugry-delete-backwards' with traditional 'backwards-kill-word'
            (define-key python-mode-map (kbd "<C-backspace>") 'backward-kill-word)
            ;; Disable auto-complete-mode since it interferes with company
            (auto-complete-mode nil)
            )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""              elisp               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook
     (lambda ()
            (block-comment--set-comment-style 80 "\"\"\"" " " "\"\"\""  ";;" ";" ";;")
            (local-set-key (kbd "C-M-k") 'block-comment-start)
       )
     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""              bash                """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'sh-mode-hook
          (lambda ()
            (block-comment--set-comment-style 80 "#" " " "#" "#" "#" "#")
            (local-set-key (kbd "C-M-k") 'block-comment-start)
            )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""              prog                """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-indent-buffer()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))
    )
  )

(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c i") 'my-indent-buffer)
            ;; Only enable if version is 27 or newer
            (when (version< "27.0" emacs-version)
              (display-fill-column-indicator-mode)
              )
            )
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

(use-package csharp-mode
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                     End of user configurable section                     """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;; tramp
;; (setq tramp-default-method "ssh")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dockerfile-mode copy-as-format js3-mode use-package-el-get markdown-preview-mode company-mode xref-rst which-key virtualenvwrapper virtualenv use-package tree-sitter-langs tree-sitter-indent tern-auto-complete solaire-mode smooth-scrolling python-mode python pyenv-mode-auto powershell org-bullets neotree multiple-cursors magit lsp-ui lsp-pyright lsp-java lsp-ivy js2-mode jedi hl-todo highlight-indent-guides helm-lsp grep-a-lot git-grep git flymake-python-pyflakes flycheck-irony exec-path-from-shell elpy dumb-jump dtrt-indent doxy-graph-mode doom-themes diff-hl dashboard csharp-mode cquery counsel company-quickhelp company-jedi cmake-mode cmake-ide ccls all-the-icons aggressive-indent ag))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "cd /home/da-thr/git/netenc-l3/; ./build.sh -t=b"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:background "black"))))
 '(ediff-even-diff-A ((t (:background "#263854")))))
(put 'erase-buffer 'disabled nil)
