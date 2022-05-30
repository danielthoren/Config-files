;; Can be used if signature of gnu key fails
;; (setq package-check-signature 'nil)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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

(require 'block-comment-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                           Initialize packages                            """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General packages

;;NOTE: Must run M-x 'all-the-icons-install-fonts' for this to work
(use-package all-the-icons
  :ensure t
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
  :bind (
         ("M-x" . counsel-M-x)
         ("C-s" . swiper)
         ("C-c C-y" . counsel-yank-pop)
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
  :config ;; TODO: Exclude ccls cache
  (grep-apply-setting 'grep-find-command
                      '("find . -type f \\( ! -iname \"*~\" ! -path \"*/.ccls-cache/*\" ! -path \"*/x86/*\" \\) -exec grep -inH -e  \\{\\} +" . 99))
  )

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
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5)
  )

;
;; Prog mode packages
;

(use-package company-mode
  :ensure company
  :bind ("C-<return>" . company-complete-common)
  :hook (c-mode
         c++-mode
         lisp-mode
         lsp-mode
         emacs-lisp-mode
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

;; Use company-jedi instead of company for python mode
;; Using company causes issues
(use-package company-jedi
  :ensure t
  :hook python-mode
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
   '(face lines trailing empty tabs spaces indentation space-mark tab-mark))
  :hook
  ((prog-mode text-mode) . bmw/whitespace-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                   el-get and packages from emacs-wiki                    """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Auto install el-get package
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)


;; List of packages you want to install
(defvar my-packages '(
                      dired+
                      grep+
                      )
  )
;; This will install any package from my-packages which is not already installed
(el-get 'sync my-packages)


;; Configure dired+
(setq diredp-hide-details-initially-flag nil) ;; If t, hide details by default
(setq diredp-hide-details-propagate-flag t)   ;; If t, use previous hide/show scheme
(set-face-foreground 'diredp-dir-name "green" ) ;; Set dirs color to green
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode))) ;; Hide uninteresting files by default

;; TODO: Bind function: diredp-move-named-in-kill-ring to key
;; TODO: Bind [C-0 w] to better key (copy files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                        Language specific settings                        """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq c-basic-offset 2)
  (setq c-indent-level 2)

  (local-set-key (kbd "C-M-j") 'c-doc-comment)

  (block-comment--init-comment-style 80   "/*" " " "*/"    "/*" "*" "*/" )
  (local-set-key (kbd "C-M-k") 'block-comment--insert-or-resume)

  (local-set-key (kbd "C-c d h") 'gendoxy-header)
  (local-set-key (kbd "C-c d g") 'gendoxy-group)
  (local-set-key (kbd "C-c d t") 'gendoxy-tag)

  (setq c-default-style "linux")
  )

(defun my-c++-mode-hook ()
  ;; Disable namespace indent
  (c-set-offset 'innamespace 0)
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode 'my-c-mode-hook)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-j") 'my/python-doc-comment)

            (block-comment--init-comment-style 80
                                               "\"\"\""
                                               " "
                                               "\"\"\""
                                               "#"
                                               "#"
                                               "#" )
            (local-set-key (kbd "C-M-k") 'block-comment--insert-or-resume)

            ;; Replace 'py-hugry-delete-backwards' with traditional 'backwards-kill-word'
            (define-key python-mode-map (kbd "<C-backspace>") 'backward-kill-word)
            ;; Disable auto-complete-mode since it interferes with company
            (auto-complete-mode nil)
            )
          )

(add-hook 'emacs-lisp-mode-hook
     (lambda ()
            (block-comment--init-comment-style 80 "\"\"\"" " " "\"\"\""    ";;" ";" ";;")
            (local-set-key (kbd "C-M-k") 'block-comment--insert-or-resume)
       )
     )

(add-hook 'sh-mode-hook
          (lambda ()
            (block-comment--init-comment-style 80 "#" " " "#" "#" "#" "#")
            (local-set-key (kbd "C-M-k") 'block-comment--insert-or-resume)
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
   '(js3-mode use-package-el-get markdown-preview-mode company-mode xref-rst which-key virtualenvwrapper virtualenv use-package tree-sitter-langs tree-sitter-indent tern-auto-complete solaire-mode smooth-scrolling python-mode python pyenv-mode-auto powershell org-bullets neotree multiple-cursors magit lsp-ui lsp-pyright lsp-java lsp-ivy js2-mode jedi hl-todo highlight-indent-guides helm-lsp grep-a-lot git-grep git flymake-python-pyflakes flycheck-irony exec-path-from-shell elpy dumb-jump dtrt-indent doxy-graph-mode doom-themes diff-hl dashboard csharp-mode cquery counsel company-quickhelp company-jedi cmake-mode cmake-ide ccls all-the-icons aggressive-indent ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
