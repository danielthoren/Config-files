;; Can be used if signature of gnu key fails
;; (setq package-check-signature 'nil)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; NOTE During first setup, after running install script "conf.sh":
;; * M-x 'package-refresh-contents'
;; * M-x 'all-the-icons-install-fonts'

;; Variable that determines if the minimal, or normal config should be used.
(setq use-minimal-config nil)

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
(unless use-minimal-config
  (if (not (package-installed-p 'use-package))
      (progn
        (package-refresh-contents)
        (package-install 'use-package)))

  (require 'use-package)
  )

;;windows only stuff
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Initialize local files                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Require minimal config if in minimal config mode, else init packages
(if use-minimal-config
    (require 'minimal-config)
  (require 'init-packages)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                        Language specific settings                        """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""              C/C++               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gendoxy)
(setq gendoxy-details-empty-line t)
(setq gendoxy-default-text "")

(defun my-c-mode-common-settings ()
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

  ;;;;;;;;;;;;;;;;;;;;;;;;;; Style options ;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;; Comment options ;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;; Mode specific functions ;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun my-c-mode-hook ()
  (my-c-mode-common-settings)
  (setq flycheck-clang-language-standard "c23")
  )

(defun my-c++-mode-hook ()
  (my-c-mode-common-settings)
  ;; Disable namespace indent
  (c-set-offset 'innamespace 0)

  (setq flycheck-clang-language-standard "c++17")
  )

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

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
            ;; (auto-complete-mode nil)
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
   '(dired-ranger impatient-mode markdown-preview-eww dockerfile-mode copy-as-format js3-mode use-package-el-get markdown-preview-mode company-mode xref-rst which-key virtualenvwrapper virtualenv use-package tree-sitter-langs tree-sitter-indent tern-auto-complete solaire-mode smooth-scrolling python-mode python pyenv-mode-auto powershell org-bullets neotree multiple-cursors magit lsp-ui lsp-pyright lsp-java lsp-ivy js2-mode jedi hl-todo highlight-indent-guides helm-lsp grep-a-lot git-grep git flymake-python-pyflakes flycheck-irony exec-path-from-shell elpy dumb-jump dtrt-indent doxy-graph-mode doom-themes diff-hl dashboard csharp-mode cquery counsel company-quickhelp company-jedi cmake-mode cmake-ide ccls all-the-icons aggressive-indent ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:background "black"))))
 '(ediff-even-diff-A ((t (:background "#263854")))))
(put 'erase-buffer 'disabled nil)
