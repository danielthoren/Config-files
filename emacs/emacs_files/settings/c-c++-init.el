;; (use-package cmake-ide
;;   :ensure t
;;   :init
;;   (setq cmake-ide-make-command "make --no-print-directory -j8")
;;   :config
;;   (cmake-ide-setup))

(use-package flycheck
  :ensure t
  :init (flycheck-mode))

(defun my/c++-comment-setup ()
  "Setup magic multiline C++ comments. M-j for newline with
multiline comment prefix."
  (interactive)
  (setq-local comment-start "/**"
              comment-end   " */"
              comment-multi-line t
              comment-padding nil
              comment-style 'extra-line
              comment-continue " * "
              comment-empty-lines t))

;; Common c and c++ mode hook.
(defun my-c-c++-mode-hook ()
  (company-mode)
  (lsp-mode)
  (lsp)
  (projectile-mode)
  (yas-minor-mode)

  (defun c-block-comment ()
    (interactive)
    (beginning-of-line)
    (open-line 1)
    (let* ((indent (current-column))
	         (stars (make-string (- 78 indent) ?/)))
      (insert stars "\n")
      (indent-to indent)
      (insert "/**\n")
      (indent-to indent)
      (insert " * \n")
      (indent-to indent)
      (insert " */ \n")
      (indent-to indent)
      (insert stars)
      (end-of-line -1)
      )
    )

  ;;Insert function doc comment.
  (defun c-doc-comment ()
    (interactive)
    (beginning-of-line)
    (indent-according-to-mode)
    (let* ((indentation (current-column)))
      (insert "/**\n")
      (indent-to indentation)
      (insert " * \n")
      (indent-to indentation)
      (insert " */")
      (end-of-line 0))
    )


  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; C-mode stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  ;; (setq c-basic-offset 4)                  ;; Default is 2
  ;; (setq c-indent-level 4)                  ;; Default is 2

  ;; (local-set-key (kbd "C-c m") 'cmake-ide-compile)
  (local-set-key (kbd "C-c i") 'indent-buffer)
  (local-set-key (kbd "C-M-k") 'c-block-comment)
  (local-set-key (kbd "C-M-j") 'c-doc-comment)

  )

;; Hooks
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))

(provide 'c-c++-init)
