(use-package cmake-ide
  :ensure t
  :init
  (setq cmake-ide-make-command "make --no-print-directory -j8")
  :config
  (cmake-ide-setup))

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

  ;;Insert function doc comment.
  (defun c-block-comment ()
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

  ;; C
  (setq c-basic-offset 4
	c-default-style "linux")

  (local-set-key (kbd "C-c m") 'cmake-ide-compile)
  (local-set-key (kbd "C-c i") 'indent-buffer)
  (local-set-key (kbd "C-M-k") 'c-doc-comment)
  (local-set-key (kbd "C-M-j") 'c-block-comment)

  )


;; Hooks
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))

(provide 'c-c++-init)
