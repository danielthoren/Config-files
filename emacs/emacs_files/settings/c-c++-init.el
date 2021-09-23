(use-package cmake-ide
  :ensure t
  :init
  (setq cmake-ide-make-command "make --no-print-directory -j8")
  :config
  (cmake-ide-setup))

(use-package flycheck
  :ensure t
  :init (flycheck-mode))

;; Common c and c++ mode hook.
(defun my-c-c++-mode-hook ()
  (company-mode)
  (add-to-list 'company-backends 'company-lsp)
  (lsp)
  (lsp-mode)


;; Insert function doc comment.
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

;; C
(setq c-basic-offset 4
      c-default-style "linux")

)


;; Hooks
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

(provide 'c-c++-init)
