(use-package highlight-indent-guides
  :ensure t)

(use-package lsp-pyright
  :ensure t)

;; jedi: python language server
(use-package company-jedi
  :ensure t)

(use-package jedi
  :ensure t)


(defun my/python-block-comment ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (let* ((indent (current-column))
         (stars (make-string (- 78 indent) ?#)))
    (insert stars "\n")
    (indent-to indent)
    (insert "\"\"\"   \"\"\" \n")
    (indent-to indent)
    (insert stars)
    (end-of-line 0)
    (backward-char 6)
    )
  )

;;Insert function doc comment.
(defun my/python-doc-comment ()
  (interactive)
  ;; (beginning-of-line)
  ;; (indent-according-to-mode)
  (let* ((indentation (current-column)))
    (insert "\"\"\"\n")
    (indent-to indentation)
    (insert "\n")
    (indent-to indentation)
    (insert "\"\"\"")
    (end-of-line 0))
  )

(defun my-python-mode-hook ()
  (require 'lsp-pyright)
  (lsp)
  (company-mode)
  (jedi-mode)
  (jedi:ac-setup)
  (highlight-indent-guides-mode)
  (add-to-list 'company-backends 'company-jedi)
  (setq jedi:complete-on-dot t)
  (setq company-idle-delay nil)
  (electric-indent-mode -1)

  ;; Replace 'py-hugry-delete-backwards' with traditional 'backwards-kill-word'
  (lambda ()
    (define-key python-mode-map (kbd "<C-backspace>") 'backward-kill-word))

  (local-set-key (kbd "C-M-k") 'my/python-block-comment)
  (local-set-key (kbd "C-M-j") 'my/python-doc-comment)
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Disable auto-complete-mode since it interferes with company
(defadvice auto-complete-mode (around disable-auto-complete-for-python)
  (unless (eq major-mode 'python-mode) ad-do-it))

(ad-activate 'auto-complete-mode)

(provide 'python-init)
