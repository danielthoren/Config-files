(use-package highlight-indent-guides
  :ensure t)

(use-package lsp-pyright
  :ensure t)


(defun my-python-mode-hook ()
  (require 'lsp-pyright)
  (lsp)
  (company-mode)
  (company-quickhelp-mode)
  (jedi-mode)
  (add-to-list 'company-backends 'company-jedi)
  (setq jedi:complete-on-dot t)
  (setq company-idle-delay nil)
  (electric-indent-mode -1)
  )

;; (use-package company-jedi
;;   :ensure t)


(add-hook 'python-mode-hook 'highlight-indent-guides-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'my-python-mode-hook)


;; Disable auto-complete-mode since it interferes with company
(defadvice auto-complete-mode (around disable-auto-complete-for-python)
  (unless (eq major-mode 'python-mode) ad-do-it))

(ad-activate 'auto-complete-mode)

(provide 'python-init)
