(defun my-python-mode-hook ()
  (company-mode)
  (company-quickhelp-mode)
  (jedi-mode)
  (add-to-list 'company-backends 'company-jedi)
  (setq company-idle-delay nil)
  (setq jedi:complete-on-dot t)
  )

(use-package company-jedi
  :ensure t)
(use-package highlight-indent-guides
  :ensure t)

(add-hook 'python-mode-hook 'highlight-indent-guides-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'my-python-mode-hook)


;; Disable auto-complete-mode since it interferes with company
(defadvice auto-complete-mode (around disable-auto-complete-for-python)
  (unless (eq major-mode 'python-mode) ad-do-it))

(ad-activate 'auto-complete-mode)

(provide 'python-init)
