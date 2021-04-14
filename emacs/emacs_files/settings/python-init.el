(defun my-python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi)
  (company-mode)

  )

(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'my-python-mode-hook)

(provide 'python-init)
