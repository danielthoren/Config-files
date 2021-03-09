(defun my-python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi)
  (company-mode)

  (dumb-jump-mode)
  (setq dumb-jump-force-searcher 'ag)
  ;;disable navigation by tags
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
  ;;Enable xref backend
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t)
  
  (local-set-key (kbd "M-i") 'dumb-jump-go)
  (local-set-key (kbd "C-M-i") 'dumb-jump-back)
  
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)

(provide 'python-init)
