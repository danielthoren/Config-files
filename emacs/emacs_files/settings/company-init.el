

(use-package company
  :ensure t
  :config
  ;; General settings
  (setq company-idle-delay              0.1
	company-minimum-prefix-length   0
	company-show-numbers            nil
	company-tooltip-limit           10
	company-dabbrev-downcase        nil)

  (defun my-company-mode-hook ()
    (setq company-backends (delete 'company-semantic company-backends))
    (local-set-key (kbd "<C-return>") 'company-complete-common))

  (add-hook 'company-mode-hook 'my-company-mode-hook)
  )

(provide 'company-init)
