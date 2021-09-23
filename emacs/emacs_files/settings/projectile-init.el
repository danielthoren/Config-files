
(use-package projectile
  :ensure t)

(projectile-global-mode)
(counsel-projectile-mode)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'projectile-init)
