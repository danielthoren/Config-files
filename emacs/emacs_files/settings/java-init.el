(use-package lsp-java
  :ensure t)
(use-package yasnippet
  :ensure t)
(use-package helm-lsp
  :ensure t)
(use-package lsp-ui
  :ensure t)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'java-mode-hook 'flycheck-mode)

(add-hook 'java-mode-hook #'lsp)
(provide 'java-init)
