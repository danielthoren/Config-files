(require 'lsp-java)
(require 'yasnippet)
(require 'company-lsp)
(require 'helm-lsp)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'java-mode-hook 'flycheck-mode)

(require 'dap-java)

(add-hook 'java-mode-hook #'lsp)
(provide 'java-init)
