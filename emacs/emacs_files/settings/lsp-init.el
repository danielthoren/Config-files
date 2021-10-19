(setq lsp-keymap-prefix "C-c l")
(use-package lsp-mode
  :after yasnippet which-key
  :ensure t
  :bind (:map lsp-mode-map
              ("M-i" . xref-find-definitions)
              ("M-I" . xref-find-definitions-other-window)
              ("C-M-i" . xref-pop-marker-stack)
              ("C-c r" . lsp-rename))
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-enable-file-watchers nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  ;; Set memory thresholds higher to increase performance
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  :config
  (define-key lsp-mode-map (kbd lsp-keymap-prefix) lsp-command-map)
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
              ("C-c R" . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-doc-enable nil)       ;; Disable on hover dialogs to speed up emacs
  (lsp-ui-sideline-enable nil)       ;; Disable sideline code actions
  (setq lsp-prefer-flymake nil)
  (setq lsp-signature-render-documentation nil) ;; Remove signature help
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-completion-provider :company)
  )

(use-package ccls
  :ensure t
  :after lsp-ui company
  ;; :init
  ;; (setq ccls-executable "C:\ProgramData\chocolatey\lib\ccls\tools")
  )

;; If in windows, set exec path to chocolatey install
(when (string-equal system-type "windows-nt")
  (progn
    (message "Windows ccls settings")
    )
  (setq ccls-executable "C:\ProgramData\chocolatey\lib\ccls\tools")
  )

(use-package dap-mode
  :ensure t
  :after lsp-mode lsp-ui
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(provide 'lsp-init)
