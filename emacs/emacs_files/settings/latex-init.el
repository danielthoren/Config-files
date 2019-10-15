
(defun my-latex-mode ()
  (interactive)
  (latex-preview-pane-enable)
  (company-auctex-init)
  (company-mode))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook 'my-latex-mode)


(provide 'latex-init)

