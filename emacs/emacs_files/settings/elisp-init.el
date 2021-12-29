(defun my-elisp-hook ()
  (company-mode))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-hook)

(provide 'elisp-init)
