;; Common c and c++ mode hook.
(defun my-c-c++-mode-hook ()
  (company-mode)
  (irony-mode)
  (add-to-list 'company-backends 'company-irony-c-headers)
  (add-to-list 'company-backends 'company-irony)  
  (define-key c-mode-map  [(C-return)] 'company-complete)
  (define-key c++-mode-map  [(C-return)] 'company-complete)
  (setq company-idle-delay 999999)

  (use-package flycheck
    :ensure t
    :init (flycheck-mode))

  ;;(flycheck-irony-setup)  

  ;; (aggressive-indent-mode)
  ;; (add-to-list
  ;;  'aggressive-indent-dont-indent-if
  ;;  '(and (derived-mode-p 'c++-mode)
  ;; 	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
  ;;                            (thing-at-point 'line)))))
  
  (dumb-jump-mode)
  (setq dumb-jump-force-searcher 'ag)
  (local-set-key (kbd "M-i") 'dumb-jump-go)
  (local-set-key (kbd "C-M-i") 'dumb-jump-back)
  
  (local-set-key (kbd "M-o") 'ff-find-other-file)
  
  (local-set-key (kbd "C-c m") 'cmake-ide-compile)

  (local-set-key (kbd "C-c k") 'c-doc-comment))


;; Insert function doc comment.
(defun c-doc-comment ()
  (interactive)
  (beginning-of-line)
  (indent-according-to-mode)
  (let* ((indentation (current-column)))
    (insert "/**\n")
    (indent-to indentation)
    (insert " * \n")
    (indent-to indentation)
    (insert " */")
    (end-of-line 0)))


;; C++
(require 'subr-x)
(cmake-ide-setup)
(setq cmake-ide-flags-c++ (append '("-std=c++11")))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))

;; C
(setq c-basic-offset 4
      c-default-style "linux")


;; Hooks
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

(provide 'c-c++-init)
