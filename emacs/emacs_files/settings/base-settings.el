;; Remove GUI bloat
(tool-bar-mode -1) 
(menu-bar-mode -1) 
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

;; Line numbering
(global-display-line-numbers-mode)

;; Column numbering
(setq column-number-mode t)

;; Autofollow symlinks
(setq vc-follow-symlinks t)

;; Disable autosaving
(setq auto-save-default nil)
;;(setq backup-directory-alist '(("" . (expand-file-name "backup" user-emacs-directory))))


;; Set opacity

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(95 . 80))
(add-to-list 'default-frame-alist '(alpha . (95 . 80)))

;; Toggle opacity
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ;; Also handle undocumented (<active> <inactive>) form.
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


(provide 'base-settings)
