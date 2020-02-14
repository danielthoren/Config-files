

(defun insert-java-comp ()
  "Insert komplettering at cursor point."
  (interactive)
  (insert "/*************** Komplettering ***************/\n/* TODO: */\n/*********************************************/")
  (backward-char 50))

(defun insert-java-comm ()
  "Insert comment at cursor point."
  (interactive)
  (insert "/***************** Kommentar *****************/\n/* */\n/*********************************************/")
  (backward-char 50))

(define-minor-mode labass-mode
  "Toggle labass-mode"
  :lighter " foo"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c k") 'insert-java-comp)
	    (define-key map (kbd "C-c l") 'insert-java-comm)
            map))

(define-global-minor-mode labass-mode-global labass-mode (lambda () (labass-mode 1)))

(provide 'labass-init)

