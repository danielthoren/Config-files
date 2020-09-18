

(defun insert-java-comp ()
  "Insert komplettering at cursor point."
  (interactive)
  (beginning-of-line)
  (insert "/*************** Complementary work ***************/\n")
  (insert "/* TODO: */\n")
  (insert "/*********************************************/")
  (backward-char 50))

(defun insert-java-comm ()
  "Insert comment at cursor point."
  (interactive)
  (beginning-of-line)
  (insert "/***************** Comment *****************/\n")
  (insert "/* */\n")
  (insert "/*********************************************/")
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

