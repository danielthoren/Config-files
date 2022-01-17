

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; C-mode comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/c++-comment-setup ()
  "Setup magic multiline C++ comments. M-j for newline with
multiline comment prefix."
  (interactive)
  (setq-local comment-start "/**"
              comment-end   " */"
              comment-multi-line t
              comment-padding nil
              comment-style 'extra-line
              comment-continue " * "
              comment-empty-lines t))

(defun c-block-comment ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (let* ((indent (current-column))
         (stars (make-string (- 78 indent) ?/)))
    (insert stars "\n")
    (indent-to indent)
    (insert "/**\n")
    (indent-to indent)
    (insert " * \n")
    (indent-to indent)
    (insert " */ \n")
    (indent-to indent)
    (insert stars)
    (end-of-line -1)
    )
  )

;;Insert function doc comment.
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
    (end-of-line 0))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Python mode comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun my/python-block-comment ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (let* ((indent (current-column))
         (stars (make-string (- 78 indent) ?#)))
    (insert stars "\n")
    (indent-to indent)
    (insert "\"\"\"   \"\"\" \n")
    (indent-to indent)
    (insert stars)
    (end-of-line 0)
    (backward-char 6)
    )
  )

;;Insert function doc comment.
(defun my/python-doc-comment ()
  (interactive)
  ;; (beginning-of-line)
  ;; (indent-according-to-mode)
  (let* ((indentation (current-column)))
    (insert "\"\"\"\n")
    (indent-to indentation)
    (insert "\n")
    (indent-to indentation)
    (insert "\"\"\"")
    (end-of-line 0))
  )


(provide 'commentFunctions)
