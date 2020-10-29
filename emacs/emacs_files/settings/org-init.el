
;;--------------------------------------------------------------------------------------------------------------------------------------
;; Org mode behaviour
;;--------------------------------------------------------------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Enables latex-like equations etc
(setq org-pretty-entities t)

;;Enables the use of the following formatting for images:
;; #+ATTR_HTML: width="100px"
;; #+ATTR_ORG: :width 100
(setq org-image-actual-width nil)

;;; display Info mode buffers in proportional font
(add-hook 'Info-mode-hook 'variable-pitch-mode)

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;;--------------------------------------------------------------------------------------------------------------------------------------
;; Original source code wich one modified row. Makes the background of images
;; White so that transparent images are visible
;;--------------------------------------------------------------------------------------------------------------------------------------
(defun org-display-inline-images-custom (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (if (fboundp 'clear-image-cache) (clear-image-cache)))
    (save-excursion
      (save-restriction
	(widen)
	(setq beg (or beg (point-min)) end (or end (point-max)))
	(goto-char beg)
	(let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
			  (substring (org-image-file-name-regexp) 0 -2)
			  "\\)\\]" (if include-linked "" "\\]")))
	      (case-fold-search t)
	      old file ov img type attrwidth width)
	  (while (re-search-forward re end t)
	    (setq old (get-char-property-and-overlay (match-beginning 1)
						     'org-image-overlay)
		  file (expand-file-name
			(concat (or (match-string 3) "") (match-string 4))))
	    (when (image-type-available-p 'imagemagick)
	      (setq attrwidth (if (or (listp org-image-actual-width)
				      (null org-image-actual-width))
				  (save-excursion
				    (save-match-data
				      (when (re-search-backward
					     "#\\+attr.*:width[ \t]+\\([^ ]+\\)"
					     (save-excursion
					       (re-search-backward "^[ \t]*$\\|\\`" nil t)) t)
					(string-to-number (match-string 1))))))
		    width (cond ((eq org-image-actual-width t) nil)
				((null org-image-actual-width) attrwidth)
				((numberp org-image-actual-width)
				 org-image-actual-width)
				((listp org-image-actual-width)
				 (or attrwidth (car org-image-actual-width))))
		    type (if width 'imagemagick)))
	    (when (file-exists-p file)
	      (if (and (car-safe old) refresh)
		  (image-refresh (overlay-get (cdr old) 'display))
		(setq img (save-match-data (create-image file type nil :width width :background "white"))) ;; Customized row, added ":background "white"
		(when img
		  (setq ov (make-overlay (match-beginning 0) (match-end 0)))
		  (overlay-put ov 'display img)
		  (overlay-put ov 'face 'default)
		  (overlay-put ov 'org-image-overlay t)
		  (overlay-put ov 'modification-hooks
			       (list 'org-display-inline-remove-overlay))
		  (push ov org-inline-image-overlays))))))))))


;;--------------------------------------------------------------------------------------------------------------------------------------
;; Latex export config
;;--------------------------------------------------------------------------------------------------------------------------------------

;;Makes captions appear below item
(setq org-latex-caption-above nil)

;; Sets path to ditaa installation
(setq org-ditaa-jar-path "/usr/bin/ditaa")

;;Enable ditaa
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; this line activates ditaa

;;Converts tabs into 4 spaces when in source block of unknown language
;;Helps make ditaa editing easier, graph not rendered correctly if block
;;contains tabs
(add-hook 'org-tab-first-hook
          (lambda ()
            (when (org-in-src-block-p t)
              (let* ((elt (org-element-at-point))
                     (lang (intern (org-element-property :language elt)))
                     (langs org-babel-load-languages))
                (unless (alist-get lang langs)
                  (indent-to 4))))))

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(setq reftex-default-bibliography '("~/git/tddd17/report/references.bib"))


;;--------------------------------------------------------------------------------------------------------------------------------------
;; Add latex classes
;;--------------------------------------------------------------------------------------------------------------------------------------

;; Add latex class to global expor tvariable "org-latex-classes" to
;; make org mode recognize IEEEtran class
(setq ieeetran-class
      '("IEEEtran" "\\documentclass[11pt]{IEEEtran}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes ieeetran-class t))

;;hooks
(defun my-org-mode-hook ()
  (org-display-inline-images-custom)
  (org-bullets-mode 1)
  )

(add-hook 'org-mode-hook 'my-org-mode-hook)

(provide 'org-init)
