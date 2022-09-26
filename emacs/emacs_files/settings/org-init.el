;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""  Org mode behaviour                                                       """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Enables latex-like equations etc
(setq org-pretty-entities t)

;;Enables the use of the following formatting for images:
;; #+ATTR_HTML: width="100px"
;; #+ATTR_ORG: :width 100
(setq org-image-actual-width nil)

;; Do not export Table of contents
(setq org-export-with-toc nil)

;;; display Info mode buffers in proportional font
(add-hook 'Info-mode-hook 'variable-pitch-mode)

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                            TODO items config                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "VERIFY" "DONE")
        (sequence "REPORT" "BUG" "KNOWNCAUSE" "FIXED")
        )
      )

(setq org-todo-keyword-faces
      '(("TODO" . "red")
        ("STARTED" . "yellow")
        ("VERIFY". "grey")
        ("DONE" . "green")
        )
      )

;; Automatically insert date stamp when a todo item goes to done state
(setq org-log-done 'time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""  Original source code with one modified row. Makes the background of      """
"""  images white so that transparent images are visible                      """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""  Latex export config                                                      """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; (setq org-latex-pdf-process
;;       '("xelatex -interaction nonstopmode -output-directory %o %f"
;;         "bibtex %b"
;;         "xelatex -interaction nonstopmode -output-directory %o %f"
;;         "xelatex -interaction nonstopmode -output-directory %o %

(setq latex-run-command "pdflatex")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""  Latex export filters                                                     """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ox-mrkup-filter-body
    (text back-end info)
  (format "<body>%s</body>" text))
(defun ox-mrkup-filter-bold
    (text back-end info)
  "Markup TEXT as <bold>TEXT</bold>. Ignore BACK-END and INFO."
  (format "<bold>%s</bold>" text))
(defun ox-mrkup-filter-babel-call
    (text back-end info)
  (format "<bbl>%s</bbl>" text))
(defun ox-mrkup-filter-center-block
    (text back-end info)
  (format "<cnt>%s</cnt>" text))
(defun ox-mrkup-filter-clock
    (text back-end info)
  (format "<clck>%s</clck>" text))
(defun ox-mrkup-filter-code
    (text back-end info)
  (format "<code>%s</code>" text))
(defun ox-mrkup-filter-comment
    (text back-end info)
  (format "<cmmn>%s</cmmn>" text))
(defun ox-mrkup-filter-comment-block
    (text back-end info)
  (format "<cmm>%s</cmm>" text))
(defun ox-mrkup-filter-diary-sexp
    (text back-end info)
  (format "<dry>%s</dry>" text))
(defun ox-mrkup-filter-drawer
    (text back-end info)
  (format "<drwr>%s</drwr>" text))
(defun ox-mrkup-filter-dynamic-block
    (text back-end info)
  (format "<dyn>%s</dyn>" text))
(defun ox-mrkup-filter-entity
    (text back-end info)
  (format "<entt>%s</entt>" text))
(defun ox-mrkup-filter-example-block
    (text back-end info)
  (format "<exm>%s</exm>" text))
(defun ox-mrkup-filter-export-block
    (text back-end info)
  (format "<exprt-b>%s</exprt-b>" text))
(defun ox-mrkup-filter-export-snippet
    (text back-end info)
  (format "<exprt-s>%s</exprt-s>" text))
(defun ox-mrkup-filter-final-output
    (text back-end info)
  (format "<fnl>%s</fnl>" text))
(defun ox-mrkup-filter-fixed-width
    (text back-end info)
  (format "<fxd>%s</fxd>" text))
(defun ox-mrkup-filter-footnote-definition
    (text back-end info)
  (format "<ftnt-d>%s</ftnt-d>" text))
(defun ox-mrkup-filter-footnote-reference
    (text back-end info)
  (format "<ftnt-r>%s</ftnt-r>" text))
(defun ox-mrkup-filter-headline
    (text back-end info)
  (format "<hdln>%s</hdln>" text))
(defun ox-mrkup-filter-horizontal-rule
    (text back-end info)
  (format "<hrz>%s</hrz>" text))
(defun ox-mrkup-filter-inline-babel-call
    (text back-end info)
  (format "<inln-b>%s</inln-b>" text))
(defun ox-mrkup-filter-inline-src-block
    (text back-end info)
  (format "<inln-s>%s</inln-s>" text))
(defun ox-mrkup-filter-inlinetask
    (text back-end info)
  (format "<inln>%s</inln>" text))
(defun ox-mrkup-filter-italic
    (text back-end info)
  (format "<itlc>%s</itlc>" text))
(defun ox-mrkup-filter-item
    (text back-end info)
  (format "<item>%s</item>" text))
(defun ox-mrkup-filter-keyword
    (text back-end info)
  (format "<kywr>%s</kywr>" text))
(defun ox-mrkup-filter-latex-environment
    (text back-end info)
  (format "<ltx-n>%s</ltx-n>" text))
(defun ox-mrkup-filter-latex-fragment
    (text back-end info)
  (format "<ltx-f>%s</ltx-f>" text))
(defun ox-mrkup-filter-line-break
    (text back-end info)
  (format "<ln-b>%s</ln-b>" text))
(defun ox-mrkup-filter-link
    (text back-end info)
  (format "<link>%s</link>" text))
(defun ox-mrkup-filter-node-property
    (text back-end info)
  (format "<nd-p>%s</nd-p>" text))
;; dont (defun ox-mrkup-filter-options ...)
(defun ox-mrkup-filter-paragraph
    (text back-end info)
  (format "<prgr>%s</prgr>" text))
;; dont (defun ox-mrkup-filter-parse-tree ...)
(defun ox-mrkup-filter-plain-list
    (text back-end info)
  (format "<pln-l>%s</pln-l>" text))
(defun ox-mrkup-filter-plain-text
    (text back-end info)
  (format "<pln-t>%s</pln-t>" text))
(defun ox-mrkup-filter-planning
    (text back-end info)
  (format "<plnn>%s</plnn>" text))
(defun ox-mrkup-filter-property-drawer
    (text back-end info)
  (format "<prp>%s</prp>" text))
(defun ox-mrkup-filter-quote-block
    (text back-end info)
  (format "<qt-b>%s</qt-b>" text))
(defun ox-mrkup-filter-radio-target
    (text back-end info)
  (format "<rd-t>%s</rd-t>" text))
(defun ox-mrkup-filter-section
    (text back-end info)
  (format "<sctn>%s</sctn>" text))
(defun ox-mrkup-filter-special-block
    (text back-end info)
  (format "<spc>%s</spc>" text))
(defun ox-mrkup-filter-src-block
    (text back-end info)
  (format "<src>%s</src>" text))
(defun ox-mrkup-filter-statistics-cookie
    (text back-end info)
  (format "<stt>%s</stt>" text))
(defun ox-mrkup-filter-strike-through
    (text back-end info)
  (format "<str>%s</str>" text))
(defun ox-mrkup-filter-subscript
    (text back-end info)
  (format "<sbsc>%s</sbsc>" text))
(defun ox-mrkup-filter-superscript
    (text back-end info)
  (format "<sprs>%s</sprs>" text))
(defun ox-mrkup-filter-table
    (text back-end info)
  (format "<tabl>%s</tabl>" text))
(defun ox-mrkup-filter-table-cell
    (text back-end info)
  (format "<tbl-c>%s</tbl-c>" text))
(defun ox-mrkup-filter-table-row
    (text back-end info)
  (format "<tbl-r>%s</tbl-r>" text))
(defun ox-mrkup-filter-target
    (text back-end info)
  (format "<trgt>%s</trgt>" text))
(defun ox-mrkup-filter-timestamp
    (text back-end info)
  (format "<tmst>%s</tmst>" text))
(defun ox-mrkup-filter-underline
    (text back-end info)
  (format "<undr>%s</undr>" text))
(defun ox-mrkup-filter-verbatim
    (text back-end info)
  (format "<vrbt>%s</vrbt>" text))
(defun ox-mrkup-filter-verse-block
    (text back-end info)
  (format "<vrs>%s</vrs>" text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""  Add latex classes                                                        """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(use-package org-bullets
  :ensure t)

(provide 'org-init)
