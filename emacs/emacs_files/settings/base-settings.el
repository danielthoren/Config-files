
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                               Base settings                               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change default wolumn width
(setq-default fill-column 100)

(add-to-list 'image-types 'svg)

;; Autofollow symlinks
(setq vc-follow-symlinks t)

;; Remvoe trailing whitespace when save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable autosaving
(setq auto-save-default nil)

;;Limit savehisp-mode length to reduce lag
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

;; Auto update when files change on disk
(global-auto-revert-mode)
(setq auto-revert-mode t)

;; Set indent mode to use spaces instead of tabs
;; (setq-default tab-width 2)
;; (setq-default tab-stop-list (number-sequence 2 200 2))

(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode nil)

;;disable the version control
(setq vc-handled-backends nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                               GUI settings                                """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove GUI bloat
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

;; Line numbering
(if (version< emacs-version "26")
    (global-linum-mode)
  (global-display-line-numbers-mode))

;; Column numbering
(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                                  Opacity                                  """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-frame-parameter (selected-frame) 'alpha '(99 . 99))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

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
	 '(95 . 95) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(toggle-transparency)
;; (toggle-transparency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                              Scroll settings                              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make scrolling more smooth
(setq
 scroll-conservatively 1000                     ;; only 'jump' when moving this far
 scroll-margin 4                                ;; scroll N lines to screen edge
 scroll-step 1                                  ;; keyboard scroll one line at a time
 mouse-wheel-scroll-amount '(6 ((shift) . 1))   ;; mouse scroll N lines
 mouse-wheel-progressive-speed nil              ;; don't accelerate scrolling

 redisplay-dont-pause t                         ;; don't pause display on input

 ;; Always redraw immediately when scrolling,
 ;; more responsive and doesn't hang!
 fast-but-imprecise-scrolling nil
 jit-lock-defer-time 0
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                                ediff mode                                 """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (custom-set-faces
;;  '(ediff-current-diff-B ((t (:foreground "white" :background "DarkRed")))))

(custom-set-faces
 '(ediff-current-diff-A ((t (:background "black")))))

(custom-set-faces
 '(ediff-even-diff-A  ((t ( :background "#263854")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                                 diff mode                                 """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute 'diff-added nil
                      :foreground "white" :background "DarkGreen")
  (set-face-attribute 'diff-removed nil
                      :foreground "white" :background "DarkRed")
  (set-face-attribute 'diff-changed nil
                      :foreground "white" :background "purple"))
(eval-after-load "diff-mode"
  '(update-diff-colors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                        Yank behavior modification                         """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change behavior of yank pointer when calling yank-pop. By default,
;; the pointer is moved to the yanked element, this mod changes that
;; behavior so that the pointer is not moved.

(defvar my-yank-pop-offset)
(setq my-yank-pop-offset 0)

(defun my-yank (&optional arg)
  (interactive)
  (rotate-yank-pointer (- my-yank-pop-offset))
  (setq my-yank-pop-offset 0)
  (yank arg)
  )

(defun my-yank-pop (&optional arg)
  (interactive)
  (setq my-yank-pop-offset (+ my-yank-pop-offset 1))
  (yank-pop arg)
  )

(global-set-key (kbd "C-y") 'my-yank)
(global-set-key (kbd "M-y") 'my-yank-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hl-line-mode 0)

(provide 'base-settings)
