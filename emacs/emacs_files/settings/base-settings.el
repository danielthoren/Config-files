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

;; Autofollow symlinks
(setq vc-follow-symlinks t)

;; Remvoe trailing whitespace when save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable autosaving
(setq auto-save-default nil)
;;(setq backup-directory-alist '(("" . (expand-file-name "backup" user-emacs-directory))))

;;Limit savehisp-mode length to reduce lag
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

;; Auto update when files change on disk
(global-auto-revert-mode 1)

;; Set indent mode to use spaces instead of tabs
;; (setq-default tab-width 2)
;; (setq-default tab-stop-list (number-sequence 2 200 2))

(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 200 4))

(setq-default indent-tabs-mode nil)

;;disable the version control
(setq vc-handled-backends nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll settings to make scrolling more smooth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight comments in prog mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hl-line-mode 0)

(provide 'base-settings)
