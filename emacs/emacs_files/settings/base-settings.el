;; Remove GUI bloat
(tool-bar-mode -1) 
(menu-bar-mode -1) 
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

;; Line numbering
(global-linum-mode t)

;; Column numbering
(setq column-number-mode t)

;; Autofollow symlinks
(setq vc-follow-symlinks t)

;; Disable autosaving
(setq auto-save-default nil)
;;(setq backup-directory-alist '(("" . (expand-file-name "backup" user-emacs-directory))))


(provide 'base-settings)
