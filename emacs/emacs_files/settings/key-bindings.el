;; Buffer switching.
(global-set-key (kbd "C-M-.") 'next-buffer)
(global-set-key (kbd "C-M-,") 'previous-buffer)

;; Window switching.
(global-set-key (kbd "C-.") 'other-window)
;;(global-set-key (kbd "C-,") 'prev-window)
(bind-key* "C-," 'prev-window)

;; Unbind pageup and pagedown
(global-unset-key (kbd "<next>"))
(global-unset-key (kbd "<prior>"))

;; Don't show buffer kill prompt.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Poker3 bindings
;; (global-set-key (kbd "H-i") 'previous-line)
;; (global-set-key (kbd "H-k") 'next-line)
;; (global-set-key (kbd "H-l") 'right-char)
;; (global-set-key (kbd "H-j") 'left-char)
;; (global-set-key (kbd "H-M-i") 'backward-paragraph)
;; (global-set-key (kbd "H-M-k") 'forward-paragraph)
;; (global-set-key (kbd "H-M-l") 'right-word)
;; (global-set-key (kbd "H-M-j") 'left-word)

;; Allows reopening recently killed buffers with C-x j and C-x C-j
(global-set-key (kbd "C-x j") 'reopen-killed-file)
(global-set-key (kbd "C-x C-j") 'reopen-killed-file-fancy)

;; Ivy mode M-x
(global-set-key (kbd "M-x") 'counsel-M-x)

(global-set-key (kbd "C-;") 'company-complete-common)

(global-set-key (kbd "C-c k") 'comment-or-uncomment-region)

(provide 'key-bindings)
