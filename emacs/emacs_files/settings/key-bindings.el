;; Buffer switching.
(global-set-key (kbd "C-M-.") 'next-buffer)
(global-set-key (kbd "C-M-,") 'previous-buffer)

;; Window switching.
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

;; Unbind pageup and pagedown
(global-unset-key (kbd "<next>"))
(global-unset-key (kbd "<prior>"))

;; Don't show buffer kill prompt.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Allows reopening recently killed buffers with C-x j and C-x C-j
(global-set-key (kbd "C-x j") 'reopen-killed-file)
(global-set-key (kbd "C-x C-j") 'reopen-killed-file-fancy)

;; Ivy mode M-x
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-;") 'company-complete-common)
(global-set-key (kbd "C-c k") 'comment-or-uncomment-region)

;;Neotree
(global-set-key (kbd "C-x t") 'neotree-toggle)

(provide 'key-bindings)
