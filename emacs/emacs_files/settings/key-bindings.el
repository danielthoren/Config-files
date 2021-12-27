;; Buffer switching.
(global-set-key (kbd "C-M-.") 'next-buffer)
(global-set-key (kbd "C-M-,") 'previous-buffer)

;; Previous window
(defun prev-window ()
  (interactive)
  (other-window -1))

;; Window switching.
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

;; Unbind pageup and pagedown
(global-unset-key (kbd "<next>"))
(global-unset-key (kbd "<prior>"))

;; Don't show buffer kill prompt.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "C-c k") 'comment-or-uncomment-region)

;;Neotree
(global-set-key (kbd "C-x t") 'neotree-toggle)

(provide 'key-bindings)
