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


;; Move x lines up at a time
(global-set-key (kbd "M-p")
  (lambda ()
    (interactive)
    (setq this-command 'previous-line)
    (previous-line 5)))

;; Move x lines down at a time
(global-set-key (kbd "M-n")
  (lambda ()
    (interactive)
    (setq this-command 'next-line)
    (next-line 5)))


;; Move borders between windows
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)


(provide 'key-bindings)
