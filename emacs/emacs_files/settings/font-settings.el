
(if (string-equal system-name "maxwell") ;; Thinkpad (low dpi)
    (progn 
      (set-default-font "Input Mono")
      (set-face-attribute 'default nil :height 100 :weight 'bold))
  
  (if (string-equal system-name "tesla") ;; Home (high dpi)
      (progn
	(set-default-font "Input Mono")
	(set-face-attribute 'default nil :height 110  :weight 'bold))))


(provide 'font-settings)
