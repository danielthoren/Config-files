;; FIXME: Bug with offset from prefix. Toggle centering does not add the same
;;        offset as insert new line

;; FIXME: implement offset between top enclose body and bottom enclose

;; FIXME: Make funciton "block-comment--align-body-width" take centering order into consideration

;; TODO: Detect block comment style automatically

;; TODO: Hold relative position of point when aligning comment text

;; TODO: Make all rows extend when one row extends in width
;;       Make function that does this

;;;;;;;;;;;;;;;;;;;;;;;; Release 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Add toggling between different lengths of block comments

;; TODO: Implement automatic block comment width detection

;; TODO: Add automatic row breaking when block comment is longer
;;       than 80 characters

;; TODO: Make block comment width indentation sensative, meaning that it does
;;       not exceed a strict width limit (80 characters)

(provide 'block-comment-mode)

(define-minor-mode block-comment-mode
  "Toggle block comments mode"
  :init-value nil
  :lighter "[Block-comment-mode]"
    :keymap (let ((map (make-sparse-keymap)))
            ;; press C-g to abort comment mode
            (define-key map (kbd "C-g") 'block-comment-abort)
            (define-key map (kbd "RET") 'block-comment-abort)
            (define-key map (kbd "M-j") 'block-comment-newline)
            (define-key map (kbd "C-c C-c") 'block-comment-toggle-centering)
            (define-key map (kbd "TAB") 'block-comment-format-comment)
            map)

    (if block-comment-mode
    (block-comment--add-hooks)
    (block-comment--shutdown)
    )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Functions bound to keys                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment-abort ()
  """ Turns block-comment-mode off """
  (interactive)
  (block-comment-mode 0))

(defun block-comment-newline ()
  """ Inserts a new line and moves text to the right of point down"""
  (interactive)

  (let (
        (remain-text-start (point-marker))
        (remain-text-end nil)
        (remain-text nil)
        )

    (block-comment--jump-to-last-char-in-body)
    (setq remain-text-end (point-marker))

    ;; Delete remaining text between point and end of body
    (setq remain-text (delete-and-extract-region remain-text-start
                                                 remain-text-end))

    (block-comment--remove-hooks)

    (end-of-line)
    (insert "\n")
    (block-comment--indent-accoring-to-previous-block-row)
    (block-comment--insert-new-line)

    (block-comment--add-hooks)

    ;; If there is text to the right of point, reinsert the deleted text
    (unless (= 0 (string-match "[ \t]*$" remain-text))
      (insert remain-text)
      )
    )

  ;; (block-comment--align-width)
  )

(defun block-comment-toggle-centering ()
  """ Toggles centering mode """
  (interactive)
  (if block-comment-centering-enabled
      (setq block-comment-centering-enabled nil) ;; If enabled , disable
    (setq block-comment-centering-enabled t)     ;; If disabled, enabled
    )

  ;; Set order to right side (end of comment)
  (unless block-comment-centering-enabled
    (setq block-comment-centering--order 1)
    )
  ;; Align text
  (block-comment--align-text block-comment-centering-enabled)
  )

(defun block-comment-format-comment ()
  (interactive)
  """  Formats the current block comment, doing the following:                 """
  """       - Aligns block comment width                                       """
  """       - Aligns block comment text                                        """
  ;; (block-comment--align-all-text)
  (block-comment--align-width)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Interactive functions                           """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--insert-or-resume ()
  """ This function is called to create or resume a block comment           """
  """ Checks if point is inside block comment or not.                       """
  """ If it is, resumeprevious block comment, else start new block comment  """
  (interactive)

  ;;Check if in block comment
  (if (block-comment--is-body nil)
      (progn
        (block-comment--resume t)      ;; If t, resume with jump back condition
        (block-comment-format-comment) ;; Auto format comment
        (if (block-comment--has-comment)
            (block-comment--jump-to-last-char-in-body)
          (progn
            (if block-comment-centering-enabled
                (block-comment--jump-to-body-center)
              (block-comment--jump-to-body-start)
            ))
        ))
    (block-comment--insert)      ;; Else insert
    )

  ;; enter centering mode
  (block-comment-mode 1)
  )

(defun block-comment--init-comment-style (
                                          width
                                          prefix
                                          fill
                                          postfix
                                          enclose-prefix
                                          enclose-fill
                                          enclose-postfix
                                          &optional centering-default
                                                    enclose-prefix-bot
                                                    enclose-fill-bot
                                                    enclose-postfix-bot)
  """ Initializes variables of block-comment-mode                             """
  """ This should be called during initialization of each mode where block-   """
  """ comment-mode shall be used. Default behaviour is c/c++ comment style    """
  """    -> centering-default : If t, then centers by default                 """
  """    -> enclose-prefix-bot :                                              """
  """    -> enclose-fill-bot :                                                """
  """    -> enclose-postfix-bot : If present, then a different set of         """
  """                             variables are used for the bottom enclose   """
  """                             than the top. If not, then the same         """
  """                             settings are used for both top and bottom   """

  (unless centering-default
    (setq centering-default nil)
    )

  (unless enclose-prefix-bot
    (setq enclose-prefix-bot enclose-prefix)
    (setq enclose-fill-bot enclose-fill)
    (setq enclose-postfix-bot enclose-postfix)
    )

  (set (make-local-variable 'block-comment-width) width)

  (set (make-local-variable 'block-comment-prefix) prefix)
  (set (make-local-variable 'block-comment-fill) fill)
  (set (make-local-variable 'block-comment-postfix) postfix)

  (set (make-local-variable 'block-comment-enclose-prefix-top) enclose-prefix)
  (set (make-local-variable 'block-comment-enclose-fill-top) enclose-fill)
  (set (make-local-variable 'block-comment-enclose-postfix-top) enclose-postfix)

  (set (make-local-variable 'block-comment-enclose-prefix-bot) enclose-prefix-bot)
  (set (make-local-variable 'block-comment-enclose-fill-bot) enclose-fill-bot)
  (set (make-local-variable 'block-comment-enclose-postfix-bot) enclose-postfix-bot)

  ;; Used to remember if is centering or not
  (set (make-local-variable 'block-comment-centering-enabled) centering-default)
  ;; Sets the target spacing between pre/postfix and user comment
  (set (make-local-variable 'block-comment-edge-offset) 2)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Startup/shutdown logic                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--init-variables ()
  """ Init function run when block-comment-mode is started.                   """
  """ Sets default values for variables                                       """
  (unless (boundp 'block-comment-prefix)
    (block-comment--init-comment-style 20   "/*" " " "*/"    "/*" "*" "*/" ))

  (set (make-local-variable 'block-comment-centering--start-pos) nil)
  (set (make-local-variable 'block-comment-centering--end-pos) nil)
  (set (make-local-variable 'block-comment-centering--order) 1)
  (set (make-local-variable 'block-comment-centering--left-offset) 0)
  (set (make-local-variable 'block-comment-centering--right-offset) 0)
  ;; If the diff between left/right margins for a row is smaller than this value,
  ;; then we assume that we are in centering mode
  (set (make-local-variable 'block-comment--centering-diff-thresh) 10)
  )

(defun block-comment--shutdown ()
  """ Turns block comment off by removing the hooks """
  (block-comment--remove-hooks)
  (block-comment--init-variables)
  )

(defun block-comment--remove-hooks ()
  """  Adds necessasry hooks                                                  """
  (setq post-command-hook
        (delete #'block-comment-centering--cursor-moved post-command-hook))
  (setq after-change-functions
        (delete #'block-comment-centering--edit after-change-functions))
  )

(defun block-comment--add-hooks ()
  """   Adds necessary hooks so that block-comment-mode can react to          """
  """   changes in the buffer                                                 """
  ;; Keep track of the cursors position, if it leaves the block comment
  ;; then abort the centering mode)
  (add-to-list 'post-command-hook #'block-comment-centering--cursor-moved)

  ;; Add a hook that is called everytime the buffer is modified
  (add-to-list 'after-change-functions #'block-comment-centering--edit)
  )

(defun block-comment--resume (&optional jump-back)
  """ Resumes block comment mode using existing block comment   """
  """ If 'jump-back' is t, jumps to end of comment inside block """
  """ else, inits block comment mode at point                   """

  ;; init the centering mode without activating it
  (block-comment--init-variables)

  (save-excursion

    ;; store the beginning of the block comment
    (beginning-of-line)
    (block-comment--jump-to-body-start 0)
    (backward-char 1)
    (setq block-comment-centering--start-pos (point-marker))

    (end-of-line)
    (block-comment--jump-to-body-end 0)
    (forward-char 1)
    (setq block-comment-centering--end-pos (point-marker))
    )

  ;; If there is a user comment, jump to end of said comment
  ;; If there is no user comment, jump to center if centering,
  ;;                              else jump to start
  (when jump-back
    (if (block-comment--has-comment)
        (block-comment--jump-to-last-char-in-body)
      (if block-comment-centering-enabled
          (block-comment--jump-to-body-center)
        (block-comment--jump-to-body-start)
        )
      )
    )
  )

(defun block-comment-centering--cursor-moved ()
  """ This function is triggered by a hook every time point has moved        """
  """ Used to abort block-comment-mode if cursor is outside of block comment """
  (let* (
         (start (marker-position block-comment-centering--start-pos))
         (end (marker-position block-comment-centering--end-pos))
         (cur (point-marker))
         )

    (if (or (< cur start) (< end cur))  ;; If outside of row boundry
        (if (block-comment--is-body t)  ;; If still in a block comment body
            (block-comment--resume nil) ;; Run resume on new line to continue
          (block-comment-mode 0)  ;; If not on block comment body, exit centering
          )
    )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Insert functions                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--insert ()
  """ Inserts a new block comment and init centering """

  ;; Insert top enclose
  (block-comment--insert-enclose block-comment-enclose-prefix-top
                                 block-comment-enclose-fill-top
                                 block-comment-enclose-postfix-top)

  (insert "\n")
  (block-comment--indent-accoring-to-previous-block-row)
  (block-comment--insert-new-line)

  ;; Insert bottom enclose
  (save-excursion
    (end-of-line)
    (insert "\n")
    (block-comment--indent-accoring-to-previous-block-row)
    (block-comment--insert-enclose block-comment-enclose-prefix-bot
                                   block-comment-enclose-fill-bot
                                   block-comment-enclose-postfix-bot)
    )
  )


(defun block-comment--insert-new-line ()
  (interactive)
  """    Inserts a block comment body line below point, at the             """
  """    current indentation level and initializes centering               """

  ;; init the centering mode without activating it
  (block-comment--init-variables)

  ;; Insert new line with same indent
  (block-comment--insert-line)

  ;; Set comment body start pos
  (save-excursion
    (block-comment--jump-to-body-start 0)
    (setq block-comment-centering--start-pos (point-marker))
    )

  (save-excursion
    (block-comment--jump-to-body-end 0)
    (setq block-comment-centering--end-pos (point-marker))
    )

  ;; Jump to center of user comment if centering enabled,
  ;; else jump to beginning of user comment
  (if block-comment-centering-enabled
      (block-comment--jump-to-body-center)
    (block-comment--jump-to-body-start)
    )
  )


(defun block-comment--insert-line ()
  """ Inserts a new block comment body line at point with 'indent-level' """
  (let* (
         (fill-count (- block-comment-width
                        (+ (current-column)
                           (string-width block-comment-prefix)
                           (string-width block-comment-postfix)
                           )
                        )
                     )
         )

    (save-excursion
      ;; Insert the comment body
      (insert block-comment-prefix)
      (insert (make-string fill-count (string-to-char block-comment-fill)))
      (insert block-comment-postfix)

      ;; Return end of block comment
      (block-comment--jump-to-body-end 0)
      (point-marker)
      )
    )
  )

(defun block-comment--insert-enclose (prefix fill postfix)
  """ Inserts a enclosing line at point                                      """
  """ A enclosing line is a line inserted before and                         """
  """ after the block comment body                                           """

  (let* (
         (target-width (- block-comment-width (current-column)))
         (padding-length (- target-width
                            (+ (string-width prefix)
                               (string-width postfix)
                               )
                            )
                         )
         (padding (make-string padding-length
                               (string-to-char fill))
                  )
         )
    (insert prefix)
    (insert padding)
    (insert postfix)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Centering logic                              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment-centering--edit (begin end length)
  """   This function is triggered by a hook every time the user has         """
  """   inserted/removed characters. It checks if the user removed or added  """
  """   characters, then decides which side of the blockc omment should be   """
  """   affected. The rest of the work is delegated                          """
  (let* (
         (step (- (- end begin) length))
         (min-step (/ step 2))
         (max-step (- step min-step))

         (left  (if (= block-comment-centering--order 0) max-step min-step))
         (right (if (= block-comment-centering--order 0) min-step max-step))
         )

    (if (< step 0)
        (block-comment-centering--removed-chars block-comment-centering--order
                                                block-comment-centering-enabled)
      (progn
        ;; If centering is not enabled, only remove from right side
        ;; of user comment
        (unless block-comment-centering-enabled
          (setq left 0)
          (setq right step)
          )
        (block-comment-centering--inserted-chars left right))
      )

    ;; Alternate between putting larger step on left/right side
    ;; if centering is enabled
    (when block-comment-centering-enabled
      (setq block-comment-centering--order
            (- 1 block-comment-centering--order))
      )
    )
  )

(defun block-comment-centering--removed-chars (curr-side centering)
  """ Handles when the user removes characters. Inserts padding on right and  """
  """ left side. When user comment is wider than target width,                """
  """ no padding is inserted.                                                 """
  """ curr-side : The side that should get the largest fill count:            """
  """             0 -> left side (start of body)                              """
  """             1 -> right side (end of body)                               """
  """ centering : If t, the block comment is centered, else not               """
  (save-excursion

    (let* (
           ;; Get line width after change
           (line-width (progn
                         (block-comment--jump-to-comment-end)
                         (current-column)
                         )
                       )
           ;; Get the removed width
           (removed-width (- block-comment-width
                             line-width)
                          )
           )

      (while (> removed-width 0)
        ;; Alternate between right and left side
        (if (= curr-side 0)
            (block-comment--jump-to-body-start 0)
              (block-comment--jump-to-body-end 0)
              )
        ;; Insert the fill and substract fill from removed-width
        (insert block-comment-fill)
        (setq removed-width (- removed-width
                               (string-width block-comment-fill)
                               )
              )
        ;; Only alternate if centering is enabled
        (when centering
          (setq curr-side (- 1 curr-side))
          )
        )
      )
    )
  )

(defun block-comment-centering--inserted-chars (left right)
  """   Handles when user inserts characters. Removes padding on right and """
  """  left side. If user comment grows larger than target width,          """
  """   stops removing characters                                          """
  (let (
        (remain-space-left 0)
        (remain-space-right 0)
        (line-width 0)
        (edge-offset block-comment-edge-offset)
        )

    ;; Make edge offset 1 char larger when centering to make it easier to
    ;; differentiate between the modes
    (when block-comment-centering-enabled
      (setq edge-offset (+ edge-offset 1))
      )

    (save-excursion

      ;; Set line width for this row
      (save-excursion

      (end-of-line)
      (setq line-width (current-column))
      )

      ;; Get space remaining on right
      (save-excursion
        (setq remain-space-right
              (block-comment--jump-to-last-char-in-body)
              )
        )

      ;; Get space remaining on left
      (save-excursion
        (setq remain-space-left
              (block-comment--jump-to-first-char-in-body)
              )
        )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;; Left side ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; If no space on left side, perform operation on right side instead
      (when (<= remain-space-left edge-offset)
        (setq right (+ right left))
        (setq left 0)
        )

      ;; Remove characters at beginning of line
      (block-comment--jump-to-body-start 0)
      (delete-char left)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;; Right side ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Remove characters at end of line
      (block-comment--jump-to-body-end 0)

      (if (<= remain-space-right edge-offset)
          ;; If there is no space left, make more space
          (progn
            (insert (make-string right
                                 (string-to-char block-comment-fill))
                    )
            ;; Update end of block comment to avoid aborting block comment mode
            (setq block-comment-centering--end-pos (point-marker))
            )
          ;; If there is space left, remove the right portion
          (delete-backward-char right)
          )

      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Text alignment functions                         """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--align-all-text ()
  """  Align text position of all block comment rows using auto detection      """
  """  for centering/non-centering                                             """
  (save-excursion
    ;; Jump to the postamble row, the row right beneath the last comment body
    (block-comment--jump-below-comment -1)

    (while (progn
             ;; Move up one line
             (block-comment--move-line -1)

             ;; Check if this is body or enclose
             (block-comment--is-body nil)
             )
      (block-comment--align-text (block-comment--is-centering-row))
      )
    )
  )

(defun block-comment--align-text (centering)
  """   Aligns the text in the comment body, centering it if param            """
  """   'centering' is t, else aligning to the left.                          """
  """   If there is no user comment in body, put point at appropriate pos     """
  (block-comment--remove-hooks)
  (let (
        (comment-text-start nil)
        (comment-text-end nil)
        (comment-text nil)
        )
    (if (block-comment--has-comment) ;; Align text if there is any
        (progn
          (block-comment--jump-to-first-char-in-body)
          (setq comment-text-start (point-marker))
          (block-comment--jump-to-last-char-in-body)
          (setq comment-text-end (point-marker))
          (setq comment-text (delete-and-extract-region comment-text-start
                                           comment-text-end))

          (if centering
              (block-comment--jump-to-body-center)
            (block-comment--jump-to-body-start)
            )

          (insert comment-text)
          )
      (if centering ;; If there is no text, move point to appropriate place
          (block-comment--jump-to-body-center)
        (block-comment--jump-to-body-start)
        )
      )
    )
  (block-comment--add-hooks)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                       Width alignment functions                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--align-width ()
"""  Aligns the width of all rows in accordance with the widest row          """
  (let (
        (start-pos (point-marker))
        (target-width (+ (block-comment--get-widest-comment-text)
                         (* (+ block-comment-edge-offset 1) 2)
                         (string-width block-comment-prefix)
                         (string-width block-comment-postfix))
                      )
        (indentation (block-comment--get-indent-level))
        )

    ;; Dont make width less than target minus indentation
    (when (< target-width (- block-comment-width indentation))
      (setq target-width (- block-comment-width indentation))
      )

    ;; Disable hooks to disable centering when adjusting width
    (block-comment--remove-hooks)

    (save-excursion
      (block-comment--jump-below-comment)
      (block-comment--adjust-rows-above-to-target-width target-width)
      )
    )

  ;; Re-enable hooks
  (block-comment--add-hooks)
  )

(defun block-comment--adjust-rows-above-to-target-width (target-width)
  """  Aligns all block comment rows above to the given target width           """
  """  Param 'target-width': The width to align to                             """
  (let (
        (curr-width 0)
        (width-diff 0)
        (is-body nil)
        (is-enclose nil)
        )

    ;; Align all block comment rows above
    (while (progn
             ;; Move up one line
             (block-comment--move-line -1)

             ;; Check if this is body or enclose
             (setq is-body (block-comment--is-body nil))
             (setq is-enclose-top (block-comment--is-enclose-top nil))
             (setq is-enclose-bot (block-comment--is-enclose-bot nil))

             ;; Exit if not in body
             (or is-body is-enclose-top is-enclose-bot)
             )

      ;; When normal block comment line
      (when is-body
        (setq curr-width (block-comment--get-width nil
                                                   block-comment-prefix
                                                   block-comment-postfix))
        (setq width-diff (- target-width curr-width))

        (message "target width: %d curr width: %d width diff: %d" target-width curr-width width-diff)

        (block-comment--align-body-width width-diff
                                         block-comment-fill)
        ) ;; When is-body

      ;; When enclose-top
      (when is-enclose-top
        (setq curr-width (block-comment--get-width nil
                                                   block-comment-enclose-prefix-top
                                                   block-comment-enclose-postfix-top))
        (setq width-diff (- target-width curr-width))

        (message "target width: %d curr width: %d width diff: %d" target-width curr-width width-diff)

        (block-comment--align-enclose-width width-diff
                                            block-comment-enclose-fill-top)
        ) ;; When enclose-top

      ;; When enclose-bot
      (when is-enclose-bot
        (setq curr-width (block-comment--get-width nil
                                                   block-comment-enclose-prefix-bot
                                                   block-comment-enclose-postfix-bot))
        (setq width-diff (- target-width curr-width))

        (message "target width: %d curr width: %d width diff: %d" target-width curr-width width-diff)

        (block-comment--align-enclose-width width-diff
                                            block-comment-enclose-fill-bot)
        ) ;; When enclose-bot
      ) ;; while
    )
  )

;; FIXME: Make funciton "block-comment--align-body-width" take centering order into consideration
(defun block-comment--align-body-width (width-diff fill)
  """  Changes the block comment width  'width-diff' characters, inserting     """
  """  if the diff is positive and removing if it is positive.                 """
  """  If inserting, then inserts half at beginning, and half at the end of    """
  """  comment body. Takes the centering mode indo consideration.              """
  """  Param 'width-diff': How much the width should change, increases if      """
  """                      positive, decreases if negative                     """
  """  Param 'fill'      : The char to fill with                               """
  """  OBS: This function assumes that the block comment body fits inside the  """
  """  new boundry!                                                            """
  (let* (
         (step (abs width-diff))
         (min-step (/ step 2))
         (max-step (- step min-step))

         (left  max-step)
         (right min-step)
         )

    (if (block-comment--is-centering-row)
        ;; When centering, move text to center to avoid truncating text
        (progn
          (block-comment--align-text t)
          (block-comment--remove-hooks)
          )
      ;; When not centering, only remove from the right if possible
      (let (
            (remain-left (block-comment--jump-to-first-char-in-body))
            (remain-right (block-comment--jump-to-last-char-in-body 0))
            )
        ;; When negative width diff, try to remove as many characters as
        ;; possible form the right to keep the formatting
        (when (< width-diff 0)
          (setq right (if (> remain-right step) step remain-right))
          (setq left (- step right))
          )
        ;; When positive width diff, add all to the right
        (when (> width-diff 0)
          (setq right (+ left right))
          (setq left 0)
          )
        )
      )

    ;; If width should increase
    (when (> width-diff 0)
      (block-comment--jump-to-body-start 0)
      (insert (make-string left
                           (string-to-char fill)
                           )
              )

      (block-comment--jump-to-body-end 0)
      (insert (make-string right
                           (string-to-char fill)
                           )
              )
      ) ;; End when width-diff positive

    (when (< width-diff 0)
      (block-comment--jump-to-body-start 0)
      (delete-forward-char left)

      (block-comment--jump-to-body-end 0)
      (delete-backward-char right)
      )
    ) ;; End when width-diff negative
  )

(defun block-comment--align-enclose-width (width-diff fill)
  """  Changes the block comment width  'width-diff' characters, inserting     """
  """  if the diff is positive and removing if it is positive.                 """
  """  Param 'width-diff': How much the width should change, increases if      """
  """                      positive, decreases if negative                     """
  """  Param 'fill'      : The char to fill with                               """
  """  OBS: This function assumes that the block comment body fits inside the  """
  """  new boundry!                                                            """
  (let* (
         (step (abs width-diff))
         (min-step (/ step 2))
         (max-step (- step min-step))
         )
    (block-comment--jump-to-body-center)

    ;; If width should increase
    (when (> width-diff 0)
      (insert (make-string step
                           (string-to-char fill)
                           )
              )
      ) ;; End when width-diff positive

    (when (< width-diff 0)
      (delete-forward-char min-step)
      (delete-backward-char max-step)
      )
    ) ;; End when width-diff negative
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Helper functions                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--indent-accoring-to-previous-block-row ()
  """  Indent current row in accordance with the block comment row on           """
  """  the previous line. Auto detects if previous line is body or enclose-top  """
  """  -> return: Current indentation level                                     """
  (block-comment--move-line -1)

  (let (
        (indent-level 0)
        (prefix block-comment-prefix)
        )

    ;; Detect if previous line is enclose. Not looking for bottom enclose since
    ;; we never indent relative to thet
    (when (block-comment--is-enclose-top)
      (setq prefix block-comment-enclose-prefix-top)
      )

    (setq indent-level (block-comment--get-indent-level prefix))
    (block-comment--move-line 1)

    (beginning-of-line)
    (insert (make-string indent-level
                         (string-to-char " ")
                         )
            )

    ;; Return indent level
    indent-level
    )
  )

(defun block-comment--get-indent-level (&optional prefix)
  """  Get the indentation level (int) of the current row                       """
  """  Param 'prefix' : The prefix to look for                                  """
  """                   Default: block-comment-prefix                           """
  """           return: Current indentation level                               """
  (unless prefix (setq prefix block-comment-prefix))

  (save-excursion
    (block-comment--jump-to-comment-start prefix)
    (current-column))
  )


(defun block-comment--is-centering-row ()
  """  Checks if the current block comment row is centering or non-centering.  """
  """  If the left margin is larger than (edge-offset + 1) and the diff        """
  """  between the margins is less than centering-diff-thresh,                 """
  """  then is centering                                                       """
  (save-excursion
  (let (
        (begin-width (block-comment--jump-to-first-char-in-body))
        (end-width (block-comment--jump-to-last-char-in-body 0))
        )
    ;; If diff between begin/end width is smaller than x, then assume
    ;; that we are in centering mode
    (and (> begin-width (+ block-comment-edge-offset 1))
         (> block-comment--centering-diff-thresh
            (abs (- begin-width end-width)))
         )
    )
    )
  )

(defun block-comment--move-line (count)
  """  Moves point 'count' lines up/down, keeping the column position.         """
  """  Param 'count':                                                          """
  """                 +x -> move point x lines down                            """
  """                 -x -> move point x lines up                              """
  (let (
        (column (current-column))
        )
    (forward-line count)
    (move-to-column column)
    )
  )

(defun block-comment--get-widest-comment-text ()
  """  Finds the width of the widest block comment text above point and        """
  """  returns said width. The block comment text is the actual user text      """
  """  inside the block comment body.                                          """
  (let (
        (widest-width 0)
        (curr-width 0)
        )

    (save-excursion
      ;; Jump to the postamble row, the row right beneath the last comment body
      (block-comment--jump-below-comment -1)

      (while (progn
               ;; Move up one line
               (forward-line -1)

               ;; Check if this is body or enclose
               (block-comment--is-body nil)
               )
        (setq curr-width (block-comment--get-comment-text-width))
        (message "width: %d" curr-width)
        (when (> curr-width widest-width)
          (setq widest-width curr-width)
          ) ;; End when

        ) ;; End while
      ) ;; End save-excursion

    ;; Return widest width
    widest-width
    ) ;; End let
  )

(defun block-comment--get-width (&optional body prefix postfix)
  (interactive)
  """  Returns the width of the block comment at point                         """
  """  Param 'body' specifies if we should take theh width of the body or the  """
  """               commment:                                                  """
  """                        t   -> Take width of body                         """
  """                        nil -> Take width of comment                      """
  """  Param 'prefix' : The prefix to look for                                 """
  """                   Default: block-comment-prefix                          """
  """  Param 'postfix' : The postfix to look for                               """
  """                    Default: block-comment-postfix                        """

  (unless prefix (setq prefix block-comment-prefix))
  (unless postfix (setq postfix block-comment-postfix))

  (let (
        (comment-start 0)
        (comment-end 0)
        )

    (if body
        (save-excursion
          (setq comment-start (block-comment--jump-to-body-start block-comment-edge-offset
                                                                 prefix))
          (setq comment-end (block-comment--jump-to-body-end block-comment-edge-offset
                                                             postfix))
          )
      (save-excursion
        (setq comment-start (block-comment--jump-to-comment-start prefix))
        (setq comment-end (block-comment--jump-to-comment-end 0
                                                              postfix))
        )
      )

    (- comment-end comment-start)
    )
  )

(defun block-comment--get-comment-text-width ()
  """  Gets the width of the actual text within the block comment              """
  (let (
        (text-start 0)
        (text-end 0)
        )

    (save-excursion
      ;; Jump to first text column position
      (block-comment--jump-to-first-char-in-body)
      (setq text-start (current-column))

      ;; Jump to last text column position, no offset
      (block-comment--jump-to-last-char-in-body 0)
      (setq text-end (current-column))
      ) ;; End save-excursion

    (message "start: %d end: %d" text-start text-end)

    ;; Return text width
    (- text-end text-start)
    ) ;; End let
  )

(defun block-comment--has-comment ()
  """ Checks if the block-comment-body at point contains a user comment """
  """ If it does, then return t, else nil                               """
  (let (
        (body-end 0)
        )
    (save-excursion
      (setq body-end (block-comment--jump-to-body-end))
      (block-comment--jump-to-body-start)
      (skip-syntax-forward " " body-end)
      (not (equal (point-marker)
                  body-end
                  )
           )
      )
    )
  )

(defun block-comment--is-body (&optional inside-body)
  """ Checks if the current row follows the format of a block comment body    """
  """  Param 'inside': specifies if point is required to be inside of the     """
  """                body or not:                                             """
  """                t   -> Point must be inside the body                     """
  """                nil -> Point must be on the same row as body             """
  """                Default: nil                                             """

  (block-comment--is-comment block-comment-prefix
                             block-comment-fill
                             block-comment-postfix
                             inside-body)
  )

(defun block-comment--is-enclose-top (&optional inside-body)
  """ Checks if the current row follows the format of a block comment         """
  """  top enclose                                                            """
  """  Param 'inside': specifies if point is required to be inside of the     """
  """                body or not:                                             """
  """                t   -> Point must be inside the body                     """
  """                nil -> Point must be on the same row as body             """
  """                Default: nil                                             """

  (block-comment--is-comment block-comment-enclose-prefix-top
                             block-comment-enclose-fill-top
                             block-comment-enclose-postfix-top
                             inside-body)
  )

(defun block-comment--is-enclose-bot (&optional inside-body)
  """ Checks if the current row follows the format of a block comment         """
  """  bottom enclose                                                         """
  """  Param 'inside': specifies if point is required to be inside of the     """
  """                body or not:                                             """
  """                t   -> Point must be inside the body                     """
  """                nil -> Point must be on the same row as body             """
  """                Default: nil                                             """

  (block-comment--is-comment block-comment-enclose-prefix-bot
                             block-comment-enclose-fill-bot
                             block-comment-enclose-postfix-bot
                             inside-body)
  )

(defun block-comment--is-comment (prefix fill postfix &optional inside)
  """ checks if the current row follows the format of a block comment body     """
  """ with the given prefix, fill and postfix.                                 """
  """  Param 'prefix' : The prefix to look for                                 """
  """  Param 'fill' : The fill to use                                          """
  """  Param 'postfix' : The postfix to look for                               """
  """  Param 'inside' specifies if point is required to be inside of the       """
  """                body or not:                                              """
  """       t   -> Point must be inside the body                               """
  """       nil -> Point must be on the same row as body                       """
  """  Param 'enclose' specifies if we should look for enclose, or normal body """
  """       t   -> Look for enclose by using param 'enclose-fill'              """
  """       nil -> Look for block body by using param 'fill'                   """
  (let (
        (read-prefix-pos nil)   ;; Position of current row:s prefix
        (read-postfix-pos nil)  ;; Position of current row:s postfix
        (point-in-body t)       ;; If point is inside body.
        )

    ;; Check if prefix is present on this row
    (save-excursion
      (beginning-of-line)
      (setq read-prefix-pos
            (search-forward
             (concat prefix fill)
             (line-end-position)
             t
             )
            )
      )

    ;; Check if postfix is present on this row
    (save-excursion

      (end-of-line)
      (setq read-postfix-pos
            (search-backward
             (concat fill postfix)
             (line-beginning-position)
             t)
            )
      )

    ;; If inside-body is true, check if point is inside body
    (when (and
           read-prefix-pos
           read-postfix-pos
           inside)

      (setq point-in-body (and
                           (> (point-marker) read-prefix-pos)
                           (< (point-marker) read-postfix-pos)
                           )
            )
      )

    ;; Return value, t if in block comment row, else nil
    (and read-prefix-pos
         read-postfix-pos
         point-in-body
         )
    )

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Jump to functions                                """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--jump-to-comment-start (&optional prefix)
  (interactive)
  """  Jump to block comment start, before the prefix.                         """
  """  Param 'prefix' : The prefix to look for                                 """
  """                   Default: block-comment-prefix                          """
  """  Ret: The position of the comment start                                  """

  (unless prefix (setq prefix block-comment-prefix))

  (block-comment--jump-to-body-start 0 prefix)
  (backward-char (string-width prefix))
  (point-marker)
  )

(defun block-comment--jump-to-comment-end (&optional offset postfix)
  """  Jump to block comment end, the char directly after after the postfix.    """
  """  Param 'offset': Offset can be used to move the position from the         """
  """                  default position                                         """
  """                  Default: 1                                               """
  """  Param 'postfix' : The postfix to look for                                """
  """                    Default: block-comment-postfix                         """
  """  Return: point-marker                                                     """

  (unless offset (setq offset 1))
  (unless postfix (setq postfix block-comment-postfix))

  (block-comment--jump-to-body-end 0 postfix)
  (forward-char (- (string-width postfix) 1))
  (forward-char offset)
  (point-marker)
  )

(defun block-comment--jump-to-body-center ()
  """ Jumps to the center of the block comment body """

  (let (
        (start-point 0)
        (end-point 0)
        (line-width 0)
        (middle-point 0)
        )

    ;; Set line width for this row
    (save-excursion

      (block-comment--jump-to-comment-start)
      (setq start-point (current-column))

      (block-comment--jump-to-comment-end)
      (setq end-point (current-column))
      )
    (setq line-width (- end-point start-point))
    (setq middle-point (/ line-width 2))

    (block-comment--jump-to-comment-start)
    (forward-char middle-point)

    )
  )

(defun block-comment--jump-to-body-start (&optional edge-offset prefix)
  """  Jumps to the start of block comment body                               """
  """  Param 'edge-offset': The offset from the block comment prefix          """
  """                       Default: block-comment-edge-offset                """
  """  Param 'prefix' : The prefix to look for                                """
  """                   Default: block-comment-prefix                         """
  """  Ret : The position of the body start                                   """
  (unless edge-offset (setq edge-offset block-comment-edge-offset))
  (unless prefix (setq prefix block-comment-prefix))

  (let (
        (start-pos (point-marker))
        (line-end (line-end-position))
        )
    (beginning-of-line)
    ;; Jump back one since search forward starts searching on point + 1
    (backward-char 1)
    ;; Place point at end of prefix if a prefix is found
    (if (search-forward prefix
                        line-end
                        t)
        (forward-char edge-offset)
      (goto-char start-pos)
      )
    )
  (point-marker)
  )

(defun block-comment--jump-to-body-end (&optional edge-offset postfix)
  (interactive)
  """  Jumps to the end of block comment body, meaning the inside of the        """
  """  block comment, excluding the pre/postfix and the edge offset.            """
  """  Param 'edge-offset': Sets a custome edge offset, meaning the distance    """
  """                       to the postfix.                                     """
  """                       Default: block-comment-edge-offset                  """
  """  Param 'postfix' : The postfix to look for                                """
  """                    Default: block-comment-postfix                         """
  """  Ret: The position of point                                               """
  (unless edge-offset (setq edge-offset block-comment-edge-offset))
  (unless postfix (setq postfix block-comment-postfix))

  (let (
        (start-pos (point-marker))
        (line-start (line-beginning-position))
        )
    (end-of-line)
    ;; Jump forward one since search backward starts searching on point + 1
    (forward-char 1)
    ;; Place point at start of postfix if a postfix is found
    (if (search-backward postfix
                         line-start
                         t)
        (backward-char edge-offset)
      (goto-char start-pos)
      )
    )
  (point-marker)
  )

(defun block-comment--jump-to-first-char-in-body (&optional offset)
  """   Jumps to the first char in the comment body text                       """
  """   Beginning means the first non-fill character in the body               """
  """   Param: 'offset': The offset can be used to change where to jump:       """
  """                    +x -> Jump closer to postfix                          """
  """                    -x -> Jump closer to prefix                           """
  """   Ret: the number of fill characters remaining on the left side          """

  (unless offset
    (setq offset 0)
    )

  (let (
        (body-start-pos nil)   ;; Start of block-comment body
        (comment-start-pos nil);; Start of user comment
        )

    (beginning-of-line)
    ;; Find start position in block comment
    (block-comment--jump-to-body-start 0)

    ;; Set start of block-comment body
    (setq body-start-pos (current-column))

    (skip-syntax-forward " " (line-end-position))

    ;; Set start of user comment
    (setq comment-start-pos (current-column))

    (forward-char offset)

    ;; Return remaining space between user comment and start of
    ;; block-comment body
    (- comment-start-pos body-start-pos)
    )
  )

(defun block-comment--jump-to-last-char-in-body (&optional offset)
  """  jumps to end of comment in body at point End means the place right     """
  """  after the last non-fill character in the body                          """
  """  Param: 'offset': Jumps to last char in body + this offset. Default = 1 """
  """  Ret: the number of fill characters remaining on the right side         """
  (let (
        (body-end-pos nil)   ;; End of block-comment body
        (comment-end-pos nil);; End of user comment
        )
    ;; Set default value
    (unless offset
      (setq offset 1)
      )

    (end-of-line)
    ;; Find end position in block comment
    (block-comment--jump-to-body-end 0)

    ;; Set end of block-comment body
    (setq body-end-pos (current-column))

    ;; Jump back to character pos right after last char in body
    (skip-syntax-backward " " (line-beginning-position))
    ;; Jump back one more to stand on last char in body
    (backward-char 1)
    ;; Jump forward by offset
    (forward-char offset)

    ;; Set end of user comment
    (setq comment-end-pos (current-column))

    ;; Return remaining space between user comment and end of block-comment body
    (- body-end-pos comment-end-pos)
    )
  )

(defun block-comment--jump-below-comment (&optional offset)
  (interactive)
  """ Moves point down to line right below the block comment enclose.        """
  """  Param 'offset': The offset can be used to tweak the relative          """
  """                  position that point ends on:                          """
  """                      +x -> Move point x lines further down             """
  """                      -x -> Move point x lines further up               """
  (unless offset
    (setq offset 0)
    )

  (let (
        (is-body nil)
        (is-enclose nil)
        )
    ;; Move to line below bottom of block commente
    (while (progn
             ;; Move down one line
               (forward-line 1)

             ;; Check if this is body or enclose
               (setq is-body (block-comment--is-body nil))
               (setq is-enclose-top (block-comment--is-enclose-top nil))
               (setq is-enclose-bot (block-comment--is-enclose-bot nil))

               (message "is-body: %s is-enclose-top: %s is-enclose-bot: %s"
                        is-body
                        is-enclose-top
                        is-enclose-bot)

             ;; Exit if not in comment
             (or is-body is-enclose-top is-enclose-bot)
             )
      ) ;; End while
    ) ;; End let

  (block-comment--move-line offset)
  )
