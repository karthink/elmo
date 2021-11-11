;;; Embark-based incremental completion and selection
;;  Customizations to use embark's live-occur as a completion system for Emacs.
;;  Most of this code is inspired by the work of Protesilaos
;;  Stavrou: https://protesilaos.com/dotemacs/

(require 'embark)

(defcustom elmo-always-show-list
  '(embark-prefix-help-command
    embark-completing-read-prompter
    embark-act)
  "List of commands for which the Embark live completions should
  always pop up immediately."
  :group 'elmo
  :type '(repeat (symbol :tag "Command: ")))

(defvar elmo-never-show-list nil
  "List of commands for which the Embark live completions should
  never pop up.

It can still be manually shown.")

(defcustom elmo-update-delay embark-collect-live-update-delay
  "Delay in seconds before updating the embark live collect buffer."
  :group 'elmo
  :type 'number)

(defcustom elmo-initial-delay embark-collect-live-initial-delay
  "Delay in seconds before displaying the embark live collect buffer."
  :group 'elmo
  :type 'number)
  
(defcustom elmo-min-input 3
  "Number of characters to type in before displaying the embark
  live collect buffer."
  :group 'elmo
  :type 'integer)

(defcustom elmo-max-height
  (lambda () (* 3 (floor (frame-height) 8)))
  "Specify the max height of the embark-collect window.

This can be an integer or a function of no arguments that returns
a number. Also see `elmo-resize'."
  :group 'elmo
  :type '(choice integer function))

(defcustom elmo-resize t
  "Should the embark-collect-live buffer be resized
  dynamically?"
  :group 'elmo
  :type 'boolean)

(defun elmo--minimum-input-p ()
  "Test if there are enough characters in the minibuffer.

This is to pop up the Embark live-collect buffer."
  (>= (length
       (buffer-substring-no-properties
        (minibuffer-prompt-end)
        (point-max)))
      elmo-min-input))

(defun elmo--wait-for-input (_beg _end _len)
  (when (and (minibufferp)
             (elmo--minimum-input-p))
    (remove-hook 'after-change-functions 'elmo--wait-for-input t)
    (let ((embark-collect-live-initial-delay elmo-initial-delay))
      (embark-collect-completions-after-delay))))

(defun elmo-collect-completions-after-input ()
  "Start `embark-collect-completions' after some minibuffer input.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Collect buffer popup soon after you type something in the
minibuffer."
  (when minibuffer-completion-table
    (if (member this-command elmo-always-show-list)
        (embark-collect-completions)
      (add-hook 'after-change-functions #'elmo--wait-for-input nil t))))

(defun elmo--minibuffer-local-completion-map ()
  "Hook to `minibuffer-setup-hook'."
  (use-local-map
   (make-composed-keymap elmo-minibuffer-local-completion-map (current-local-map))))

(defun elmo--embark-collect-mode-map ()
  "Hook to `embark-collect-mode-hook'."
  (use-local-map
   (make-composed-keymap elmo-live-collect-map (current-local-map))))

(defun elmo--directory-completing-file-p ()
  "Return non-nil when completing file names."
  (eq 'file
      (completion-metadata-get
       (completion-metadata
        (buffer-substring (minibuffer-prompt-end)
                          (max (minibuffer-prompt-end) (point)))
        minibuffer-completion-table
        minibuffer-completion-predicate)
       'category)))

;; Ido-like directory handling
(defun elmo-directory-up ()
  "Delete directory before point."
  (interactive)
  (when (and (> (point) (minibuffer-prompt-end))
             (eq (char-before) ?/)
             (elmo--directory-completing-file-p))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (delete-region (1+ (point)) (point-max))
        t))))

(defun elmo-directory-delete-char ()
  "Delete directory or char before point."
  (interactive)
  (unless (elmo-directory-up)
    (call-interactively #'backward-delete-char)))

(defun elmo-directory-delete-word ()
  "Delete directory or word before point."
  (interactive)
  (unless (elmo-directory-up)
    (let ((pt (point)))
      (forward-word -1)
      (delete-region pt (point)))))

(defun elmo-directory-enter ()
  "Enter directory or exit completion with current candidate."
  (interactive)
  (let ((cand (cdr (embark-target-top-minibuffer-completion))))
    (if (and (elmo--directory-completing-file-p)
             (string-suffix-p "/" cand))
        (progn (delete-minibuffer-contents)
               (insert (substring-no-properties cand)))
      (minibuffer-force-complete-and-exit))))

(defun elmo--height (win)
  (let ((height (if (functionp elmo-max-height)
                    (funcall elmo-max-height)
                  elmo-max-height)))
    (if elmo-resize
        (fit-window-to-buffer
         win
         height)
      height)))

(defun elmo-keyboard-quit ()
  "If in an Embark live collect/completions buffer, run
`abort-recursive-edit'. Otherwise run `keyboard-quit'."
  (interactive)
  (if (elmo--live-completions-p)
      (if (use-region-p)
          (keyboard-quit)
        (kill-buffer)
        (abort-recursive-edit))
    (keyboard-quit)))

(defun elmo--clear-live-buffers ()
  "Remove lingering Embark Collect Completions' buffers.
Add this to `minibuffer-exit-hook'."
  (let* ((buffers (buffer-list))
         (case-fold-search nil)
         (completions
          (cl-remove-if-not (lambda (buf)
                              (string-match "\\*Embark.*Completions.*"
                                            (format "%s" buf)))
                            buffers)))
    (mapc #'kill-buffer completions)))

(defun elmo--collect-fit-window (&rest _)
  "Fit Embark's live occur window to its buffer.
To be added to `embark-collect-post-revert-hook'."
  (when (and (derived-mode-p 'embark-collect-mode)
             (eq embark-collect--kind :completions))
    (if elmo-resize
        (elmo--height (get-buffer-window)))))

(defun elmo--live-buffer-p ()
  "Determine presence of a linked live occur buffer."
  (let ((buf embark-collect-linked-buffer))
    (when buf
      (window-live-p (get-buffer-window buf)))))

;; (defun elmo--live-buffer-p ()
;; "Determine presence of a linked live occur buffer."
;; (let* ((buf-link embark-collect-linked-buffer)
;;        (buf-name (buffer-name buf-link)))
;;   (when buf-name
;;     (string-match-p elmo-collect-window-regexp buf-name))))

;;   (defvar elmo-collect-window-regexp
;;   "\\*Embark Collect \\(Live\\|Completions\\).*"
;;   "Regexp to match window names with Embark collections.")

(defun elmo--live-completions-p ()
  "Determine whether current collection is for live completions."
  (and (derived-mode-p 'embark-collect-mode)
       (eq embark-collect--kind :completions)))

(defun elmo-completions-toggle ()
  "Toggle `embark-collect-completions'."
  (interactive)
  (if (elmo--live-buffer-p)
      (kill-buffer embark-collect-linked-buffer)
    (embark-collect-completions)))

(defun elmo-collect-toggle-view ()
  (when (eq embark-collect-view 'list)
    (hl-line-mode -1)
    (embark-collect--toggle 'embark-collect-view 'list 'grid)))

(setq embark-candidate-collectors
      (delete 'embark-minibuffer-candidates embark-candidate-collectors))
(add-to-list 'embark-candidate-collectors 'embark-sorted-minibuffer-candidates)

(defun embark-top-sorted-minibuffer-candidates ()
  "Return a sorted list of the top 30 current minibuffer completion candidates.
This using the same sort order that `icomplete' and
`minibuffer-force-complete' use. The intended usage is that you
replace `embark-minibuffer-candidates' with this function in the
list `embark-candidate-collectors'."
  (when (minibufferp)
    (cons
     (completion-metadata-get (embark--metadata) 'category)
     (let ((cacs (completion-all-sorted-completions)))
       (nconc (cl-copy-list (if (listp cacs) (seq-take cacs 30))) nil)))))

;; (defun elmo-minibuffer-candidates ()
;;   (seq-take (embark-minibuffer-candidates) 40))

(defun elmo-minibuffer-focus-mini ()
  "Focus the active minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))  

;; Move from minibuffer to embark-collect and back
(defun elmo-next-line-or-mini (&optional arg)
  "Move to the next line or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction, then it switches to
the minibuffer."
  (interactive "p")
  (if (or (eobp) (eq (point-max)
                     (1+ (line-end-position))
                     ;; (save-excursion (forward-line 1) (point))
                     ))
      (elmo-minibuffer-focus-mini)    ; from `setup-minibuffer.el'
    (forward-line (or arg 1)))
  (setq this-command 'next-line))

(defun elmo-previous-line-or-mini (&optional arg)
  "Move to the previous line or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction, then it switches to
the minibuffer."
  (interactive "p")
  (let ((num (if arg (- arg)))) ; from `elmo-common.el'
    (if (bobp)
        (elmo-minibuffer-focus-mini)    ; from `elmo-minibuffer.el'
      (forward-line (or num -1)))))

(defun elmo--switch-to-completions ()
  "Subroutine for switching to the Embark completions buffer."
  (unless (elmo--live-buffer-p)
    (elmo-completions-toggle))
  (pop-to-buffer embark-collect-linked-buffer))

(defun elmo-switch-to-completions-top ()
  "Switch to the top of Embark's completions buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (elmo--switch-to-completions)
  (goto-char (point-min)))

(defun elmo-switch-to-completions-bottom ()
  "Switch to the bottom of Embark's completions buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (elmo--switch-to-completions)
  (goto-char (point-max))
  (forward-line -1)
  (goto-char (point-at-bol)))

;; Highlighting selections in embark-collect buffers
(defvar-local elmo--collect-overlay nil
  "Text overlay for embark-collect buffers.")

(defun elmo--embark-collect-live-setup ()
  "Remove mode-line from live embark-collect buffers and set up
highlighting."
  (when (elmo--live-completions-p)
    (setq-local mode-line-format nil))
  (setq elmo--collect-overlay (make-overlay 1 1))
  (overlay-put elmo--collect-overlay 'face 'highlight)
  (add-hook 'post-command-hook 'elmo--collect-live-overlay-update nil t))

(defun elmo--collect-live-overlay-update ()
  "Update the overlay in the embark-collect buffer."
  (pcase embark-collect-view
    ('list (hl-line-mode 1))
    ('grid (when (and (overlayp elmo--collect-overlay)
                      (get-text-property (point) 'mouse-face))
             (hl-line-mode 0)
             (let ((beg (previous-single-property-change
                         (if (eobp) (point-max) (1+ (point)))
                         'mouse-face nil (point-min)))
                   (end (next-single-property-change (point) 'mouse-face nil (point-max))))
               (move-overlay elmo--collect-overlay beg end))))))

(defun elmo-insert ()
  (interactive)
  "Insert candidate into minibuffer."
  (let ((completion-cycle-threshold t))
    (minibuffer-complete)))

(defun elmo--collect-delay-update (orig-fn &rest _)
  (let ((embark-collect-live-update-delay elmo-update-delay))
    (funcall orig-fn _)))

;; (lambda () (interactive)
;;   (if minibuffer--require-match
;;       (minibuffer-complete-and-exit)
;;     (exit-minibuffer)))

(defvar elmo-minibuffer-local-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n")     #'elmo-switch-to-completions-top)
    (define-key map (kbd "C-p")     #'elmo-switch-to-completions-bottom)
    (define-key map (kbd "C-l")     #'elmo-completions-toggle)
    (define-key map (kbd "RET")     #'elmo-directory-enter)
    (define-key map (kbd "C-j")     #'exit-minibuffer)
    (define-key map (kbd "DEL")     #'elmo-directory-delete-char)
    (define-key map (kbd "M-DEL")   #'elmo-directory-delete-word)
    (define-key map (kbd "C-w")     #'elmo-directory-delete-word)
    (define-key map (kbd "C-M-l")   #'embark-export)
    (define-key map (kbd ">")       #'embark-become)
    (define-key map (kbd "M-q")     #'embark-collect-toggle-view)
    (define-key map (kbd "C-c C-f") #'consult-preview-at-point-mode)
    (define-key map (kbd "M-i")     #'elmo-insert)
    (define-key map (kbd "<tab>")   #'elmo-insert)
    (define-key map (kbd "C-M-v")   #'minibuffer-scroll-other-window)
    (define-key map (kbd "C-M-S-v") #'minibuffer-scroll-other-window-down)
    (define-key map (kbd "C-v")     #'scroll-other-window)
    (define-key map (kbd "M-v")     #'scroll-other-window-down)
    map))

(defvar elmo-live-collect-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n")     #'elmo-next-line-or-mini)
    (define-key map (kbd "C-p")     #'elmo-previous-line-or-mini)
    (define-key map (kbd "C-g")     #'elmo-keyboard-quit)
    (define-key map (kbd "C-M-l")   #'embark-export)
    (define-key map (kbd ">")       #'embark-become)
    (define-key map (kbd "M-q")     #'embark-collect-toggle-view)
    (define-key map (kbd "C-c C-f") #'consult-preview-at-point-mode)
    (define-key map (kbd "M-RET")   #'consult-preview-at-point)
    map))

;;;###autoload
(define-minor-mode elmo-mode
  "Embark-live based incremental completion system."
  :global t
  :group 'elmo
  :lighter ""
  (if elmo-mode
      (progn
        (add-hook 'minibuffer-setup-hook 'elmo-collect-completions-after-input)
        (add-hook 'minibuffer-setup-hook 'elmo--minibuffer-local-completion-map)
        (add-hook 'minibuffer-exit-hook 'elmo--clear-live-buffers)
        ;; (add-hook 'embark-post-action-hook 'embark-collect--update-linked)
        (add-hook 'embark-collect-post-revert-hook 'elmo--collect-fit-window)
        (add-hook 'embark-collect-mode-hook 'elmo--embark-collect-mode-map)
        (add-hook 'embark-collect-mode-hook 'elmo--embark-collect-live-setup)
        (advice-add 'embark-collect--update-linked :around
                    'elmo--collect-delay-update)
        (and (fboundp 'consult-preview-at-point-mode)
             (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))
        (setq resize-mini-windows t)
        (setf  (alist-get "^\\*Embark Collect Completions\\*"
                          display-buffer-alist nil nil 'equal)
               `((display-buffer-reuse-mode-window display-buffer-at-bottom)
                 (window-height . elmo--height)
                 (side . bottom)
                 (slot . 0)
                 (window-parameters . ((no-other-window . t))))))
    (remove-hook 'minibuffer-setup-hook 'elmo-collect-completions-after-input)
    (remove-hook 'minibuffer-setup-hook 'elmo--minibuffer-local-completion-map)
    (remove-hook 'minibuffer-exit-hook 'elmo--clear-live-buffers)
    ;; (remove-hook 'embark-post-action-hook 'embark-collect--update-linked) ;
    (remove-hook 'embark-collect-post-revert-hook 'elmo--collect-fit-window)
    (remove-hook 'embark-collect-mode-hook 'elmo--embark-collect-live-setup)
    (remove-hook 'embark-collect-mode-hook 'elmo--embark-collect-mode-map)
    (remove-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)
    (advice-remove 'embark-collect--update-linked 'elmo--collect-delay-update)
    (setf  (alist-get "^\\*Embark Collect Completions\\*"
                      display-buffer-alist nil nil 'equal)
           nil)))

(provide 'elmo)
