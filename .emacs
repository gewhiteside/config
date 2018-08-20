;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global

;; turn off bell ring
(setq ring-bell-function 'ignore)

;; set f9 to call macro
(global-set-key [f9] 'kmacro-end-and-call-macro)

;; set f12 to indent buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; set fill column for formatting comments/text
(setq-default fill-column 80)
(setq column-number-mode t)

;; diable menu bar
(menu-bar-mode -1)

;; add hook to multiple modes
(defun add-hook-to-multiple-modes (modes hook)
  (mapc
   (lambda (mode)
     (add-hook mode hook))
   modes))

;; delete trailing whitespace
(add-hook-to-multiple-modes
 '(c-mode-hook
   c++-mode-hook
   python-mode-hook
   emacs-lisp-mode-hook
   sh-mode-hook)
 (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; require final newline on save
(setq require-final-newline t)

;; split window right first
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; use ibuffer as buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ask before quitting
(setq confirm-kill-emacs 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

;; set c indentation style
(setq c-default-style "linux"
      c-basic-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bash

;; disale << as insert document skeleton
(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))
(setq sh-basic-offset 2
      sh-indentation 2)
