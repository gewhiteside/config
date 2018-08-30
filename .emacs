;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global

;; turn off bell ring
(setq ring-bell-function 'ignore)

;; set f5 to flip the orientation of two buffers
(defun resplit-window ()
  (interactive)
  (other-window 1)
  (delete-other-windows)
  (split-window-sensibly (selected-window))
  (other-window 1)
  (switch-to-buffer nil)
  (other-window 1))
(global-set-key [f5] 'resplit-window)

;; set f12 to format buffer
(defun format-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max) nil))
(global-set-key [f12] 'format-buffer)

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

;; add marker for column 80 and *shudders* tabs
(require 'whitespace)
(setq whitespace-style '(face tabs lines-tail))
(global-whitespace-mode t)

;; set the face of trailing lines
(set-face-background 'whitespace-line "red")
(set-face-background 'whitespace-tab "red")
(set-face-foreground 'whitespace-line nil)

;; require final newline on save
(setq require-final-newline t)

;; split window right first
(setq split-height-threshold 160
      split-width-threshold 160)

;; ask before quitting
(setq confirm-kill-emacs 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBuffer

;; use ibuffer as buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; turn off prompt to close unmodified buffers
(setq ibuffer-expert t)

;; hide empty filter groups
(setq ibufer-show-empty-filter-groups nil)

;; auto refresh buffer list
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; set default groups
(setq my-default-filter-groups
      '("Special" (or (name . "\*")
                      (name . "TAGS"))))

;; add default groups
;; my-groups variable can be overwritten to add other groups
(setq ibuffer-saved-filter-groups
      `(("default"
         ,my-default-filter-groups))
      my-groups "default")

(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups my-groups)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

;; set C indentation style
(setq c-default-style "linux"
      c-basic-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bash

;; disale << as insert document skeleton
(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))

;; set bash indentation
(setq sh-basic-offset 2
      sh-indentation 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other

;; load other settings, like project-specific modes or ibuffer groups
;; (load "~/path-to-project/project.el")
