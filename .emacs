;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global

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

;; add hook to multiple modes
(defun add-hook-to-multiple-modes (modes hook)
  (mapc
   (lambda (mode)
     (add-hook mode hook))
   modes))

(require 'whitespace)
;; common hooks
(add-hook-to-multiple-modes
 '(c-mode-hook
   c++-mode-hook
   python-mode-hook
   emacs-lisp-mode-hook
   sh-mode-hook)
 (lambda ()
   ;; delete trailing whitespace
   (add-to-list 'write-file-functions 'delete-trailing-whitespace)
   ;; add marker for column 80 and *shudders* tabs
   'whitespace-mode))

;; diable menu bar
(menu-bar-mode -1)

;; enable upcase-region
(put 'upcase-region 'disabled nil)

(setq-default
 ;; indent with spaces
 indent-tabs-mode nil
 ;; fill column
 fill-column 80)

(setq
 ;; turn on column number
 column-number-mode t
 ;; require final newline on save
 require-final-newline t
 ;; turn off bell ring
 ring-bell-function 'ignore
 ;; split window right first
 split-height-threshold 160
 split-width-threshold 160
 ;; ask before quitting
 confirm-kill-emacs 'y-or-n-p
 ;; case-insensitive completion
 read-buffer-completion-ignore-case t)

;; scroll window
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   1)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 1)) )
;; auto-fill
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; set the face of trailing lines
(set-face-background 'whitespace-line "red")
(set-face-background 'whitespace-tab "red")
(set-face-foreground 'whitespace-line nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBuffer

(add-hook 'ibuffer-mode-hook
          (lambda ()
            ;; default groups
            (ibuffer-switch-to-saved-filter-groups my-groups)
            ;; auto refresh buffer list
            (ibuffer-auto-mode 1)))

(setq
 ;; turn off prompt to close unmodified buffers
 ibuffer-expert t
 ;; hide empty filter groups
 ibuffer-show-empty-filter-groups nil
 ;; set default groups
 ibuffer-saved-filter-groups
 '(("default"
    ("org"     (mode . org-mode))
    ("special" (or (name . "\*")
                   (name . "TAGS")))))
 ;; my-groups can be overwritten by a project-specific default
 my-groups "default")

;; use ibuffer as buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

(setq
 ;; set C indentation style
 c-default-style "linux"
 c-basic-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bash

;; disale << as insert document skeleton
(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))

;; set bash indentation
(setq sh-basic-offset 2
      sh-indentation 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(add-hook
 'org-mode-hook
 (lambda ()
   ;; swap level 1 and level 3 text colors
   (set-face-foreground 'org-level-1 "light blue")
   (set-face-foreground 'org-level-3 "dark blue")
   ;; rebind header navigation keys
   (define-key org-mode-map (kbd "C-m") 'org-next-visible-heading)
   (define-key org-mode-map (kbd "C-,") 'org-previous-visible-heading)
   (define-key org-mode-map (kbd "C-.") 'org-cycle)))

;; open link in this window
(setq org-link-frame-setup (lambda () (file . find-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other

;; load other settings, like project-specific modes or ibuffer groups
;; (load "~/path-to-file/file.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-cycle-separator-lines 1)
 '(org-export-backends (quote (ascii html icalendar latex md odt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
