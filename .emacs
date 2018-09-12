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
(add-hook-to-multiple-modes
 '(c-mode-hook
   c++-mode-hook
   python-mode-hook
   emacs-lisp-mode-hook
   sh-mode-hook)
 'whitespace-mode)

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

;; scroll window
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   1)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 1)) )

;; auto-fill
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; enable upcase-region
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBuffer

;; use ibuffer as buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; turn off prompt to close unmodified buffers
(setq ibuffer-expert t)

;; hide empty filter groups
(setq ibuffer-show-empty-filter-groups nil)

;; auto refresh buffer list
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; set default groups
(setq ibuffer-saved-filter-groups
      '(("default"
         ("org"     (mode . org-mode))
         ("special" (or (name . "\*")
                        (name . "TAGS"))))))

;; my-groups can be overwritten by a project-specific default
(setq my-groups "default")

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
;; Org

;; swap level 1 and level 3 text colors
(add-hook
 'org-mode-hook
 (lambda () (set-face-foreground 'org-level-1 "light blue")))
(add-hook
 'org-mode-hook
 (lambda () (set-face-foreground 'org-level-3 "dark blue")))

;; open link in this window
(setq org-link-frame-setup (lambda () (file . find-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other

;; load other settings, like project-specific modes or ibuffer groups
;; (load "~/path-to-project/project.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends (quote (ascii html icalendar latex md odt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
