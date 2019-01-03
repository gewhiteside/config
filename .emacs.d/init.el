;; ~/.emacs
;; George Whiteside

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
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key [f12] 'format-buffer)

;; add hook to multiple modes
(defun add-hook-to-multiple-modes (modes hook)
  (mapc
   (lambda (mode)
     (add-hook mode hook))
   modes))

;; common hooks
(require 'whitespace)
(add-hook-to-multiple-modes
 '(c-mode-hook
   c++-mode-hook
   python-mode-hook
   emacs-lisp-mode-hook
   sh-mode-hook)
 (lambda ()
   ;; delete trailing whitespace
   (if my-delete-trailing-whitespace
       (add-to-list 'write-contents-functions 'delete-trailing-whitespace))
   ;; add marker for column 80 and tabs
   (whitespace-mode)))

;; diable menu, tool and scroll bars
(menu-bar-mode -1)
;; scroll and tool bar mode are void functions on some systems
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; enable upcase-region
(put 'upcase-region 'disabled nil)

;; enable desktop-save-mode
(desktop-save-mode)

;; automatically balance windows after vertical split
(defadvice split-window-right
    (after balance-windows-after-right-split activate)
  (balance-windows))

;; add my elisp dir to load path
(add-to-list 'load-path "~/.emacs.d/elisp")

(setq-default
 ;; indent with spaces
 indent-tabs-mode nil
 ;; fill column
 fill-column 79)

(setq
 ;; don't show startup screen
 inhibit-startup-screen t
 ;; turn on delete trailling whitespace
 my-delete-trailing-whitespace t
 ;; turn on column number
 column-number-mode t
 ;; require final newline on save
 require-final-newline t
 ;; turn off bell ring
 ring-bell-function 'ignore
 ;; split window right first
 split-height-threshold 160
 split-width-threshold  160
 ;; fit window to buffer
 fit-window-to-buffer-horizontally t
 ;; ask before quitting
 confirm-kill-emacs 'y-or-n-p
 ;; case-insensitive buffers and filenames
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t)

;; scroll window
(global-set-key "\M-p"  'scroll-up-line)
(global-set-key "\M-n"  'scroll-down-line)

;; switch window
(global-set-key "\M-i" 'other-window)
;; TODO: this next line works, but is prohibitively slow. Why?
;; (global-set-key "\M-\S-i" (lambda () (interactive) (other-window -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace

;; set the face of trailing lines and tabs
(set-face-foreground 'whitespace-line nil)
(set-face-background 'whitespace-line "red")
(set-face-background 'whitespace-tab "red")

(setq
 ;; highlight long lines and tabs
 whitespace-style '(face tabs lines-tail)
 ;; inherit max column from fill column
 whitespace-line-column nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBuffer

(add-hook
 'ibuffer-mode-hook
 (lambda ()
   ;; default groups
   (ibuffer-switch-to-saved-filter-groups my-group)
   ;; auto refresh buffer list
   (ibuffer-auto-mode 1)))

(setq
 ;; turn off prompt to close unmodified buffers
 ibuffer-expert t
 ;; default sorting mode is alphabetic
 ibuffer-default-sorting-mode 'alphabetic
 ;; hide empty filter groups
 ibuffer-show-empty-filter-groups nil
 ;; set default groups
 ibuffer-saved-filter-groups
 '(("default"
    ("org"     (mode . org-mode))
    ("special" (or (name . "\*")
                   (name . "TAGS")))))
 ;; my-group can be overwritten by a project-specific default
 my-group "default"
 ;; set default sorting to alphabetic
 ibuffer-default-sorting-mode 'alphabetic
 ;; set custom formats
 ibuffer-formats
 '((mark modified read-only " "
         (name 40 40 :left :elide)
         " " filename-and-process)
   (mark modified read-only " "
         (name 18 18 :left :elide)
         " "
         (size 9 -1 :right)
         " "
         (mode 16 16 :left :elide)
         " " filename-and-process)))

;; use ibuffer as buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; shortcut to find header file in the same directory
   (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; my personal C style
(c-add-style
 "whiteside"
 '("linux"
   ;; set indentation to 4
   (c-basic-offset . 4)
   ;; turn off indentation from namespaces
   (c-offsets-alist . ((innamespace . 0)))))

(setq
 ;; set C default style
 c-default-style "whiteside")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bash

(add-hook
 'sh-mode-hook
 (lambda ()
   ;; disable << as insert here document
   (sh-electric-here-document-mode -1)))

;; (setq
;;  ;; set indentation to 2
;;  sh-basic-offset 2
;;  sh-indentation  2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(add-hook
 'org-mode-hook
 (lambda ()
   ;; ;; swap level 1 and level 3 text colors
   ;; (set-face-foreground 'org-level-1 "light blue")
   ;; (set-face-foreground 'org-level-3 "dark blue")

   ;; rebind header navigation keys
   (local-set-key (kbd "C-,") 'org-next-visible-heading)
   (local-set-key (kbd "C-.") 'org-previous-visible-heading)))

(setq
 ;; open link in this window
 org-link-frame-setup (lambda () (file . find-file))
 ;; add markdown to export backends
 org-export-backends '(ascii html icalendar latex md odt)
 ;; disable line truncation
 org-startup-truncated nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other files

;; load project-specific modes and ibuffer groups
;; (load "~/path-to-file/file.el")
