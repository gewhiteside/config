;; ~/.emacs.d/init.el
;; George Whiteside


;;; Global

;; Set f5 to flip the orientation of two buffers.
(defun resplit-window ()
  (interactive)
  (other-window 1)
  (delete-other-windows)
  (split-window-sensibly (selected-window))
  (other-window 1)
  (switch-to-buffer nil)
  (other-window 1))
(global-set-key [f5] 'resplit-window)

;; Set f12 to format buffer.
(defun format-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key [f12] 'format-buffer)

;; Add hook to multiple modes.
(defun add-hook-to-multiple-modes (modes hook)
  (mapc
   (lambda (mode)
     (add-hook mode hook))
   modes))

;; Common hooks.
(require 'whitespace)
(add-hook-to-multiple-modes
 '(c-mode-common-hook
   python-mode-hook
   emacs-lisp-mode-hook
   sh-mode-hook
   java-mode)
 (lambda ()
   ;; Delete trailing whitespace.
   (if my-delete-trailing-whitespace
       (add-to-list 'write-contents-functions 'delete-trailing-whitespace))
   ;; Add marker for column 80 and tabs.
   (whitespace-mode)))

;; Diable menu, tool and scroll bars.
(menu-bar-mode -1)
;; Scroll and tool bar mode are void functions on some systems.
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Enable upcase-region.
(put 'upcase-region 'disabled nil)

;; Enable desktop-save-mode. If running as a daemon, wait until the first frame
;; has been created.
(if (daemonp)
    (add-hook
     'after-make-frame-functions
     (lambda (frame)
       (unless desktop-save-mode
         (desktop-read)
         (desktop-save-mode 1))))
  (desktop-save-mode))

;; Resize window to 165x90 characters.
(defun my-frame ()
  (interactive)
  (set-frame-width (selected-frame) 165)
  (set-frame-height (selected-frame) 90)) ;; TODO: Fix this line.

;; Set default buffer. This prevents emacs from running as a daemon on my
;; Windows machine, so only enable it for non-daemon instances.
;; (unless (daemonp) (setq initial-buffer-choice 'ibuffer))

;; Automatically balance windows after vertical split.
(defadvice split-window-right
  (after balance-windows-after-right-split activate)
  (balance-windows))

;; Add my elisp directory and its subdirectories to the load path.
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq-default
 ;; Indent with spaces.
 indent-tabs-mode nil
 ;; Set fill column to 79.
 fill-column 79)

(setq
 ;; Don't show startup screen.
 inhibit-startup-screen t
 ;; Turn on delete trailling whitespace.
 my-delete-trailing-whitespace t
 ;; Turn on column number.
 column-number-mode t
 ;; Allow desktop-save-mode to remember remote files.
 ;; desktop-files-not-to-save "^$"
 ;; Require final newline on save.
 require-final-newline t
 ;; Turn off bell ring.
 ring-bell-function 'ignore
 ;; Split window right first.
 split-height-threshold 160
 split-width-threshold  160
 ;; Fit window to buffer.
 fit-window-to-buffer-horizontally t
 ;; Ask before quitting.
 confirm-kill-emacs 'y-or-n-p
 ;; Case-insensitive buffers and filenames.
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; Sentences are terminated with a single space.
 sentence-end-double-space nil)

;; Scroll up and down a line.
(global-set-key "\M-p"  'scroll-up-line)
(global-set-key "\M-n"  'scroll-down-line)

;; Switch windows with an easier key.
(global-set-key "\M-i" 'other-window)
;; TODO: this next line works, but is prohibitively slow. Why?
;; (global-set-key "\M-\S-i" (lambda () (interactive) (other-window -1)))


;;; Whitespace

;; Set the face of trailing lines and tabs.
(set-face-foreground 'whitespace-line nil)
(set-face-background 'whitespace-line "red")
(set-face-background 'whitespace-tab "red")

(setq
 ;; Highlight long lines and tabs.
 whitespace-style '(face tabs lines-tail)
 ;; Inherit max column from fill column.
 whitespace-line-column nil)


;;; IBuffer

;; make *Ibuffer* live
;; (if (daemonp) (ibuffer))

(add-hook
 'ibuffer-mode-hook
 (lambda ()
   ;; Default groups.
   (ibuffer-switch-to-saved-filter-groups my-group)
   ;; Auto refresh buffer list.
   (ibuffer-auto-mode 1)))

(setq
 ;; Turn off prompt to close unmodified buffers.
 ibuffer-expert t
 ;; Default sorting mode is alphabetic.
 ibuffer-default-sorting-mode 'alphabetic
 ;; Hide empty filter groups.
 ibuffer-show-empty-filter-groups nil
 ;; Set default groups.
 ibuffer-saved-filter-groups
 '(("default"
    ("emacs"   (filename . ".emacs.d"))
    ("org"     (mode . org-mode))
    ("special" (or (name . "\*")
                   (name . "TAGS")))))
 ;; my-group can be overwritten by a project-specific default.
 my-group "default"
 ;; Set default sorting to alphabetic.
 ibuffer-default-sorting-mode 'alphabetic
 ;; Set custom formats.
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

;; Use ibuffer as buffer list.
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;; C

(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; Set key to find header file in the same directory.
   (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; My personal C style.
(c-add-style
 "whiteside"
 '("linux"
   ;; Set indentation to 4.
   (c-basic-offset . 4)
   ;; Turn off indentation from namespaces.
   (c-offsets-alist . ((innamespace . 0)))))

(setq
 ;; Set C default style.
 c-default-style "whiteside")


;;; Bash

;; Use sh-mode to edit bashrc, *_bash, and bash_* files.
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("_bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_[_\w]*" . sh-mode))

(add-hook
 'sh-mode-hook
 (lambda ()
   ;; Disable << as insert here document.
   (sh-electric-here-document-mode -1)))

;; (setq
;;  ;; Set indentation to 2.
;;  sh-basic-offset 2
;;  sh-indentation  2)


;;; Org

(add-hook
 'org-mode-hook
 (lambda ()
   ;; ;; Swap level 1 and level 3 text colors.
   ;; (set-face-foreground 'org-level-1 "light blue")
   ;; (set-face-foreground 'org-level-3 "dark blue")

   ;; Rebind header navigation keys.
   ;; TODO: Check if these functions are called outline-* or org-*.
   (local-set-key (kbd "C-,") 'outline-next-visible-heading)
   (local-set-key (kbd "C-.") 'outline-previous-visible-heading)))

(setq
 ;; Open link in this window.
 org-link-frame-setup (lambda () (file . find-file))
 ;; Add markdown to export backends.
 org-export-backends '(ascii html icalendar latex md odt)
 ;; Disable line truncation.
 org-startup-truncated nil)


;;; Other files

;; Load machine-specific configuration.
;; (load "~/path-to-file/file.el")
