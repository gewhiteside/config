;; Add my elisp directory and its subdirectories to the load path (
;; https://www.emacswiki.org/emacs/LoadPath).
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; If /usr/local/bin exists, add it to the executable path.
(if (file-directory-p "/usr/local/bin")
    (add-to-list 'exec-path "/usr/local/bin" t))

(require 'whitespace)

;; LLVM
(load "llvm/emacs.el")

;; Reservoir
(if (file-exists-p "~/reservoir/reservoir.el")
    (load "~/reservoir/reservoir.el"))



;; General settings
(setq
 ;; Don't show startup screen.
 inhibit-startup-screen t
 ;; Turn on column number.
 column-number-mode t
 ;; Require final newline on save.
 require-final-newline t
 ;; Turn off bell ring.
 ring-bell-function 'ignore
 ;; Split the current window vertically if it is more than 160 columns.
 split-height-threshold nil
 split-width-threshold 160
 ;; Fit window to buffer horizontally as well as vertically.
 fit-window-to-buffer-horizontally t
 ;; Ask before quitting.
 confirm-kill-emacs 'y-or-n-p
 ;; Case-insensitive buffer and file names.
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; Sentences are terminated with a single space.
 sentence-end-double-space nil)

(setq-default
 ;; Indent with spaces.
 indent-tabs-mode nil
 ;; Set fill column to 80.
 fill-column 80)

;; Disable menu, tool and scroll bars.
(menu-bar-mode -1)
;; Scroll and tool bar mode are void functions on some systems.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Enable upcase-region.
(put 'upcase-region 'disabled nil)

;; Scroll up and down a line.
(global-set-key "\M-p"  'scroll-up-line)
(global-set-key "\M-n"  'scroll-down-line)

;; Switch windows with an easier key.
(global-set-key "\M-i" 'other-window)
(global-set-key "\M-\S-i" (lambda () (interactive) (other-window -1)))



;; SMerge
;;
;; Change the command prefix to something easier.
(setq smerge-command-prefix "\C-cv")



;; Desktop save
(desktop-save-mode)

;; Save all files (including remote files).
(setq desktop-files-not-to-save "^$")



;; Whitespace
(setq
 ;; Hightlight tabs.
 whitespace-style '(face tabs)
 ;; Use whitespace-mode in all major modes that support it.
 global-whitespace-mode t)

;; Change the tab highlight color.
(set-face-background 'whitespace-tab "orange red")



;; IBuffer
(add-hook
 'ibuffer-mode-hook
 (lambda ()
   ;; Switch to my default filter groups if I have set them.
   (if (boundp my-default-groups)
       (ibuffer-switch-to-saved-filter-groups my-default-groups))
   ;; Auto refresh buffer list.
   (ibuffer-auto-mode 1)))

(setq
 ;; Turn off prompt to close unmodified buffers.
 ibuffer-expert t
 ;; Hide empty filter groups.
 ibuffer-show-empty-filter-groups nil
 ;; Set default sorting to alphabetic.
 ibuffer-default-sorting-mode 'alphabetic
 ;; Set custom formats.
 ibuffer-formats
 '((mark modified read-only " "
         (name 40 40 :left :elide) " " filename-and-process)))

;; Use ibuffer as buffer list.
(global-set-key (kbd "C-x C-b") 'ibuffer)



;; C
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; Set key to find header file in the same directory.
   (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; Set C default style.
(setq c-default-style "llvm.org")



;; Bash
(add-hook
 'sh-mode-hook
 (lambda ()
   ;; Disable << as insert here document.
   (sh-electric-here-document-mode -1)))

;; Use sh-mode to edit bashrc, *_bash, and bash_* files.
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("_bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_[_\w]*" . sh-mode))



;; Org
(add-hook
 'org-mode-hook
 (lambda ()
   ;; Rebind header navigation keys.
   (local-set-key (kbd "C-,") 'outline-next-visible-heading)
   (local-set-key (kbd "C-.") 'outline-previous-visible-heading)))

(setq
 ;; Open links in this window.
 org-link-frame-setup (lambda () (file . find-file))
 ;; Add markdown to export backends.
 org-export-backends '(ascii html icalendar latex md odt)
 ;; Disable line truncation.
 org-startup-truncated nil)
