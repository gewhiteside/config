;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-

(add-to-list 'load-path "~/.emacs.d/elisp/")

(load "llvm/emacs.el")
(load "llvm/llvm-mode.el")
(load "llvm/tablegen-mode.el")
(load "~/reservoir/reservoir.el" 'noerror)

;; Store customization info in a separate file so this file isn't polluted.
(setq custom-file "~/.emacs.d/elisp/custom.el")
(load custom-file 'noerror)



;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-selected-packages
      '(ggtags projectile flycheck clang-format cmake-mode markdown-mode))
(package-install-selected-packages)



(require 'cc-vars)
(require 'clang-format)
(require 'desktop)
(require 'ibuf-ext)
(require 'org)
(require 'sh-script)
(require 'smerge-mode)
(require 'whitespace)



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
 sentence-end-double-space nil
 ;; Open the *scratch* buffer in fundamental-mode.
 initial-major-mode 'fundamental-mode
 ;; Disable the transparent toolbar on macOS.
 default-frame-alist '((ns-transparent-titlebar . nil))
 ;; Default to the home directory.
 default-directory "~/")

(setq-default
 ;; Indent with spaces.
 indent-tabs-mode nil
 ;; Set fill column to 80.
 fill-column 80)

;; Add /usr/local/bin to the executable path.
(add-to-list 'exec-path "/usr/local/bin")

;; Disable menu, tool and scroll bars.
(menu-bar-mode -1)
;; Scroll and tool bar mode are void functions on some systems.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Enable upcase-region.
(put 'upcase-region 'disabled nil)

;; Scroll up and down a line.
(global-set-key (kbd "M-p") 'scroll-up-line)
(global-set-key (kbd "M-n") 'scroll-down-line)

;; Switch windows with an easier key.
(global-set-key (kbd "M-i") 'other-window)
(global-set-key (kbd "M-I") (lambda () (interactive) (other-window -1)))



;; Fill column indicator
(global-display-fill-column-indicator-mode)

;; Disable the fill column indicator in certain modes.
(dolist (hook '(help-mode-hook completion-list-mode-hook))
  (add-hook hook (lambda () (display-fill-column-indicator-mode 0))))



;; Git
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . text-mode))

;; Shorten the fill column of Git commit messages and disable whitespace-mode
;; because the default template includes tabs.
(add-hook 'text-mode-hook
          (lambda () (if (string-equal "COMMIT_EDITMSG" (buffer-name))
                         (setq fill-column 70)
                       (whitespace-mode))))



;; SMerge
;;
;; Change the command prefix to something easier.
(setq smerge-command-prefix (kbd "C-c s"))



;; Desktop save
(desktop-save-mode)

;; Save all files (including remote files).
(setq desktop-files-not-to-save "^$")



;; Whitespace
(setq
 ;; Highlight tabs.
 whitespace-style '(face tabs)
 ;; Enable whitespace visualization for given modes.
 whitespace-global-modes
 '(c-mode c++-mode sh-mode emacs-lisp-mode cmake-mode python-mode))

;; Change the tab highlight color.
(set-face-background 'whitespace-tab "orange red")

;; Turn off whitespace visualization from global-whitespace-mode by disabling
;; local whitespace-mode.
(global-set-key (kbd "C-c w") (lambda () (interactive) (whitespace-mode 0)))



;; IBuffer (https://www.emacswiki.org/emacs/IbufferMode)
(add-hook
 'ibuffer-mode-hook
 (lambda ()
   ;; Auto refresh buffer list.
   (ibuffer-auto-mode 1)
   ;; Don't show a fill column indicator in IBuffer.
   (display-fill-column-indicator-mode 0)))

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

;; Don't show special buffers.
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; Use IBuffer as buffer list.
(global-set-key (kbd "C-x C-b") 'ibuffer)



;; C/C++
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; Set key to find header file in the same directory.
   (local-set-key (kbd "C-c o") 'ff-find-other-file)
   (local-set-key (kbd "C-M-<tab>") 'clang-format-region)
   ;; Set key to format the whole buffer.
   (local-set-key (kbd "C-c f")
                  (lambda () (interactive)
                    (clang-format-region (point-min) (point-max))))))

;; Set C default style.
(setq c-default-style "llvm.org")

;; Use c++-mode for .h files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))



;; Bash
(add-hook
 'sh-mode-hook
 (lambda ()
   ;; Disable << as insert here document.
   (sh-electric-here-document-mode -1)))

;; Use sh-mode to edit bashrc, *_bash, and bash_* files.
(add-to-list 'auto-mode-alist '("\\.?bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("_bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?bash_" . sh-mode))
(add-to-list 'auto-mode-alist '("bash-fc" . sh-mode))



;; Org
(add-hook
 'org-mode-hook
 (lambda ()
   ;; Re-bind header navigation keys.
   (local-set-key (kbd "C-,") 'outline-next-visible-heading)
   (local-set-key (kbd "C-.") 'outline-previous-visible-heading)))

(setq
 ;; Open links in this window.
 org-link-frame-setup '((file . find-file))
 ;; Add markdown to export backends.
 org-export-backends '(ascii html icalendar latex md odt)
 ;; Disable line truncation.
 org-startup-truncated nil)



;; Flycheck
(global-flycheck-mode)

;; Display the error list on the bottom of the frame occupying a tenth of the
;; height of the frame.
;;
;; https://www.flycheck.org/en/latest/user/error-list.html#tune-error-list-display
(add-to-list
 'display-buffer-alist
 `(,(rx bos "*Flycheck errors*" eos)
   (display-buffer-reuse-window display-buffer-in-side-window)
   (side            . bottom)
   (reusable-frames . visible)
   (window-height   . 0.1)))



;; Flyspell
;;
;; Enable flyspell-mode and bind ispell-buffer in text modes.
(dolist (hook '(text-mode-hook markdown-mode-hook))
  (add-hook hook (lambda() (flyspell-mode)
                    (local-set-key (kbd "C-c i") 'ispell-buffer))))

;; Enable flyspell-prog-mode and bind ispell-comments-and-strings in programming
;; modes.
(dolist (hook '(c-mode-common-hook emacs-lisp-mode-hook cmake-mode-hook
                                   python-mode-hook))
  (add-hook hook (lambda() (flyspell-prog-mode)
                   (local-set-key (kbd "C-c i") 'ispell-comments-and-strings))))
