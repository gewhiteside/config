;;; init.el --- My Emacs configuration
;;; Commentary:

;; A lot of this configuration is inspired by Steve Purcell's Emacs
;; configuration (https://github.com/purcell/emacs.d) and Spacemacs
;; (https://develop.spacemacs.org/).

;;; Code:

;; (setq debug-on-error t)

(let ((minimum-version "27.1"))
  (when (version< emacs-version minimum-version)
    (error "This Emacs is outdated--this configuration requires v%s or higher"
           minimum-version)))

;; Add elisp and its subdirectories to the load path.
(let ((default-directory (expand-file-name "elisp" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Store customization info in a separate file so this file isn't polluted.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Because package-initialize is called in this file, don't call it on startup.
(setq package-enable-at-startup nil)
(package-initialize)

(setq package-selected-packages
      '(projectile flycheck clang-format cmake-mode markdown-mode magit
                   exec-path-from-shell xclip ibuffer-vc))

(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install-selected-packages)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load features

(require 'cc-vars)
(require 'clang-format)
(require 'desktop)
(require 'exec-path-from-shell)
(require 'git-commit)
(require 'ibuf-ext)
(require 'llvm-mode)
(require 'org)
(require 'projectile)
(require 'reservoir "~/reservoir/reservoir.el" 'noerror)
(require 'sh-script)
(require 'smerge-mode)
(require 'tablegen-mode)
(require 'tramp)
(require 'whitespace)
(require 'magit-diff)

;; The LLVM style isn't provided, so just load the file.
(load "llvm/emacs.el")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 initial-scratch-message "Welcome, Mr. Whiteside.\n\n"
 ;; Default to the home directory.
 default-directory "~/"
 ;; Select the help window when it is opened.
 help-window-select t
 uniquify-buffer-name-style 'reverse
 make-backup-files nil)

;; Remove mode-line-mule-info and VC information from the mode line.
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   mode-line-position
   " "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

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
(global-set-key (kbd "M-p") 'scroll-up-line)
(global-set-key (kbd "M-n") 'scroll-down-line)

;; Switch windows with an easier key.
(global-set-key (kbd "M-i") 'other-window)
(global-set-key (kbd "M-I") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-c C-s") 'sort-lines)

;; A list of text modes for adding common hooks.
(defvar text-modes '(text-mode-hook markdown-mode-hook))

;; A list of programming modes for adding common hooks.
(defvar prog-modes '(c-mode-common-hook emacs-lisp-mode-hook cmake-mode-hook
                                   python-mode-hook sh-mode-hook))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill column indicator

(global-display-fill-column-indicator-mode)

;; Disable the fill column indicator in certain modes.
(dolist (hook '(help-mode-hook completion-list-mode-hook shell-mode-hook
                               dired-mode-hook))
  (add-hook hook (lambda () (display-fill-column-indicator-mode 0))))
;; Disable the transparent toolbar on macOS.
(push '(ns-transparent-titlebar . nil)
 (alist-get 'ns window-system-default-frame-alist))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git

(setq
 ;; Set the max length of the summary line.
 git-commit-summary-max-length 50
 ;; Check for both a non-empty second line and a long summary line.
 git-commit-style-convention-checks
 '(non-empty-second-line overlong-summary-line))

;; Set the fill column in git-commit-mode to 72 characters.
(add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))



;; Magit
;;
;; When visiting a file from a hunk, open it in another window.
(define-key magit-hunk-section-map (kbd "RET")
  'magit-diff-visit-file-other-window)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMerge

;; Change the command prefix to something easier.
(add-hook 'smerge-mode-hook
          (lambda () (local-set-key (kbd "C-c s") smerge-basic-map)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop save

(desktop-save-mode)

;; Save all files (including remote files).
(setq desktop-files-not-to-save "^$")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 ;; Set default sorting to major mode.
 ibuffer-default-sorting-mode 'major-mode
 ;; Set custom formats.
 ibuffer-formats
 '((mark modified read-only " " (name 40 40 :left :elide) " "
         (mode 16 16 :left :elide) " " filename-and-process)))

;; Don't show special buffers and non-status magit buffers.
(dolist (predicate '("^\\*" "^magit-"))
  (add-to-list 'ibuffer-never-show-predicates predicate))

;; Use IBuffer as buffer list.
(global-set-key (kbd "C-x C-b") 'ibuffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(setq-default clang-format-style "LLVM")

;; Use c++-mode for .h files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(define-key org-mode-map (kbd "C-,") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "C-.") 'outline-previous-visible-heading)

(setq
 ;; Open links in this window.
 org-link-frame-setup '((file . find-file))
 ;; Add markdown to export backends.
 org-export-backends '(ascii html icalendar latex md odt)
 ;; Disable line truncation.
 org-startup-truncated nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
(global-flycheck-mode)

(setq flycheck-emacs-lisp-load-path 'inherit)

;; Display the error list on the bottom of the frame occupying a tenth of the
;; height of the frame
;; (https://www.flycheck.org/en/latest/user/error-list.html#tune-error-list-display).
(add-to-list
 'display-buffer-alist
 `(,(rx bos "*Flycheck errors*" eos)
   (display-buffer-reuse-window display-buffer-in-side-window)
   (side            . bottom)
   (reusable-frames . visible)
   (window-height   . 0.1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell and auto fill

;; Enable flyspell-mode and auto-fill-mode and bind ispell-buffer in text modes.
(dolist (hook text-modes)
  (add-hook hook (lambda() (flyspell-mode) (auto-fill-mode)
                   (local-set-key (kbd "C-c i") 'ispell-buffer))))

;; Enable flyspell-prog-mode auto-fill-mode and bind ispell-comments-and-strings
;; in programming modes.
(dolist (hook prog-modes)
  (add-hook hook (lambda() (flyspell-prog-mode) (auto-fill-mode)
                   ;; Only auto-fill inside comments.
                   (setq comment-auto-fill-only-comments t)
                   (local-set-key (kbd "C-c i") 'ispell-comments-and-strings))))

;; Disable flyspell in sh-mode. There are too many non-English strings.
(add-hook 'sh-mode-hook (lambda () (flyspell-mode 0)))

(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile

(projectile-mode)

(setq projectile-mode-line-function
      (lambda () (format " Proj[%s]" (projectile-project-name))))

;; Set the projectile prefix key.
(define-key projectile-mode-map (kbd "C-c a") 'projectile-command-map)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP

;; Allow the method to be omitted.
(tramp-change-syntax 'simplified)

;; Use the path assigned to the remote user to search for remote programs.
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment variables

;; In addition to PATH and MANPATH, set GIT_PAGER from the shell.
(add-to-list 'exec-path-from-shell-variables "GIT_PAGER")

;; My environment variables are set in bash_profile, so a non-interactive, login
;; shell is sufficient.
(setq exec-path-from-shell-arguments '("--login"))

;; Only set variables from the shell if Emacs is running on a window system.
(when window-system (exec-path-from-shell-initialize))


(provide 'init)
;;; init.el ends here
