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
                   exec-path-from-shell xclip ibuffer-vc ivy counsel company))

(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install-selected-packages)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load features

(require 'cc-mode)
(require 'cc-vars)
(require 'clang-format)
(require 'company)
(require 'desktop)
(require 'exec-path-from-shell)
(require 'flycheck)
(require 'flyspell)
(require 'git-commit)
(require 'hideshow)
(require 'ibuf-ext)
(require 'llvm-mode)
(require 'magit-diff)
(require 'org)
(require 'paren)
(require 'projectile)
(require 'reservoir "~/reservoir/reservoir.el" 'noerror)
(require 'sh-script)
(require 'smerge-mode)
(require 'tablegen-mode)
(require 'tramp)
(require 'view)
(require 'whitespace)

;; The LLVM style isn't provided, so just load the file.
(load "llvm/emacs.el")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings

(setq
 inhibit-startup-screen t
 column-number-mode t
 require-final-newline t
 ;; Turn off bell ring.
 ring-bell-function 'ignore
 ;; Split the current window vertically if it is more than 160 columns.
 split-height-threshold nil
 split-width-threshold 160
 fit-window-to-buffer-horizontally t
 confirm-kill-emacs 'y-or-n-p
 ;; Case-insensitive buffer and file names.
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; Sentences are terminated with a single space.
 sentence-end-double-space nil
 ;; Open the *scratch* buffer in fundamental-mode.
 initial-major-mode 'fundamental-mode
 initial-scratch-message "Welcome, Mr. Whiteside.\n\n"
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
(menu-bar-mode 0)
;; scroll-bar-mode and tool-bar-mode can be void.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))

;; Enable upcase-region.
(put 'upcase-region 'disabled nil)

;; Scroll up and down a line.
(global-set-key (kbd "M-p") 'scroll-up-line)
(global-set-key (kbd "M-n") 'scroll-down-line)

;; Switch windows with an easier key.
(global-set-key (kbd "M-i") 'other-window)
(global-set-key (kbd "M-I") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-c C-s") 'sort-lines)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames

(add-to-list 'default-frame-alist '(width  . 230))
(add-to-list 'default-frame-alist '(height . 85))

;; Disable the transparent toolbar on macOS.
(push '(ns-transparent-titlebar . nil)
 (alist-get 'ns window-system-default-frame-alist))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running in a terminal

(unless window-system
  ;; Enable xterm-mouse-mode.
  (xterm-mouse-mode)
  (xclip-mode)
  ;; Set the mouse wheel to scroll.
  (global-set-key
   [mouse-4] (lambda () (interactive) (mwheel-scroll mouse-wheel-down-event)))
  (global-set-key
   [mouse-5] (lambda () (interactive) (mwheel-scroll mouse-wheel-up-event))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment variables

;; In addition to PATH and MANPATH, set GIT_PAGER from the shell.
(add-to-list 'exec-path-from-shell-variables "GIT_PAGER")

;; My environment variables are set in bash_profile, so a non-interactive, login
;; shell is sufficient.
(setq exec-path-from-shell-arguments '("--login"))

;; Only set variables from the shell if Emacs is running on a window system.
(when window-system (exec-path-from-shell-initialize))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop save

(desktop-save-mode)

;; Save all files (including remote files).
(setq desktop-files-not-to-save "^$")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP

;; Allow the method to be omitted.
(tramp-change-syntax 'simplified)

;; Use the path assigned to the remote user to search for remote programs.
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ibuffer (https://www.emacswiki.org/emacs/IbufferMode)

(add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)

(setq
 ;; Turn off prompt to close unmodified buffers.
 ibuffer-expert t
 ;; Change the color of the group names.
 ibuffer-filter-group-name-face 'font-lock-comment-face
 ibuffer-show-empty-filter-groups nil
 ibuffer-default-sorting-mode 'filename/process
 ibuffer-formats
 '((mark modified read-only " " (name 30 -1 :left) " " (mode 16 16 :left :elide)
         " " filename-and-process)))

;; Don't show special buffers and non-status magit buffers.
(dolist (predicate '("^\\*" "^magit-"))
  (add-to-list 'ibuffer-never-show-predicates predicate))

;; Use IBuffer as buffer list.
(define-key global-map [remap list-buffers] 'ibuffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I-search/Occur

;; https://www.emacswiki.org/emacs/SearchAtPoint
(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)

;; DEL during isearch should edit the search string, not jump back to the
;; previous result.
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(define-key isearch-mode-map (kbd "C-c o") 'isearch-occur)

;; Switch to the Occur buffer if any match is found.
(add-hook 'occur-hook
          (lambda () (unless (eq (get-buffer "*Occur*") (window-buffer))
                       (switch-to-buffer-other-window "*Occur*"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace

(add-hook 'prog-mode-hook 'whitespace-mode)

;; Highlight tabs.
(setq whitespace-style '(face tabs))

;; Change the tab highlight color.
(set-face-background 'whitespace-tab "orange red")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show parentheses

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill column indicator and auto fill

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (display-fill-column-indicator-mode)
                   (auto-fill-mode))))

;; In programming modes, only auto-fill comments.
(add-hook 'prog-mode-hook (lambda () (setq comment-auto-fill-only-comments t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight custom keywords

;; Highlight NB: and TODO: in programming modes.
(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil `(("\\<\\(\\(?:TODO\\|NB\\)\\(?:(\\w*)\\)?:\\)" 1
           'font-lock-constant-face prepend)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy and counsel

(ivy-mode)
(counsel-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company

(global-company-mode)

;; Use tab to activate company completion when the current line has been
;; indented.
(setq tab-always-indent 'complete)
(define-key company-mode-map [remap completion-at-point] 'company-complete)

;; Use company instead of dabbrev-expand.
(define-key company-mode-map (kbd "M-/") 'company-complete)

;; Use the same keys as ivy.
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)



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
;; Flyspell

;; Enable flyspell-mode in text modes. Bind "C-c i" to ispell-buffer.
;; NB(whiteside): local-set-key is used instead of define-key here because
;; flyspell-prog-mode doesn't have its own key map.
(add-hook 'text-mode-hook (lambda() (flyspell-mode)
                            (local-set-key (kbd "C-c i") 'ispell-buffer)))

;; Enable flyspell-prog-mode in programming modes. Bind "C-c i" to
;; ispell-comments-and-strings to avoid spell checking the whole buffer.
(add-hook 'prog-mode-hook
          (lambda() (flyspell-prog-mode)
            (local-set-key (kbd "C-c i") 'ispell-comments-and-strings)))

;; Disable flyspell in sh-mode. There are too many non-English strings.
(add-hook 'sh-mode-hook (lambda () (flyspell-mode 0)))

;; Unset keys which conflict with org-mode.
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide/show

(setq hs-hide-comments-when-hiding-all nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile

(projectile-mode)

(setq projectile-mode-line-function
      (lambda () (format " Proj[%s]" (projectile-project-name))))

;; Set the projectile prefix key.
(define-key projectile-mode-map (kbd "C-c a") 'projectile-command-map)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control--git-commit, magit, and smerge

(setq
 ;; Set the max length of the summary line.
 git-commit-summary-max-length 50
 ;; Check for both a non-empty second line and a long summary line.
 git-commit-style-convention-checks
 '(non-empty-second-line overlong-summary-line))

;; Set the fill column in git-commit-mode to 72 characters.
(add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))

;; When visiting a file from a hunk, open it in another window.
(dolist (map (list magit-hunk-section-map magit-file-section-map))
  (define-key map (kbd "RET") 'magit-diff-visit-file-other-window))

;; Change the command prefix to something easier.
(define-key smerge-mode-map (kbd "C-c v") smerge-basic-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bash

(add-hook
 'sh-mode-hook
 (lambda ()
   ;; Disable << as insert here document.
   (sh-electric-here-document-mode 0)))

;; Use sh-mode to edit bashrc, *_bash, and bash_* files.
(add-to-list 'auto-mode-alist '("\\.?bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("_bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?bash_" . sh-mode))
(add-to-list 'auto-mode-alist '("bash-fc" . sh-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp

(defun whiteside/library-el-with-view-mode ()
  "Open library .el files with function `view-mode'.
If this elisp file ends with .el.gz, or is in an elpa directory,
then open it for viewing, not editing. Open this file with
function `view-mode' and kill the buffer with q."
  (when (and buffer-file-name
             (or (string-match-p "\\.el\\.gz\\'" buffer-file-name)
                 (string-match-p package-user-dir buffer-file-name)))
    (view-mode) (setq view-exit-action 'kill-buffer)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (whiteside/library-el-with-view-mode)
            (add-hook 'after-save-hook 'check-parens)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(setq
 ;; Open links in this window.
 org-link-frame-setup '((file . find-file))
 ;; Disable line truncation.
 org-startup-truncated nil
 ;; TODO statistics cover all entries in the subtree.
 org-hierarchical-todo-statistics nil
 ;; Don't show any separator lines in the outline.
 org-cycle-separator-lines 0)

;; Add markdown to export backends.
(add-to-list 'org-export-backends 'md)

(define-key org-mode-map (kbd "C-,") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "C-.") 'outline-previous-visible-heading)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

;; Set C and clang-format default styles.
(setq c-default-style "llvm.org")
(setq-default clang-format-style "LLVM")

;; Use c++-mode for .h files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Find a header file in the same directory.
(define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)

;; clang-format
(define-key c-mode-base-map (kbd "C-M-<tab>") 'clang-format-region)
(define-key c-mode-base-map (kbd "C-c f")
  (lambda () (interactive) (clang-format-region (point-min) (point-max))))



(provide 'init)
;;; init.el ends here
