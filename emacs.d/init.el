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

(setq
 package-selected-packages
 '(avy clang-format clipetty cmake-mode company counsel crontab-mode diminish
       exec-path-from-shell flycheck highlight-escape-sequences ibuffer-vc ivy
       lsp-ivy lsp-mode magit markdown-mode projectile ssh-config-mode which-key
       yaml-mode yasnippet))

(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install-selected-packages)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load features

(require 'abbrev)
(require 'cc-mode)
(require 'cc-vars)
(require 'clang-format)
(require 'company)
(require 'desktop)
(require 'eldoc)
(require 'exec-path-from-shell)
(require 'flycheck)
(require 'flyspell)
(require 'git-commit)
(require 'hideshow)
(require 'ibuf-ext)
(require 'llvm-mode)
(require 'lsp-clangd)
(require 'lsp-mode)
(require 'lsp-pyls)
(require 'magit-diff)
(require 'man)
(require 'org)
(require 'paren)
(require 'projectile)
(require 'server)
(require 'sh-script)
(require 'smerge-mode)
(require 'subword)
(require 'tablegen-mode)
(require 'tramp)
(require 'view)
(require 'whiteside-yas-snippet-helpers)
(require 'whitespace)
(require 'yasnippet)

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
 initial-scratch-message nil
 command-line-default-directory "~/"
 default-directory "~/"
 ;; Select the man and help windows when they are opened.
 help-window-select t
 Man-notify-method 'aggressive
 uniquify-buffer-name-style 'reverse
 make-backup-files nil
 revert-without-query '(".*"))

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

;; Typed text replaces an active selection.
(delete-selection-mode)

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

;; Prefer comment-line to comment-dwim.
(global-set-key (kbd "M-;") 'comment-line)

(global-set-key (kbd "C-c C-s") 'sort-lines)
(global-set-key (kbd "C-c c") 'compile)

(defun whiteside/move-end-of-line-newline ()
  "Insert a newline at the end of current line."
  (interactive) (move-end-of-line nil) (newline-and-indent))

(global-set-key (kbd "S-<return>") 'whiteside/move-end-of-line-newline)

;; If a server is not running, start it. If this is a daemon, server-start will
;; be called automatically, so don't call it here.
(unless (or (server-running-p) (daemonp)) (server-start))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which key

(which-key-mode)



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
  (global-clipetty-mode)
  ;; Set the mouse wheel to scroll.
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment variables

;; My environment variables are set in bash_profile, so a non-interactive, login
;; shell is sufficient.
(setq exec-path-from-shell-arguments '("--login"))

;; Only set variables from the shell if Emacs is running on a window system.
(when window-system (exec-path-from-shell-initialize))

;; When connected via ssh and running as a daemon, use a standard location for
;; the authentication socket so that it can be updated if it changes.
(when (and (daemonp) (not (string= "" (getenv "SSH_CONNECTION"))))
  (setenv "SSH_AUTH_SOCK" (expand-file-name "~/.ssh/ssh_auth_sock")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop save

;; If this Emacs is a daemon, it will not be restarted across many sessions and
;; therefore has no need to save state.
(unless (daemonp) (desktop-save-mode))

(defun desktop-read-force ()
  "Force reading from a locked desktop."
  (interactive) (let ((desktop-load-locked-desktop 't)) (desktop-read)))

(setq
 ;; If the desktop is locked, don't ask to steal it or to save a new one.
 desktop-load-locked-desktop nil
 desktop-save 'if-exists)

;; Also save some of the minibuffer history.
(add-to-list 'desktop-globals-to-save '(minibuffer-history . 50))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP

;; Allow the method to be omitted.
(tramp-change-syntax 'simplified)

;; Use the path assigned to the remote user to search for remote programs.
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ibuffer (https://www.emacswiki.org/emacs/IbufferMode)

(add-hook 'ibuffer-hook (lambda () (ibuffer-vc-set-filter-groups-by-vc-root)
                          ;; Auto refresh buffer list.
                          (ibuffer-auto-mode)))

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

;; Don't show special buffers and non-status Magit buffers.
(dolist (predicate '("^\\*" "^magit-"))
  (add-to-list 'ibuffer-never-show-predicates predicate))

;; Use Ibuffer as buffer list.
(define-key global-map [remap list-buffers] 'ibuffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I-search/Occur

;; https://www.emacswiki.org/emacs/SearchAtPoint
(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)

;; DEL during Isearch should edit the search string, not jump back to the
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

;; Don't show whitespace when view-mode is enabled.
(add-hook 'view-mode-hook (lambda () (whitespace-mode 0)))

;; Highlight tabs and trailing whitespace.
(setq whitespace-style '(face tabs trailing))

;; Change the tab highlight color to match trailing whitespace.
(set-face-background 'whitespace-tab "red1")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto address

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parentheses/pairs

(show-paren-mode)
(electric-pair-mode)



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
;; Highlight escape sequences

(hes-mode)
(put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
(put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy, counsel, and swiper

(ivy-mode)
(counsel-mode)

(global-set-key (kbd "C-s") 'swiper)
(global-unset-key (kbd "C-r"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company

(global-company-mode)

(add-hook 'shell-mode-hook (lambda () (company-mode 0)))

(setq company-idle-delay 0.1
      company-show-numbers t)

(define-key company-mode-map [remap completion-at-point] 'company-complete)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yas-snippet

(yas-global-mode)

(add-to-list 'yas-snippet-dirs "~/reservoir/snippets" 't)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

(global-flycheck-mode)

;; Disable Flycheck in the scratch buffer.
(add-hook 'flycheck-mode-hook
          (lambda () (when (string-match-p "^\\*scratch\\*$" (buffer-name))
                       (flycheck-mode 0))))

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
   (window-height   . 0.15)
   (dedicated       . t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell

;; Enable flyspell-mode in text modes. Bind "C-c i" to ispell-buffer.
;; NB(whiteside): local-set-key is used instead of define-key here because
;; flyspell-prog-mode doesn't have its own key map.
(add-hook 'text-mode-hook (lambda() (flyspell-mode)
                            (local-set-key (kbd "C-c i i") 'ispell-buffer)))

;; Enable flyspell-prog-mode in programming modes. Bind "C-c i" to
;; ispell-comments-and-strings to avoid spell checking the whole buffer.
(add-hook 'prog-mode-hook
          (lambda() (flyspell-prog-mode)
            (local-set-key (kbd "C-c i i") 'ispell-comments-and-strings)))

(setq ispell-silently-savep 't
      ispell-program-name "aspell"
      ;; Enable spell checking for CamelCase words.
      ispell-extra-args '("--run-together"))

;; Don't spell check strings in programming modes; there are just too many
;; non-English strings
;; (https://emacs.stackexchange.com/questions/31300/can-you-turn-on-flyspell-for-comments-but-not-strings).
(setq flyspell-prog-text-faces
      (delq 'font-lock-string-face flyspell-prog-text-faces))

;; Unset keys which conflict with org-mode.
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)

(define-key flyspell-mode-map (kbd "C-c i o") 'flyspell-buffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide/show

(setq hs-hide-comments-when-hiding-all nil)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(define-key hs-minor-mode-map (kbd "C-c f") 'hs-toggle-hiding)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy

(global-set-key (kbd "M-'") 'avy-goto-char-timer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile

(projectile-mode)

(setq projectile-mode-line-function
      (lambda () (format " Proj[%s]" (projectile-project-name))))

;; Set the projectile prefix key.
(define-key projectile-mode-map (kbd "C-c a") 'projectile-command-map)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp

(setq
 ;; Don't show code actions or project diagnostics in the mode line.
 lsp-modeline-code-actions-enable nil lsp-modeline-diagnostics-enable nil
 ;; Disable the breadcrumb headline.
 lsp-headerline-breadcrumb-enable nil)

;; clangd
(setq lsp-clients-clangd-args '("--header-insertion=never"))

;; pyls
(setq lsp-pyls-plugins-pydocstyle-enabled 't)

;; Prefer YAPF over autopep8.
(setq lsp-pyls-plugins-autopep8-enabled nil
      lsp-pyls-plugins-yapf-enabled 't)

(dolist (mode-hook '(c++-mode-hook python-mode-hook))
  (add-hook mode-hook 'lsp))

;; (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

(define-key lsp-mode-map (kbd "C-c r") 'lsp-format-region)
(define-key lsp-mode-map (kbd "C-c C-f") 'lsp-format-buffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control--git-commit, Magit, and Smerge

(setq
 ;; Set the max length of the summary line.
 git-commit-summary-max-length 50
 ;; Check for both a non-empty second line and a long summary line.
 git-commit-style-convention-checks
 '(non-empty-second-line overlong-summary-line))

;; Set the fill column in git-commit-mode to 72 characters.
(add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))

(setq
 magit-diff-refine-hunk 'all
 magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

;; When visiting a file from a hunk, open it in another window.
(dolist (map (list magit-hunk-section-map magit-file-section-map))
  (define-key map (kbd "RET") 'magit-diff-visit-file-other-window))

(dolist (hide-section '(branch unpushed))
  (add-to-list 'magit-section-initial-visibility-alist `(,hide-section . hide)))

;; Change the command prefix to something easier.
(define-key smerge-mode-map (kbd "C-c v") smerge-basic-map)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ssh_config

(add-to-list 'auto-mode-alist '("/\\.?ssh/config\\'" . ssh-config-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bash

;; Use sh-mode to edit bashrc, *_bash, and bash_* files.
(add-to-list 'auto-mode-alist '("\\.?bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("_bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?bash_" . sh-mode))
(add-to-list 'auto-mode-alist '("bash-fc" . sh-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp

(defun sort-elisp-symbols (beg end)
  "Sort Emacs Lisp symbols in the active region.
Lexicographical sort the symbols, i.e. characters in the [:word:]
character class, possibly joined by a dash, from BEG to END."
  (interactive"r") (sort-regexp-fields nil "[[:word:]-]+" "\\&" beg end))

(defun whiteside/library-el-with-view-mode ()
  "Open library .el.gz files with function `view-mode'.
If this elisp file ends with .el.gz, then open it for viewing,
not editing. Open this file with function `view-mode' and kill
the buffer with q."
  (when (and buffer-file-name (string-match-p "\\.el\\.gz\\'" buffer-file-name))
    (view-mode) (setq view-exit-action 'kill-buffer)))

(add-hook 'after-save-hook
          (lambda () (if (eq major-mode 'emacs-lisp-mode) 'check-parens)))

(add-hook 'emacs-lisp-mode-hook 'whiteside/library-el-with-view-mode)



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

;; Enable speed commands on any star in a header.
(setq org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp)
                      (looking-back "^\**" nil))))

;; Add markdown to export back ends.
(add-to-list 'org-export-backends 'md)

(define-key org-mode-map (kbd "C-,") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "C-.") 'outline-previous-visible-heading)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(add-hook 'c++-mode-hook 'subword-mode)

;; Set C and clang-format default styles.
(setq c-default-style "llvm.org")
(setq-default clang-format-style "LLVM")

;; Use c++-mode for .h files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Find a header file in the same directory.
(define-key c-mode-base-map (kbd "C-c o") 'projectile-find-other-file)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminish

;; NB(whiteside): Modes must be diminished after they are activated, so this
;; section should be at the end of this file.
(dolist
    (mode
     '(abbrev-mode auto-fill-function auto-revert-mode clipetty-mode
                   company-mode counsel-mode eldoc-mode flyspell-mode
                   hs-minor-mode ivy-mode subword-mode which-key-mode
                   yas-minor-mode))
  (diminish mode))


(provide 'init)
;;; init.el ends here
