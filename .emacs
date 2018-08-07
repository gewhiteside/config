;; turn off bell ring
(setq ring-bell-function 'ignore)

;; set f12 to indent file
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; set c indentation style
(setq c-default-style "linux"
      c-basic-offset 2)

;; set fill column for formatting comments/text
(setq-default fill-column 80)
(setq column-number-mode t)

;; diable menu bar
(menu-bar-mode -1)

;; delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
