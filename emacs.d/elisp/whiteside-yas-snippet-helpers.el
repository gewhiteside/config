;;; whiteside-yas-snippet-helpers --- Helpers for my more complicated snippets.
;;; Commentary:

;; Helper functions I use in my snippets. I find it easier to keep them in an
;; actual package instead of defining them inline.

;;; Code:

(defun whiteside/llvm-file-header-file-name ()
  "Return the name of this file for a LLVM file header.
See https://llvm.org/docs/CodingStandards.html#file-headers for a
detailed description of an LLVM file header. If this buffer is
not visiting a file, then return the empty string."
  (if buffer-file-name
      (if (string-match-p "/include/" buffer-file-name)
          (replace-regexp-in-string ".*/include/" "" buffer-file-name )
        (file-name-nondirectory buffer-file-name))
    ""))

(defun whiteside/llvm-include-guard ()
  "Return the include guard for this file.
Return a string to be used as an include guard for this file
accoring to
https://llvm.org/docs/CodingStandards.html#header-guard. If this
buffer is not visiting a file or this file is not in an include
directory, then return an empty string."
  (if (and buffer-file-name (string-match-p "/include/" buffer-file-name))
      (concat
       (upcase
        (replace-regexp-in-string
         "/" "_"
         (replace-regexp-in-string
          ".*/include/" "" (file-name-sans-extension buffer-file-name))))
       "_H")
    ""))

(defun whiteside/pad-with-dashes (pad-width &rest strings)
  "Return a string with 0 or more dashes padding to PAD-WIDTH.
If the combined width of STRINGS is less than PAD-WIDTH, then
return a string which will pad STRINGS to PAD-WIDTH when
everything is concatenated together."
  (let ((dashes-needed (- pad-width (apply '+ (mapcar 'string-width strings)))))
    (if (> dashes-needed 0) (make-string dashes-needed ?-))))

(provide 'whiteside-yas-snippet-helpers)
;;; whiteside-yas-snippet-helpers.el ends here
