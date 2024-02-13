;;; custom-functions.el --- My Custom Functions

;;; Code:

(defun rt/sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."

  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\_<.*?\\_>" "\\&" beg end))

(provide 'custom-functions)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; custom-functions.el ends here
