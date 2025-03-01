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


;; Org Journal
;; -----------------------------------------------------------------------------

(defun rt/org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)))


(defun rt/set-org-journal-mode (&rest _)
  "Enable org-journal-mode after decrypting an age file in the journal directory."
  (when (and buffer-file-name
             (string-match-p
              (expand-file-name "Documents/notes/journal" (getenv "HOME"))
              buffer-file-name))
    (org-journal-mode)))

;; Along with the function above this allows me to open up journal
;; files and have org-journal-mode set automatically, without this
;; they just open in fundamental-mode
(advice-add 'age-file-insert-file-contents :after #'rt/set-org-journal-mode)


;; gptel
;; -----------------------------------------------------------------------------

(defun rt/get-anthropic-key ()
  "Retrieve the Anthropic API key from auth-source.
Searches for an entry with host 'api.anthropic.com' and user 'apikey'."
  (let ((secret (plist-get (car (auth-source-search :host "api.anthropic.com"
                                                    :user "apikey"))
                           :secret)))
    (when secret
      (if (functionp secret)
          (funcall secret)
        secret))))


(provide 'custom-functions)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; custom-functions.el ends here
