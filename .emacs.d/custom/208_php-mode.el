(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Coding style
(setq php-mode-coding-style 'pear)

;; Hook to use PHP_CodeSniffer with compile command (M-x compile).
;; Associated shortcuts should be available to jump between errors.
(defun rt/phpcs ()
 (set (make-local-variable 'compile-command) (format "phpcs --report=emacs --standard=PEAR %s" (buffer-file-name))))
(add-hook 'php-mode-hook 'rt/phpcs)

;; Code completion with Company
(defun company-my-php-backend (command &optional arg &rest ignored)
    (case command
      ('prefix (and (eq major-mode 'php-mode)
                    (company-grab-symbol)))
      ('sorted t)
      ('candidates (all-completions
                    arg 
                    (if (and (boundp 'my-php-symbol-hash)
                             my-php-symbol-hash)
                        my-php-symbol-hash

                      (message "Fetching completion list...")

                      (with-current-buffer
                          (url-retrieve-synchronously "http://php.net/quickref.php")

                        (goto-char (point-min))

                        (if (re-search-forward "<!-- result list start -->" nil t)
                            (let ((end (save-excursion
                                         (if (re-search-forward "<!-- result list end -->" nil t)
                                             (point)))))
                              (if end
                                  (let ((hash (make-hash-table)))
                                    (while (re-search-forward ">\\([^<]+\\)</a>" end t)
                                      (puthash (match-string 1) t hash))
                                    (setq my-php-symbol-hash hash)))))))))))
