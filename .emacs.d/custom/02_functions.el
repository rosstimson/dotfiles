;; My custom fucntions
;;

;; Use ido to open recent files
(require 'recentf)

(setq recentf-exclude '("~/.emacsregisters.el" "~/.ido.last")
      recentf-max-saved-items 50)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
