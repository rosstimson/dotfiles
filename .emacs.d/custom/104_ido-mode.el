(require 'ido)
(ido-mode t)

;; Fuzzy matching
(setq ido-enable-flex-matching t)

(add-hook 'ido-setup-hook 'rosstimson/ido-extra-keys)

;; Custom keybindings for ido
(defun rosstimson/ido-extra-keys ()
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map "\C-b" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))
