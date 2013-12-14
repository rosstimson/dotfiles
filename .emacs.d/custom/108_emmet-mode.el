(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup mode
(add-hook 'css-mode-hook 'emmet-mode) ;; Aut-start in css mode.

;; Set default indent depth of HTML abbreviations
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;; Position cursor between first empty quotes after expanding
(setq emmet-mode-cursor-between-quotes t) ;; defaults to nil
