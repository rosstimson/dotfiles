(require 'go-mode-load)

;; gofmt on save
(add-hook 'before-save-hook 'gofmt-before-save)
