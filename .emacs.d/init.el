;; init.el --- Emacs configuration of Ross Timson -*- lexical-binding: t; -*-

;; Always load newest byte code
(setq load-prefer-newer t)

;; Use elpaca (https://github.com/progfolio/elpaca) package manager.
(load-file "~/.emacs.d/package-manager.el")

;; Additional load paths.
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Load the rest of my personal settings
(org-babel-load-file (expand-file-name "~/.emacs.d/rosstimson-init.org"))

;; Don't litter main init file with custom-set-variables stuff.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
