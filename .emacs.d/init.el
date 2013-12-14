(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/custom")

(load "00_general-settings.el")
(load "01_key-bindings.el")
(load "02_functions.el")

;; Tools 1xx_name.el
(load "100_magit.el")
(load "101_sr-speedbar.el")
(load "102_ag.el")
(load "103_projectile.el")
(load "104_ido-mode.el")
(load "105_smex.el")
(load "106_yasnippet.el")
(load "107_dired-plus.el")
(load "108_emmet-mode.el")

;; Langs / Major modes 2xx_name.el
(load "200_org-mode.el")
(load "201_enh-ruby-mode.el")
(load "202_web-mode.el")
(load "203_sass-mode.el")
(load "204_markdown-mode.el")
(load "205_coffee-mode.el")
(load "206_js2-mode.el")
(load "207_go-mode.el")
(load "208_php-mode.el")
(load "209_slim-mode.el")
