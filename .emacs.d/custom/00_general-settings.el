;; Add /usr/local/bin to path (Flyspell could not detect aspell without this)
(setq exec-path (append exec-path '("/usr/local/bin")))

;; To get rid of Weird color escape sequences in Emacs.
;; Instruct Emacs to use emacs term-info not system term info
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setq system-uses-terminfo nil)

;; Prefer utf-8 encoding.
(prefer-coding-system 'utf-8)

;; Sensible buffer names if you visit two files with same name but within
;; different directories.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save location of the point when you kill a buffer and return to it next
;; time you visit the associated file.
(require 'saveplace)
(setq-default save-place t)

;; Whitespace
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq whitespace-line-column 80)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq default-indicate-empty-lines 1) ;; Dashes in left-hand fringe.

;; Display continuous lines
(setq-default truncate-lines nil)

;; The bell is pretty obnoxious.
(setq ring-bell-function 'ignore)

;; Interface
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-message t)

;; Highlight current line
(global-hl-line-mode)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Auto-indent new lines
(electric-indent-mode 1)

;; Column numbers
(column-number-mode 1)

;; Font settings
(global-font-lock-mode 1)

(set-frame-font "Source Code Pro")
(set-face-attribute 'default nil :font "Source Code Pro" :height 120)
(set-face-font 'default "Source Code Pro")

;; Colour theme
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(setq custom-safe-themes t) ;; Stop asking if the theme is safe, I can trust my own
(setq custom-enabled-themes '(rt-misterioso))
(load-theme 'rt-misterioso)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight incremental search
(setq search-highlight t)
(transient-mark-mode t)

;; Use visual line mode
(global-visual-line-mode 1)

;; Disable backups and auto-save
(setq backup-inhibited t)
(setq auto-save-default nil)

;; IDO settings
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;; Spelling / Flyspell settings, let's use real English
(setq ispell-program-name "aspell")
(setq ispell-dictionary "british")

;; Activate auto-fill-mode and Flyspell for all text mode buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Activate Eldoc minor mode in Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; Default to unified diffs
(setq diff-switches "-u")

;; Keep a list of recently opened files
(recentf-mode 1)

;; Use European style calendar for Diary mode
(setq european-calendar-style t)
