;;; init.el --- Emacs configuration of Ross Timson -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016-2016 Ross Timson <ross@rosstimson.com>
;;
;; Author: Ross Timson <ross@rosstimson.com>
;; URL: https://gihub.com/rosstimson/dotfiles
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; Copyright (c) 2016, Ross Timson <ross@rosstimson>

;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE
;; FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
;; DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
;; IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
;; OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; Emacs configuration of Ross Timson.

;;; Code:


;;; Package Management

;; Always load newest byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)

;;; Standard package repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; Essential settings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; i18n
(prefer-coding-system 'utf-8)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 12   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; User interface
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inihibt-startup-echo-area-message "rosstimson")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode -1)
(global-hl-line-mode t)
(line-number-mode t)
(column-number-mode t)
(setq-default indicate-empty-lines t)

;; Start maximised
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Short Yes/No questions
(fset 'yes-or-no-p #'y-or-n-p)

;; Enable Ido everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Use Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; Colour theme
(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-night 'no-confirm))


;;; Packages

;; Always ensure packages are installed automatically if not present
(setq use-package-always-ensure t)

(use-package whitespace
  :defer t
  :diminish (whitespace-mode . " Ⓦ"))

;;; Requires
(require 'init-evil)
(require 'init-dired)
(require 'init-helm)

;; Branching undo
(use-package undo-tree
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

;; Better M-x
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; Enhanced package list
(use-package paradox
  :commands (paradox-upgrade-packages paradox-list-packages)
  :config (setq paradox-execute-asynchronously t))

;; Auto-completion
(use-package company
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  :diminish company-mode)

;; Sort Company completion candidates by stats
(use-package company-statistics
  :after company
  :config (company-statistics-mode))

;; The one and only Git frontend
(use-package magit
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g p" . magit-pull))
  :config (use-package evil-magit))

;; Highlight delimiters by depth
(use-package rainbow-delimiters
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

;; Displays current match and total matches of searches
(use-package evil-anzu
  :init
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode t)
  :diminish anzu-mode isearch-mode)

;; Flycheck, modern on the fly syntax checking.
(use-package flycheck
  :init (global-flycheck-mode)
  :bind ("C-c t f" . flycheck-mode)
  :diminish (flycheck-mode . " Ⓢ"))

(use-package spaceline-config
  :ensure spaceline
  :init
  (setq powerline-default-separator 'wave
        powerline-height (truncate (* 1.2 (frame-char-height)))
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-minor-modes-separator nil)
  :config
  (spaceline-spacemacs-theme)
  ;; Tomorrow Night Colours for Evil states.
  (set-face-attribute 'spaceline-evil-emacs nil :background "#b294bb")    ; Purple
  (set-face-attribute 'spaceline-evil-insert nil :background "#81a2be")   ; Blue
  (set-face-attribute 'spaceline-evil-motion nil :background "#b294bb")   ; Purple
  (set-face-attribute 'spaceline-evil-normal nil :background "#b5bd68")   ; Green
  (set-face-attribute 'spaceline-evil-replace nil :background "#cc6666")  ; Red
  (set-face-attribute 'spaceline-evil-visual nil :background "#f0c674"))  ; Yellow

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  :diminish projectile-mode)

(use-package helm-projectile
  :config (helm-projectile-on))

(use-package yasnippet
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs "~/.emacs.d/snippets")
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)
  :diminish (yas-minor-mode . " Ⓨ"))

;; Simple copy paste with system clipboard
(use-package simpleclip
  :bind (("M-c" . simpleclip-copy)
         ("M-v" . simpleclip-paste)))


;;; Major modes

(use-package go-mode
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save)
  :init
  (progn
    (defun rosstimson/maybe-gofmt-before-save ()
      (when (eq major-mode 'go-mode)
	(gofmt-before-save)))
    (add-hook 'before-save-hook 'rosstimson/maybe-gofmt-before-save)))

;; Load custom file last
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
