;;; init.el --- Emacs configuration of Ross Timson -*- lexical-binding: t; -*-


;; Author: Ross Timson <ross@rosstimson.com>
;; URL: https://gihub.com/rosstimson/dotfiles
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; Copyright (c) 2016, Ross Timson <ross@rosstimson>

;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; Emacs configuration of Ross Timson.

;;; Code:


;;; Package Management

;; Always load newest byte code
(setq load-prefer-newer t)

;; Use straight.el (https://github.com/raxod502/straight.el) a next-gen package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/master/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package via Straight and make the integration a default
;; so that I don't have to repeat `:straight t` everywhere.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Important to install here before reading my main config that uses
;; org mode.  This way the built-in org mode doesn't load and
;; therefore the latest and greatest gets installed and used rather
;; that the older built-in stuff.
(straight-use-package '(org))

;; Additional load paths.
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Load the rest of my personal settings
(org-babel-load-file (expand-file-name "~/.emacs.d/rosstimson-init.org"))

;; Don't litter main init file with custom-set-variables stuff.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here
