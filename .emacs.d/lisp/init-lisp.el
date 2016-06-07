;;; init-lisp.el --- Emacs configuration of Ross Timson -*- lexical-binding: t; -*-
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

;; Configuration for editing Lisp code.

;;; Code:

(use-package smartparens
  :commands (smartparens-mode
	     smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(use-package slime
  :commands slime-mode
  :config
  (setq inferior-lisp-program "clisp")
  (slime-setup '(slime-fancy))

  (add-hook 'slime-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode)

  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)

  (add-hook 'lisp-mode-hook #'slime-lisp-mode-hook))

(provide 'init-lisp)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init-lisp.el ends here
