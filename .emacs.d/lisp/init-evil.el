;;; init-evil.el --- Emacs configuration of Ross Timson -*- lexical-binding: t; -*-
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

;; Evil configuration, Evil is an extensible vi layer for Emacs.

;;; Code:

(use-package evil
  :init (progn
          (setq evil-want-C-u-scroll t
                evil-overriding-maps nil
                evil-intercept-maps nil))

          (use-package evil-leader
            :init (global-evil-leader-mode t)
            :config (progn
                      (setq evil-leader/in-all-states t)
                      (evil-leader/set-leader ",")
                      ;; Keyboard shortcuts
                      (evil-leader/set-key
		       "b" 'ido-switch-buffer
                       "B" 'ibuffer
                       "f" 'ido-find-file
                       "g" 'magit-status
                       "x" 'smex
                       )))

          (use-package evil-surround
            :init (global-evil-surround-mode t))

          (use-package evil-commentary
            :init (evil-commentary-mode t)
            :diminish evil-commentary-mode)

  :config (evil-mode 1))

;; Escape everything
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Ibuffer
(eval-after-load 'ibuffer
  '(progn
     (evil-set-initial-state 'ibuffer-mode 'normal)
     (evil-define-key 'normal ibuffer-mode-map
       (kbd "j") 'ibuffer-forward-line
       (kbd "k") 'ibuffer-backward-line
       (kbd "J") 'ibuffer-jump-to-buffer))) ; 'j'

(provide 'init-evil) 

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init-evil.el ends here
