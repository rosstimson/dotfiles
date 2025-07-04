;;; early-init.el --- Emacs configuration of Ross Timson -*- lexical-binding: t; -*-


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


;; This file is loaded before the package system and GUI is
;; initialized, so in it you can customize variables that affect frame
;; appearance as well as the package initialization process.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

;; Disable package.el, the built-in package manager, as using elpaca
;; (https://github.com/progfolio/elpaca)
(setq package-enable-at-startup nil)

;; Set path early doors as otherwise native compilation blows up for some reason.
(setenv "PATH" (concat (getenv "HOME") "/Library/pnpm:"
                       "/opt/homebrew/Caskroom/miniconda/base/bin:"
                       "/opt/homebrew/Caskroom/miniconda/base/condabin:"
                       (getenv "HOME") "/.asdf/shims:"
                       (getenv "HOME") "/.local/bin:"
                       (getenv "HOME") "/code/go/bin:"
                       (getenv "HOME") "/.cargo/bin:"
                       (getenv "HOME") "/.npm-global/bin:"
                       (getenv "HOME") "/.krew/bin:"
                       (getenv "HOME") "/bin:"
                       "/usr/local/MacGPG2/bin:"
                       "/opt/homebrew/bin:"
                       "/Applications/Postgres.app/Contents/Versions/latest/bin:"
                       "/Library/TeX/texbin:"
                       "/usr/local/bin:"
                       "/usr/local/sbin:"
                       "/usr/bin:"
                       "/usr/sbin:"
                       "/bin:"
                       "/sbin"))

(setq exec-path (split-string (getenv "PATH") path-separator))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
