;;; rosstimson-muted-theme.el --- A muted theme.

;;; Code:

(deftheme rosstimson-muted
  "A light theme with a muted colour palette.")

(let ((class '((class color) (min-colors 89)))
      (fg "#444") (fg-light "#666") (fg-lighter "#999") (fg-dark "#333") (fg-darker "#222")
      (bg "#e5e3d9") (bg-light "#F5F3E8") (bg-lighter "#FCFAEF") (bg-dark "#DBD9D0") (bg-darker "#D1CFC6")
      (red "#825b69") (red-light "#bda0aa")
      (green "#69825b") (green-light "#aabda0")
      (yellow "#82755b") (yellow-light "#bdb3a0")
      (blue "#5b6982") (blue-light "#a0aabd")
      (magenta "#755b82") (magenta-light "#b3a0bd")
      (cyan "#5b8275") (cyan-light "#a0bdb3")
      (white "#cacaca") (white-light "#fff"))

  (custom-theme-set-faces
   'rosstimson-muted
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,fg))))
   
;;; Highlighting faces
   `(fringe ((,class (:background ,bg-darker))))
   `(highlight ((,class (:background ,white-light))))
   `(region ((,class (:background ,blue-light :foreground ,fg-dark))))
   `(secondary-selection ((,class (:background ,yellow-light))))
   `(isearch ((,class (:foreground ,fg-dark :background ,green-light))))
   `(isearch-fail ((,class (:foreground ,fg-dark :background ,red-light))))
   `(lazy-highlight ((,class (:background ,yellow-light))))
   `(trailing-whitespace ((,class (:background ,red-light))))
   `(hl-line ((,class (:background ,bg-light))))
   `(show-paren-match ((,class (:background ,white))))
   `(show-paren-mismatch ((,class (:background ,red-light :foreground ,white-light))))

;;; Mode line faces
   `(mode-line ((,class (:box nil :background ,bg-lighter))))
   `(mode-line-inactive ((,class (:box nil :background ,bg-dark))))
   `(mode-line-highlight ((,class (:box nil :foreground ,blue))))

;;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground ,blue))))
   ;; `(escape-glyph ((,class (:foreground ,red-3))))
   ;; `(homoglyph ((,class (:foreground ,red-3))))
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,yellow))))
   `(success ((,class (:foreground ,green))))

;;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,red))))
   `(font-lock-comment-face ((,class (:slant italic :foreground ,fg-lighter))))
   `(font-lock-constant-face ((,class (:weight bold :foreground ,cyan-light))))
   `(font-lock-function-name-face ((,class (:foreground ,cyan))))
   `(font-lock-keyword-face ((,class (:foreground ,blue))))
   `(font-lock-string-face ((,class (:foreground ,magenta))))
   `(font-lock-type-face ((,class (:foreground ,green))))
   `(font-lock-variable-name-face ((,class (:foreground ,yellow))))

;;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue))))
   `(link-visited ((,class (:underline t :foreground ,magenta))))

;;; Web Mode
   `(web-mode-current-column-highlight-face ((,class (:background ,white))))
   `(web-mode-current-element-highlight-face ((,class (:background ,white))))

;;; Ivy Mode
   `(ivy-current-match ((,class (:foreground ,fg :background ,white))))
   `(ivy-minibuffer-match-face-1 ((,class (:weight bold :foreground ,cyan))))
   `(ivy-minibuffer-match-face-2 ((,class (:weight bold :foreground ,green))))
   `(ivy-minibuffer-match-face-3 ((,class (:weight bold :foreground ,magenta))))
   `(ivy-minibuffer-match-face-4 ((,class (:weight bold :foreground ,blue))))
   `(ivy-minibuffer-match-highlight ((,class (:foreground ,green))))
   `(ivy-confirm-face ((,class (:foreground ,green))))
   `(ivy-match-required-face ((,class (:foreground ,red))))
   `(ivy-virtual ((,class (:foreground ,cyan))))
   `(ivy-action ((,class (:foreground ,blue))))

;;; eshell
   `(eshell-prompt ((,class (:foreground ,cyan))))
   `(eshell-ls-directory ((,class (:foreground ,blue))))
   `(eshell-ls-executable ((,class (:foreground ,red))))
   `(eshell-ls-symlink ((,class (:foreground ,magenta))))
   `(eshell-ls-special ((,class (:background ,red-light))))

;;; Company
   `(company-tooltip ((,class (:foreground ,fg :background ,bg-lighter))))
   `(company-tooltip-selection ((,class (:background ,white))))
   `(company-scrollbar-bg ((,class (:background ,white-light))))
   `(company-scrollbar-fg ((,class (:background ,magenta-light))))

;;; YASnippet
   `(yas-field-highlight-face ((,class (:background ,white-light))))

;;; js2-mode
   `(js2-error ((,class (:underline (:color ,red :style wave)))))
   `(js2-warning ((,class (:underline (:color ,yellow-light :style line)))))
   `(js2-external-variable ((,class (:foreground ,red))))

;;; Org Mode
   `(org-todo ((,class (:weight bold :foreground ,red))))
   `(org-done ((,class (:weight bold :foreground ,green))))
   `(org-tag ((,class (:weight bold :foreground ,yellow))))
   `(org-date ((,class (:foreground ,magenta))))
   `(org-special-keyword ((,class (:foreground ,magenta-light))))
   `(org-drawer ((,class (:foreground ,cyan))))
   ))


(provide-theme 'rosstimson-muted)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; rosstimson-muted-theme.el ends here
