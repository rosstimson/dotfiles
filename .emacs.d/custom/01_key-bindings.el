;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c r") 'bury-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda ()
                                  (interactive)
                                  (other-window 2))) ;; forward two

;; Indentation help
(global-set-key (kbd "C-x ^") 'join-line)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; Toggle whitespace-mode
(global-set-key "\C-c_w" 'whitespace-mode)

;; Start proced in a similar manner to dired
(global-set-key (kbd "C-x p") 'proced)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

;; program shortcuts - s stands for windows key(super)
(global-set-key (kbd "s-w") 'browse-url)          ;; Browse (W3M)
(global-set-key (kbd "s-l") 'linum-mode)          ;; show line numbers in buffer
(global-set-key (kbd "s-r") 're-builder)          ;; build regular expressions
(global-set-key (kbd "s-d") 'sr-speedbar-toggle)  ;; Toggle sr-speedbar
(global-set-key (kbd "s-p") 'projectile-find-file)        ;; Projectile fuzzy find in project
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer) ;; Projectile fuzzy switch buffer

;; Super + uppercase letter signifies a buffer/file
(global-set-key (kbd "s-S")                       ;; scratch
                (lambda()(interactive)(switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-E")                       ;; .emacs
                (lambda()(interactive)(find-file "~/.emacs.d")))

;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Replace dired with dired+
(global-set-key (kbd "C-x d") 'diredp-dired-files)
(global-set-key (kbd "C-x 4 d") 'diredp-dired-files-other-window)

;; swap windows
(global-set-key (kbd "C-c s") 'swap-windows)

;; duplicate the current line or region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; rename buffer & visited file
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

;; macros
(global-set-key [f10]  'start-kbd-macro)
(global-set-key [f11]  'end-kbd-macro)
(global-set-key [f12]  'call-last-kbd-macro)

;; Toggle auto-fill-mode
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Replace M-x with smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; Old M-x

;; Toggle commenting
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)
