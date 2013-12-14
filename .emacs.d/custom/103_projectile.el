(require 'projectile)
(require 'grizzl)
(projectile-global-mode) ; enable in all buffers
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
