(require 'linum-relative)

;; No GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Visually match parens
(show-paren-mode)

;; Relative line numbers
(line-number-mode 0)
(global-linum-mode t)
(linum-relative-on)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'hybrid t)
(setq active-theme 'hybrid)

(provide 'init-visual)
