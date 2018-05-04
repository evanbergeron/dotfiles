;; Fix C-u vimemulation
(setq evil-want-C-u-scroll t)
(require 'evil)
(require 'evil-leader)
(require 'evil-commentary)
(require 'evil-numbers)
(require 'key-chord)


(evil-mode t)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "a" 'balance-windows-area
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "h" 'evil-window-left
  "u" 'evil-scroll-up
  "d" 'evil-scroll-down
  "i" 'evil-shell-insert
  "s" 'toggle-window-split)
  ;; "f" 'clang-format-region)

(evil-commentary-mode)

(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;; Emulate
;;   noremap <silent> k gk
;;   noremap <silent> j gj
;; from vimrc.
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "$") 'evil-end-of-visual-line)

;; There's probably a bit of loss of functionality here, as the
;; default binding for 0 in evil mode is
;; evil-digit-argument-or-evil-beginning-of-line, which sounds more
;; complicated.
(define-key evil-normal-state-map (kbd "0") 'evil-beginning-of-visual-line)
(define-key evil-normal-state-map (kbd "^") 'evil-first-non-blank-of-visual-line)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
  (define-key evil-motion-state-map (kbd ";") 'evil-ex))

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; This might be vim-purist heresy, but I prefer the behavior of
;; fill-paragraph to evil-fill-and-move. It's a bit more context-aware
;; - its approach to LaTeX enviornments is the killer feature for me
(define-key evil-normal-state-map "gq" 'fill-paragraph)

(provide 'init-evil)
