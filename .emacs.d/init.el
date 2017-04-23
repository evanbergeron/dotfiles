;;;;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;
;; Aesthetics
;;   - Make quotes match nicely
;;   - Splash screen with LRU files + fun h@ck0r vibe
;; 
;; LaTex
;;   - Compile on save
;;   - Auto-reflow on save
;;   - Easy keymapping to open pdf in split
;;
;; IDO
;;   - Fuzzy completion
;;   - Vertical layout (or grid layout)
;;   - Tab completion (kill the default tab completion)
;; 
;; Doc-view
;;   - Vim-style keybindings
;;
;; Org-mode
;;   - Learn
;;
;; Magit
;;   - Learn
;;
;; General preferences
;;   - Tab completion
;;   - Access ncmpcpp or cmus from inside
;;   - Email
;;   - Persistent undo
;;   - Get rid of autogen'd font and appearence code
;;
;; General bugs
;;   - C-h doesn't always work for switching panes
;;

(require 'package)

; List the packages you want
(setq package-list '(evil
                     evil-leader))

; Add Melpa as the default Emacs Package repository
; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

; Activate all the packages (in particular autoloads)
(package-initialize)

; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Fix C-u vimemulation
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode t)
(global-linum-mode t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e" 'find-file
  "t" 'ansi-term
  "b" 'switch-to-buffer
  "a" 'balance-windows-area
  "w" 'save-buffer)

;; TODO not sure if I need a
;; (require 'evil-commentary)
;; here
(evil-commentary-mode)

;; This might be vim-purist heresy, but I prefer the behavior of
;; fill-paragraph to evil-fill-and-move. It's a bit more context-aware
;; - its approach to LaTeX enviornments is the killer feature for me
(define-key evil-normal-state-map "gq" 'fill-paragraph)

;; Important window moving bindings
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)

;; Fuck the GUI!
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Visually match parens
(show-paren-mode)

(require 'linum-relative)
(linum-relative-on)

(line-number-mode 0)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-dark t)

(require 'powerline)
(powerline-default-theme)

(visual-line-mode 1)

;; TODO this tab mapping conflicts with zlc plugin I believe
;; (or maybe the default tab completion behavior
;; 'ido-prev-match also exists
(require 'ido)
(ido-mode t)
(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "\t") 'ido-next-match)
(add-hook 'ido-setup-hook 'ido-define-keys))

;;;;;;;;;;;;; BELOW HERE IS AUTO-GEN'd ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; What even is a GUI ;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" default)))
 '(font-use-system-font t)
 '(line-number-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata for Powerline" :foundry "PfEd" :slant normal :weight normal :height 110 :width normal)))))
