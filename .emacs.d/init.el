;;;;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;
;; Aesthetics
;;   - Splash screen with LRU files + fun h@ck0r vibe
;; 
;; LaTex
;;   - Compile on save
;;   - Auto-reflow on save
;;   - Easy keymapping to open pdf in split
;;
;; IDO
;;   - Fuzzy completion
;; 
;; Doc-view
;;   - Vim-style keybindings
;;
;; Org-mode
;;   - Learn
;;   - Use for personal finances + cost of various recipes!
;;   - Use for lifting data!
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
;;   - No line numbers in eshell
;;
;; General bugs
;;   - C-h doesn't always work for switching panes
;;   - Fix LaTeX quotes
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
(require 'linum-relative)
(linum-relative-on)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e" 'find-file
  "t" 'eshell
  "b" 'switch-to-buffer
  "a" 'balance-windows-area
  "w" 'save-buffer)

(require 'evil-commentary)
(evil-commentary-mode)

(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;; Make TeX-view open zathura instead of stupid evince
(custom-set-variables
 '(TeX-view-program-list (quote (("Zathura" "zathura %o")))) ; [1]
 '(TeX-view-program-selection
  (quote
   (((output-dvi style-pstricks) "dvips and gv")
   (output-dvi "xdvi")
   (output-pdf "Zathura")                                    ; [2]
   (output-html "xdg-open")))))

;; TODO obviously this is a complete hack
(defun run-pdflatex ()
  "run a command on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "pdflatex %s > /dev/null" 
       (shell-quote-argument (buffer-file-name))))
  )

(add-to-list 'load-path "~/.emacs.d/lisp/")

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

(line-number-mode 0)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-dark t)

(require 'powerline)
(powerline-center-evil-theme)

(visual-line-mode 1)
(require 'smart-quotes)
(add-hook 'text-mode-hook 'turn-on-smart-quotes)

(require 'ido)
(require 'ido-grid-mode)
(ido-mode t)
(ido-grid-mode t)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; Eshell prompt stuff
(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                               (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))  ;; Otherwise, we just return the PWD

(setq eshell-prompt-function (lambda nil
   (concat
    (propertize "λ " 'face `(:foreground "#b58900"))
    (propertize (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))
    'face `(:foreground "#839496"))
    " "
   )))

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
