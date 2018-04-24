;;;;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;
;; Reorganize all of this into a more reasonable setup
;; Maybe use a literate programming approach
;; There's definitely an audience for this sort of thing
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
;;   - Should get a good Texas method setup
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
;;
;; FOR WORK
;; Get a mapping that opens a tag in a split 

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

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Fix C-u vimemulation
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode t)
(global-linum-mode t)
(require 'linum-relative)
(linum-relative-on)

(defun evil-shell-insert ()
  "Jump to end of shell buffer in insert mode. Saves my left hand from
   effectively chording S-g with S-a."
  (interactive)
  (end-of-buffer)
  (evil-append-line 1))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(require 'cff)

(require 'evil-leader)
(require 'fiplr)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e" 'find-file
  "t" 'eshell
  ;; "p" 'basic-save-buffer
  "b" 'switch-to-buffer
  "a" 'balance-windows-area
  "m" 'man
  "r" 'cff-find-other-file
  "j" 'evil-window-down
  "g" 'magit
  "k" 'evil-window-up
  "l" 'evil-window-right
  "h" 'evil-window-left
  "u" 'evil-scroll-up
  "w" 'evil-scroll-up
  "d" 'evil-scroll-down
  "i" 'evil-shell-insert
  "l" 'linum-relative-toggle
  "s" 'toggle-window-split
  "f" 'clang-format-region)

(require 'evil-commentary)
(evil-commentary-mode)

(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)

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

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
;; (setq-default tab-always-indent 'complete)
(setq initial-scratch-message "")

(require 'company)
(require 'company-simple-complete)
;; company mode everywhere
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))

(define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)

(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".h"))
    ("\\.c\\'" (".h"))))

(require 'magit)
(add-hook 'with-editor-mode-hook 'evil-insert-state)

(add-to-list 'load-path (concat (getenv "PATH_TO_MEMSQL") "/utils/private/emacs/rtags"))
;; (add-to-list 'load-path "/home/evan/memsql/utils/private/emacs/rtags")
(require 'rtags)

(require 'adaptive-wrap)
(setq-default adaptive-wrap-extra-indent 2)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)

;; (setq-default ff-other-file-alist 'my-cpp-other-file-alist)
;; (setq ff-always-in-other-window 1)
;; (setq ff-search-directories (list "." "~/metalfe/include"))

;; (add-hook 'c-initialization-hook (lambda ()
;;     (define-key c-mode-base-map [(meta o)] 'ff-get-other-file))
;; )

;; defines shortcut for find source/header file for the current
;; file
;; (add-hook 'c++-mode-hook
;;           '(lambda ()
;;              (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
;; (add-hook 'c-mode-hook
;;           '(lambda ()
;;              (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))

;; Make TeX-view open zathura instead of stupid evince
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-list (quote (("Zathura" "zathura %o"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes
   (quote
    ("e8ebf4fb99b76dad3b7ec2313d5d091c1f3a3e0e4f9d55d3a58d73a8d3387358" "58572b25dff279a21f80df16a91c45f76bcf206946ae739161404f5b73bfeea1" "b0d8c9f81c9a67f4d41a9056848e4f7da664c4e12ed2870ac7b9c2f5d3c820b8" "ce1b4d9e73ad0a802795a9d87a3bb5c7fae6385f799b765339574312da17704f" "09925db9ced19954de50b076f9723929ed9a0d8a9d51d395840d75d8123af320" "ecb672899531da18413f35fb531e96bdf98b2e5b52fbfc88b9aeb2dbb032c9d3" "5ccedf10d6537e06b61f86668fbc99b72f03438d08d20076b91c515b5e611c5d" "54d9e573780a69fc18e4551cc3a64b9c35712ed2db1aa70fb871b822deb45517" "2195712f18461c096cbcacb28ac2e98af069c429dfd13bea7e7b3070ee2392af" "cae57ae0e23d58934eb0b696a131273481039dc40a05f3cb7ed90570c3171587" "8270d32e3d273e339f7c815426fb79d47e73d10af3f628c12064f85de9b6f42e" "b70a5cf1a9f5b0064cddce9f99bbeb7d1ba4c8469c449f12149cada621ed16e2" "932ed7410a0de773562bd3282371e6b25a616cd5ac7b2a085a1c9af0d2c70787" "f4dc6799efc868f03e6bf59ba2f267938f2867b0e999b421addfc77b4f9a9dd7" "cbc0d140ae652159ce19a5777e0c2122eb05a3610ccace6132268af38fff6cf4" "1ffd1bd5b873e2e54d294212b1d2eede30546682a30bab68874912a914bb4729" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "f50aa7409dff79a72d530a55fdd42cbaa8217d47f83cfb1c753a289e02e49a6b" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" default)))
 '(fci-rule-color "#383838")
 '(font-use-system-font t)
 '(line-number-mode nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (cff fiplr irony company list-packages-ext rainbow-mode exec-path-from-shell ggtags magit highlight-numbers solarized-theme smex powerline linum-relative ido-grid-mode evil-numbers evil-leader evil-commentary dracula-theme atom-one-dark-theme)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I/Users/evan/metalfe/include/" "-I/Users/evan/metalfe/llvm-src/llvm/include/"))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

;; TODO obviously this is a complete hack
(defun run-pdflatex ()
  "run a command on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "pdflatex %s > /dev/null" 
       (shell-quote-argument (buffer-file-name))))
  )

(defun run-xelatex ()
  "run a command on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "xelatex %s > /dev/null" 
       (shell-quote-argument (buffer-file-name))))
  )

(defun run-standalone ()
  "run a command on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "./standalone %s > /dev/null" 
       (shell-quote-argument (buffer-file-name))))
  )

;; This might be vim-purist heresy, but I prefer the behavior of
;; fill-paragraph to evil-fill-and-move. It's a bit more context-aware
;; - its approach to LaTeX enviornments is the killer feature for me
(define-key evil-normal-state-map "gq" 'fill-paragraph)

;; Important window moving bindings
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)

;; Text size
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)

;; Fuck the GUI!
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Visually match parens
(show-paren-mode)

(line-number-mode 0)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'hybrid t)
(setq active-theme 'hybrid)
(defun toggle-solarized-theme ()
  (interactive)
  (if (eq active-theme 'solarized-light)
      (setq active-theme 'solarized-dark)
    (setq active-theme 'solarized-light))
  (load-theme active-theme))

(require 'powerline)
(powerline-center-evil-theme)

(visual-line-mode 1)
(require 'smart-quotes)
;; TODO need to turn off for some things
;; (add-hook 'text-mode-hook 'turn-on-smart-quotes)

(require 'ido)
(require 'ido-grid-mode)
(ido-mode t)
(ido-grid-mode t)
;; TODO maybe delete this; not sure if I had issues last time.
(ido-everywhere t)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; (require 'org-journal)
;; (setq 'org-journal-dir "/home/evan/org-journal")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; Fuck 'yes' and 'no'
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'auto-mode-alist '("\\.metal\\'" . c++-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

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

;; Want to bind something to a combination of
;; move-end-of-line (or evil-goto-line)
;; evil-insert or (evil-append-line)

(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/Users/evan/build/metalfe/build-metalfe-host/Debug/build/bin:" eshell-path-env))
  ;; (setenv "PATH_TO_MEMSQL" "/home/evan/memsql")
  (setenv "PATH" (concat "/Users/evan/build/metalfe/build-metalfe-host/Debug/build/bin:" (getenv "PATH"))))

(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)

;; (setq eshell-prompt-function (lambda nil
;;    (concat
;;     (propertize "λ " 'face `(:foreground "#b58900"))
;;     (propertize (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))
;;     'face `(:foreground "#839496"))
;;     " "))
;;       eshell-prompt-regexp "λ ")

;; Bugginess made me give up and just do identical prompt and regexp
(setq eshell-prompt-function (lambda nil (concat (propertize "λ" 'face `(:foreground "#b58900"))
						 (propertize " " 'face `(:foreground "#c5c8c6"))
						 ))
      eshell-prompt-regexp               (concat (propertize "λ" 'face `(:foreground "#b58900"))
						 (propertize " " 'face `(:foreground "#c5c8c6"))
						 )
      )
(setq read-file-name-completion-ignore-case t)

;; TODO map eshell-next-matching-input to Control R
;; TODO(Bold) implement comint-history-isearch-backward behavior for eshell
;; TODO substring tab completion in eshell
;; TODO ^^ can this use ido somehow?

(add-hook 'eshell-mode-hook
     (lambda ()
       (local-set-key (kbd "M-P") 'eshell-previous-prompt)
       (local-set-key (kbd "M-N") 'eshell-next-prompt)
       (local-set-key (kbd "M-R") 'eshell-list-history)
       (local-set-key (kbd "M-s")
              (lambda ()
                (interactive)
                (insert
                 (ido-completing-read "Eshell history: "
                                      (delete-dups
                                       (ring-elements eshell-history-ring))))))))

;; TODO IDO history completion in a python REPL
;; Can write something like rlwrap for eshell+ido?

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; (defvar ido-enable-replace-completing-read t
;;     "If t, use ido-completing-read instead of completing-read if possible.
    
;;     Set it to nil using let in around-advice for functions where the
;;     original completing-read is required.  For example, if a function
;;     foo absolutely must use the original completing-read, define some
;;     advice like this:
    
;;     (defadvice foo (around original-completing-read-only activate)
;;       (let (ido-enable-replace-completing-read) ad-do-it))")
    
;;     ;; Replace completing-read wherever possible, unless directed otherwise
;;     (defadvice completing-read
;;       (around use-ido-when-possible activate)
;;       (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
;;               (and (boundp 'ido-cur-list)
;;                    ido-cur-list)) ; Avoid infinite loop from ido calling this
;;           ad-do-it
;;         (let ((allcomp (all-completions "" collection predicate)))
;;           (if allcomp
;;               (setq ad-return-value
;;                     (ido-completing-read prompt
;;                                    allcomp
;;                                    nil require-match initial-input hist def))
;;             ad-do-it))))

;;;;;;;;;;;;; BELOW HERE IS AUTO-GEN'd ;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
