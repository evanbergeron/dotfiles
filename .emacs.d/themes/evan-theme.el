;;; zenburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2017 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 2.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme evan "The Evan color theme")

;;; Color Palette

(defvar evan-default-colors-alist
  '(("evan-fg+1"     . "#c5c8c6")
    ("evan-fg"       . "#c5c8c6")
    ("evan-fg-1"     . "#c5c8c6")
    ("evan-bg-2"     . "#232c31")
    ("evan-bg-1"     . "#232c31")
    ("evan-bg-05"    . "#232c31")
    ("evan-bg"       . "#232c31")
    ("evan-bg+05"    . "#232c31") 
    ("evan-bg+1"     . "#232c31")
    ("evan-bg+2"     . "#232c31")
    ("evan-bg+3"     . "#232c31")
    ("evan-red+1"    . "#DCA3A3")
    ("evan-red"      . "#cc6666")
    ("evan-red-1"    . "#cc6666")
    ("evan-red-2"    . "#cc6666")
    ("evan-red-3"    . "#5f0000")
    ("evan-red-4"    . "#5f0000")
    ("evan-orange"   . "#de935f")
    ("evan-yellow"   . "#f0c674")
    ("evan-yellow-1" . "#f0c674")
    ("evan-yellow-2" . "#f0c674")
    ("evan-green-1"  . "#5F7F5F")
    ("evan-green"    . "#b5bd68")
    ("evan-green+1"  . "#b5bd68")
    ("evan-green+2"  . "#b5bd68")
    ("evan-green+3"  . "#b5bd68")
    ("evan-green+4"  . "#b5bd68")
    ("evan-cyan"     . "#005f5f")
    ("evan-blue+1"   . "#94BFF3")
    ("evan-blue"     . "#81a2be")
    ("evan-blue-1"   . "#81a2be")
    ("evan-blue-2"   . "#81a2be")
    ("evan-blue-3"   . "#81a2be")
    ("evan-blue-4"   . "#00005f")
    ("evan-blue-5"   . "#00005f")
    ("evan-comment"  ."#6c7a80")
    ("evan-magenta"  . "#b294bb"))
  "List of Evan colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defvar evan-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar evan-colors-alist
  (append evan-default-colors-alist evan-override-colors-alist))

(defmacro evan-with-color-variables (&rest body)
  "`let' bind all colors defined in `evan-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   evan-colors-alist))
     ,@body))

;;; Theme Faces
(evan-with-color-variables
  (custom-theme-set-faces
   'evan
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,evan-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,evan-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,evan-fg :background ,evan-bg))))
   `(cursor ((t (:foreground ,evan-fg :background ,evan-fg+1))))
   `(escape-glyph ((t (:foreground ,evan-yellow :weight bold))))
   `(fringe ((t (:foreground ,evan-fg :background ,evan-bg+1))))
   `(header-line ((t (:foreground ,evan-yellow
                                  :background ,evan-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,evan-bg-05))))
   `(success ((t (:foreground ,evan-green :weight bold))))
   `(warning ((t (:foreground ,evan-orange :weight bold))))
   `(tooltip ((t (:foreground ,evan-fg :background ,evan-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,evan-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,evan-green))))
   `(compilation-error-face ((t (:foreground ,evan-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,evan-fg))))
   `(compilation-info-face ((t (:foreground ,evan-blue))))
   `(compilation-info ((t (:foreground ,evan-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,evan-green))))
   `(compilation-line-face ((t (:foreground ,evan-yellow))))
   `(compilation-line-number ((t (:foreground ,evan-yellow))))
   `(compilation-message-face ((t (:foreground ,evan-blue))))
   `(compilation-warning-face ((t (:foreground ,evan-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,evan-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,evan-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,evan-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,evan-fg-1))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,evan-fg))))
   `(grep-error-face ((t (:foreground ,evan-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,evan-blue))))
   `(grep-match-face ((t (:foreground ,evan-orange :weight bold))))
   `(match ((t (:background ,evan-bg-1 :foreground ,evan-orange :weight bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,evan-yellow-2 :weight bold :background ,evan-bg+2))))
   `(isearch-fail ((t (:foreground ,evan-fg :background ,evan-red-4))))
   `(lazy-highlight ((t (:foreground ,evan-yellow-2 :weight bold :background ,evan-bg-05))))

   `(menu ((t (:foreground ,evan-fg :background ,evan-bg))))
   `(minibuffer-prompt ((t (:foreground ,evan-yellow))))
   `(mode-line
     ((,class (:foreground ,evan-green+1
                           :background ,evan-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,evan-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,evan-green-1
                      :background ,evan-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,evan-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,evan-bg+2))))
   `(trailing-whitespace ((t (:background ,evan-red))))
   `(vertical-border ((t (:foreground ,evan-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,evan-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,evan-comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,evan-comment))))
   `(font-lock-constant-face ((t (:foreground ,evan-green+4))))
   `(font-lock-doc-face ((t (:foreground ,evan-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,evan-yellow))))
   `(font-lock-keyword-face ((t (:foreground ,evan-blue :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,evan-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,evan-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,evan-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,evan-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,evan-green))))
   `(font-lock-type-face ((t (:foreground ,evan-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,evan-orange))))
   `(font-lock-warning-face ((t (:foreground ,evan-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,evan-fg))))
   `(newsticker-default-face ((t (:foreground ,evan-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,evan-green+3))))
   `(newsticker-extra-face ((t (:foreground ,evan-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,evan-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,evan-green))))
   `(newsticker-new-item-face ((t (:foreground ,evan-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,evan-red))))
   `(newsticker-old-item-face ((t (:foreground ,evan-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,evan-fg))))
   `(newsticker-treeview-face ((t (:foreground ,evan-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,evan-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,evan-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,evan-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,evan-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,evan-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,evan-bg-1 :foreground ,evan-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,evan-fg-1 :background ,evan-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,evan-green+2 :background ,evan-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,evan-fg-1 :background ,evan-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,evan-green+1))))
   `(android-mode-error-face ((t (:foreground ,evan-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,evan-fg))))
   `(android-mode-verbose-face ((t (:foreground ,evan-green))))
   `(android-mode-warning-face ((t (:foreground ,evan-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,evan-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,evan-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,evan-bg :background ,evan-green))))
   `(anzu-match-2 ((t (:foreground ,evan-bg :background ,evan-orange))))
   `(anzu-match-3 ((t (:foreground ,evan-bg :background ,evan-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,evan-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,evan-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,evan-yellow))))
   `(font-latex-italic-face ((t (:foreground ,evan-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,evan-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,evan-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,evan-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,evan-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,evan-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,evan-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,evan-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,evan-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,evan-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,evan-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,evan-bg :background ,evan-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,evan-bg :background ,evan-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,evan-bg :background ,evan-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,evan-bg :background ,evan-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,evan-bg :background ,evan-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,evan-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,evan-bg+3 :foreground ,evan-bg-2))))
   `(ac-selection-face ((t (:background ,evan-blue-4 :foreground ,evan-fg))))
   `(popup-tip-face ((t (:background ,evan-yellow-2 :foreground ,evan-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,evan-yellow-2 :foreground ,evan-bg-2))))
   `(popup-summary-face ((t (:background ,evan-bg+3 :foreground ,evan-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,evan-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,evan-bg-1))))
   `(popup-isearch-match ((t (:background ,evan-bg :foreground ,evan-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,evan-fg-1 :background ,evan-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,evan-green+3 :background ,evan-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,evan-yellow :background ,evan-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,evan-red+1 :background ,evan-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,evan-cyan :background ,evan-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,evan-fg :background ,evan-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,evan-orange :background ,evan-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,evan-orange :background ,evan-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,evan-fg :background ,evan-bg-1))))
   `(company-tooltip-mouse ((t (:background ,evan-bg-1))))
   `(company-tooltip-common ((t (:foreground ,evan-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,evan-green+2))))
   `(company-scrollbar-fg ((t (:background ,evan-bg-1))))
   `(company-scrollbar-bg ((t (:background ,evan-bg+2))))
   `(company-preview ((t (:background ,evan-green+2))))
   `(company-preview-common ((t (:foreground ,evan-green+2 :background ,evan-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,evan-yellow-1 :foreground ,evan-bg))))
   `(bm-fringe-face ((t (:background ,evan-yellow-1 :foreground ,evan-bg))))
   `(bm-fringe-persistent-face ((t (:background ,evan-green-1 :foreground ,evan-bg))))
   `(bm-persistent-face ((t (:background ,evan-green-1 :foreground ,evan-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,evan-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,evan-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,evan-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,evan-cyan))))
   `(cfw:face-saturday ((t (:foreground ,evan-blue :weight bold))))
   `(cfw:face-select ((t (:background ,evan-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,evan-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,evan-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,evan-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,evan-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,evan-green+1))))
   `(cider-deprecated-face ((t (:background ,evan-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,evan-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,evan-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,evan-red-4))))
   `(cider-test-error-face ((t (:background ,evan-magenta))))
   `(cider-test-success-face ((t (:background ,evan-green-1))))
   `(cider-fringe-good-face ((t (:foreground ,evan-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,evan-cyan))))
   `(circe-my-message-face ((t (:foreground ,evan-fg))))
   `(circe-fool-face ((t (:foreground ,evan-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,evan-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,evan-fg))))
   `(circe-server-face ((t (:foreground ,evan-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,evan-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,evan-orange :background ,evan-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,evan-fg)))
   `(context-coloring-level-1-face ((t :foreground ,evan-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,evan-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,evan-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,evan-orange)))
   `(context-coloring-level-5-face ((t :foreground ,evan-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,evan-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,evan-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,evan-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,evan-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,evan-blue :foreground ,evan-bg))))
   `(ctbl:face-continue-bar ((t (:background ,evan-bg-05 :foreground ,evan-bg))))
   `(ctbl:face-row-select ((t (:background ,evan-cyan :foreground ,evan-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,evan-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,evan-green))))
   `(debbugs-gnu-new ((t (:foreground ,evan-red))))
   `(debbugs-gnu-pending ((t (:foreground ,evan-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,evan-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,evan-red))))
;;;;; diff
   `(diff-added          ((t (:background "#335533" :foreground ,evan-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,evan-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,evan-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,evan-green+4))))
   `(diff-refine-change  ((t (:background "#888811" :foreground ,evan-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,evan-red))))
   `(diff-header ((,class (:background ,evan-bg+2))
                  (t (:background ,evan-fg :foreground ,evan-bg))))
   `(diff-file-header
     ((,class (:background ,evan-bg+2 :foreground ,evan-fg :weight bold))
      (t (:background ,evan-fg :foreground ,evan-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,evan-blue :background ,evan-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,evan-red+1 :background ,evan-red-1))))
   `(diff-hl-insert ((,class (:foreground ,evan-green+1 :background ,evan-green-1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,evan-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,evan-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,evan-orange))))
   `(diredp-date-time ((t (:foreground ,evan-magenta))))
   `(diredp-deletion ((t (:foreground ,evan-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,evan-red))))
   `(diredp-dir-heading ((t (:foreground ,evan-blue :background ,evan-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,evan-cyan))))
   `(diredp-exec-priv ((t (:foreground ,evan-red))))
   `(diredp-executable-tag ((t (:foreground ,evan-green+1))))
   `(diredp-file-name ((t (:foreground ,evan-blue))))
   `(diredp-file-suffix ((t (:foreground ,evan-green))))
   `(diredp-flag-mark ((t (:foreground ,evan-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,evan-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,evan-red))))
   `(diredp-link-priv ((t (:foreground ,evan-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,evan-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,evan-orange))))
   `(diredp-no-priv ((t (:foreground ,evan-fg))))
   `(diredp-number ((t (:foreground ,evan-green+1))))
   `(diredp-other-priv ((t (:foreground ,evan-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,evan-red-1))))
   `(diredp-read-priv ((t (:foreground ,evan-green-1))))
   `(diredp-symlink ((t (:foreground ,evan-yellow))))
   `(diredp-write-priv ((t (:foreground ,evan-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,evan-red :weight bold))))
   `(dired-async-message ((t (:foreground ,evan-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,evan-yellow))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,evan-fg :background ,evan-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,evan-fg :background ,evan-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,evan-fg :background ,evan-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,evan-fg :background ,evan-blue-5))))
   `(ediff-even-diff-A ((t (:background ,evan-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,evan-bg+1))))
   `(ediff-even-diff-B ((t (:background ,evan-bg+1))))
   `(ediff-even-diff-C ((t (:background ,evan-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,evan-fg :background ,evan-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,evan-fg :background ,evan-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,evan-fg :background ,evan-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,evan-fg :background ,evan-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,evan-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,evan-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,evan-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,evan-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,evan-fg))))
   `(egg-help-header-1 ((t (:foreground ,evan-yellow))))
   `(egg-help-header-2 ((t (:foreground ,evan-green+3))))
   `(egg-branch ((t (:foreground ,evan-yellow))))
   `(egg-branch-mono ((t (:foreground ,evan-yellow))))
   `(egg-term ((t (:foreground ,evan-yellow))))
   `(egg-diff-add ((t (:foreground ,evan-green+4))))
   `(egg-diff-del ((t (:foreground ,evan-red+1))))
   `(egg-diff-file-header ((t (:foreground ,evan-yellow-2))))
   `(egg-section-title ((t (:foreground ,evan-yellow))))
   `(egg-stash-mono ((t (:foreground ,evan-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,evan-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,evan-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,evan-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,evan-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,evan-green))))
   `(elfeed-search-feed-face ((t (:foreground ,evan-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,evan-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,evan-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,evan-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,evan-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,evan-green+2 :background ,evan-bg))))
   `(w3m-lnum-match ((t (:background ,evan-bg-1
                                     :foreground ,evan-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,evan-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,evan-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,evan-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,evan-yellow))))
   `(erc-keyword-face ((t (:foreground ,evan-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,evan-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,evan-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,evan-green))))
   `(erc-pal-face ((t (:foreground ,evan-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,evan-orange :background ,evan-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,evan-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,evan-green+4 :background ,evan-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,evan-red :background ,evan-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,evan-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,evan-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,evan-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,evan-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,evan-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,evan-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,evan-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,evan-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-red-1) :inherit unspecified))
      (t (:foreground ,evan-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-yellow) :inherit unspecified))
      (t (:foreground ,evan-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-cyan) :inherit unspecified))
      (t (:foreground ,evan-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,evan-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,evan-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,evan-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,evan-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,evan-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,evan-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-orange) :inherit unspecified))
      (t (:foreground ,evan-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-red) :inherit unspecified))
      (t (:foreground ,evan-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,evan-fg))))
   `(ack-file ((t (:foreground ,evan-blue))))
   `(ack-line ((t (:foreground ,evan-yellow))))
   `(ack-match ((t (:foreground ,evan-orange :background ,evan-bg-1 :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,evan-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,evan-blue+1  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,evan-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,evan-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,evan-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,evan-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,evan-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,evan-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,evan-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,evan-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, evan-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,evan-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,evan-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,evan-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,evan-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,evan-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,evan-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,evan-blue))))
   `(gnus-summary-high-read ((t (:foreground ,evan-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,evan-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,evan-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,evan-blue))))
   `(gnus-summary-low-read ((t (:foreground ,evan-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,evan-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,evan-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,evan-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,evan-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,evan-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,evan-fg))))
   `(gnus-summary-selected ((t (:foreground ,evan-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,evan-blue))))
   `(gnus-cite-10 ((t (:foreground ,evan-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,evan-yellow))))
   `(gnus-cite-2 ((t (:foreground ,evan-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,evan-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,evan-green+2))))
   `(gnus-cite-5 ((t (:foreground ,evan-green+1))))
   `(gnus-cite-6 ((t (:foreground ,evan-green))))
   `(gnus-cite-7 ((t (:foreground ,evan-red))))
   `(gnus-cite-8 ((t (:foreground ,evan-red-1))))
   `(gnus-cite-9 ((t (:foreground ,evan-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,evan-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,evan-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,evan-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,evan-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,evan-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,evan-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,evan-bg+2))))
   `(gnus-signature ((t (:foreground ,evan-yellow))))
   `(gnus-x ((t (:background ,evan-fg :foreground ,evan-bg))))
   `(mm-uu-extract ((t (:background ,evan-bg-05 :foreground ,evan-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,evan-blue))))
   `(guide-key/key-face ((t (:foreground ,evan-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,evan-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,evan-green
                      :background ,evan-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,evan-yellow
                      :background ,evan-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,evan-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,evan-bg+1))))
   `(helm-visible-mark ((t (:foreground ,evan-bg :background ,evan-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,evan-green+4 :background ,evan-bg-1))))
   `(helm-separator ((t (:foreground ,evan-red :background ,evan-bg))))
   `(helm-time-zone-current ((t (:foreground ,evan-green+2 :background ,evan-bg))))
   `(helm-time-zone-home ((t (:foreground ,evan-red :background ,evan-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,evan-orange :background ,evan-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,evan-magenta :background ,evan-bg))))
   `(helm-bookmark-info ((t (:foreground ,evan-green+2 :background ,evan-bg))))
   `(helm-bookmark-man ((t (:foreground ,evan-yellow :background ,evan-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,evan-magenta :background ,evan-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,evan-red :background ,evan-bg))))
   `(helm-buffer-process ((t (:foreground ,evan-cyan :background ,evan-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,evan-fg :background ,evan-bg))))
   `(helm-buffer-size ((t (:foreground ,evan-fg-1 :background ,evan-bg))))
   `(helm-ff-directory ((t (:foreground ,evan-cyan :background ,evan-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,evan-fg :background ,evan-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,evan-green+2 :background ,evan-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,evan-red :background ,evan-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,evan-yellow :background ,evan-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,evan-bg :background ,evan-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,evan-cyan :background ,evan-bg))))
   `(helm-grep-file ((t (:foreground ,evan-fg :background ,evan-bg))))
   `(helm-grep-finish ((t (:foreground ,evan-green+2 :background ,evan-bg))))
   `(helm-grep-lineno ((t (:foreground ,evan-fg-1 :background ,evan-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,evan-red :background ,evan-bg))))
   `(helm-match ((t (:foreground ,evan-orange :background ,evan-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,evan-cyan :background ,evan-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,evan-fg-1 :background ,evan-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,evan-fg :background ,evan-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,evan-fg :background ,evan-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,evan-yellow :background ,evan-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,evan-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,evan-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,evan-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,evan-red-1 :background ,evan-bg))))
   `(hydra-face-amaranth ((t (:foreground ,evan-red-3 :background ,evan-bg))))
   `(hydra-face-blue ((t (:foreground ,evan-blue :background ,evan-bg))))
   `(hydra-face-pink ((t (:foreground ,evan-magenta :background ,evan-bg))))
   `(hydra-face-teal ((t (:foreground ,evan-cyan :background ,evan-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,evan-bg-1 :foreground ,evan-orange))))
   `(info-constant-ref-item ((t (:background ,evan-bg-1 :foreground ,evan-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,evan-bg-1 :foreground ,evan-yellow))))
   `(info-function-ref-item ((t (:background ,evan-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,evan-bg-1 :foreground ,evan-yellow))))
   `(info-menu ((t (:foreground ,evan-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,evan-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,evan-bg-1 :foreground ,evan-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,evan-bg-1 :foreground ,evan-blue+1))))
   `(info-user-option-ref-item ((t (:background ,evan-bg-1 :foreground ,evan-red))))
   `(info-variable-ref-item ((t (:background ,evan-bg-1 :foreground ,evan-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,evan-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,evan-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,evan-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,evan-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,evan-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,evan-green+3))))
   `(irfc-title-face ((t (:foreground ,evan-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,evan-green :background ,evan-bg))))
   `(ivy-match-required-face ((t (:foreground ,evan-red :background ,evan-bg))))
   `(ivy-remote ((t (:foreground ,evan-blue :background ,evan-bg))))
   `(ivy-subdir ((t (:foreground ,evan-yellow :background ,evan-bg))))
   `(ivy-current-match ((t (:foreground ,evan-yellow :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,evan-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,evan-green-1))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,evan-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,evan-green+1))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,evan-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,evan-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,evan-yellow))))
   `(ido-indicator ((t (:foreground ,evan-yellow :background ,evan-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,evan-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,evan-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,evan-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,evan-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,evan-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,evan-orange))))
   `(jabber-roster-user-error ((t (:foreground ,evan-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,evan-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,evan-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,evan-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,evan-green+3))))
   `(jabber-activity-face((t (:foreground ,evan-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,evan-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,evan-orange))))
   `(js2-error ((t (:foreground ,evan-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,evan-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,evan-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,evan-green+3))))
   `(js2-function-param ((t (:foreground, evan-orange))))
   `(js2-external-variable ((t (:foreground ,evan-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,evan-green-1))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,evan-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,evan-red-1))))
   `(js2-object-property ((t (:foreground ,evan-blue+1))))
   `(js2-magic-paren ((t (:foreground ,evan-blue-5))))
   `(js2-private-function-call ((t (:foreground ,evan-cyan))))
   `(js2-function-call ((t (:foreground ,evan-cyan))))
   `(js2-private-member ((t (:foreground ,evan-blue-1))))
   `(js2-keywords ((t (:foreground ,evan-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,evan-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,evan-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,evan-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,evan-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,evan-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,evan-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,evan-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,evan-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,evan-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,evan-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,evan-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,evan-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,evan-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,evan-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,evan-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,evan-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,evan-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,evan-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,evan-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,evan-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,evan-comment :background ,evan-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,evan-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,evan-bg :background ,evan-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,evan-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,evan-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,evan-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,evan-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,evan-green+2 :background ,evan-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,evan-blue-1))))
   `(lui-hilight-face ((t (:foreground ,evan-green+2 :background ,evan-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,evan-green+2 :background ,evan-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,evan-red+1 :background ,evan-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,evan-blue+1 :background ,evan-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,evan-magenta :background ,evan-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,evan-yellow :background ,evan-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,evan-bg+05))))
   `(magit-section-heading             ((t (:foreground ,evan-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,evan-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,evan-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,evan-bg+05
                                                        :foreground ,evan-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,evan-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,evan-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,evan-bg+2
                                                        :foreground ,evan-orange))))
   `(magit-diff-lines-heading          ((t (:background ,evan-orange
                                                        :foreground ,evan-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,evan-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,evan-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,evan-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,evan-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,evan-green-1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,evan-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,evan-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,evan-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,evan-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,evan-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,evan-orange))))
   `(magit-log-date      ((t (:foreground ,evan-fg-1))))
   `(magit-log-graph     ((t (:foreground ,evan-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,evan-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,evan-green))))
   `(magit-sequence-part ((t (:foreground ,evan-yellow))))
   `(magit-sequence-head ((t (:foreground ,evan-blue))))
   `(magit-sequence-drop ((t (:foreground ,evan-red))))
   `(magit-sequence-done ((t (:foreground ,evan-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,evan-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,evan-green))))
   `(magit-bisect-skip ((t (:foreground ,evan-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,evan-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,evan-bg-1 :foreground ,evan-blue-2))))
   `(magit-blame-hash    ((t (:background ,evan-bg-1 :foreground ,evan-blue-2))))
   `(magit-blame-name    ((t (:background ,evan-bg-1 :foreground ,evan-orange))))
   `(magit-blame-date    ((t (:background ,evan-bg-1 :foreground ,evan-orange))))
   `(magit-blame-summary ((t (:background ,evan-bg-1 :foreground ,evan-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,evan-bg+3))))
   `(magit-hash           ((t (:foreground ,evan-bg+3))))
   `(magit-tag            ((t (:foreground ,evan-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,evan-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,evan-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,evan-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,evan-blue   :weight bold))))
   `(magit-refname        ((t (:background ,evan-bg+2 :foreground ,evan-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,evan-bg+2 :foreground ,evan-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,evan-bg+2 :foreground ,evan-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,evan-green))))
   `(magit-signature-bad       ((t (:foreground ,evan-red))))
   `(magit-signature-untrusted ((t (:foreground ,evan-yellow))))
   `(magit-cherry-unmatched    ((t (:foreground ,evan-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,evan-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,evan-green))))
   `(magit-reflog-amend        ((t (:foreground ,evan-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,evan-green))))
   `(magit-reflog-checkout     ((t (:foreground ,evan-blue))))
   `(magit-reflog-reset        ((t (:foreground ,evan-red))))
   `(magit-reflog-rebase       ((t (:foreground ,evan-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,evan-green))))
   `(magit-reflog-remote       ((t (:foreground ,evan-cyan))))
   `(magit-reflog-other        ((t (:foreground ,evan-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,evan-green+1))))
   `(message-header-other ((t (:foreground ,evan-green))))
   `(message-header-to ((t (:foreground ,evan-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,evan-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,evan-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,evan-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,evan-green))))
   `(message-mml ((t (:foreground ,evan-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,evan-orange))))
   `(mew-face-header-from ((t (:foreground ,evan-yellow))))
   `(mew-face-header-date ((t (:foreground ,evan-green))))
   `(mew-face-header-to ((t (:foreground ,evan-red))))
   `(mew-face-header-key ((t (:foreground ,evan-green))))
   `(mew-face-header-private ((t (:foreground ,evan-green))))
   `(mew-face-header-important ((t (:foreground ,evan-blue))))
   `(mew-face-header-marginal ((t (:foreground ,evan-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,evan-red))))
   `(mew-face-header-xmew ((t (:foreground ,evan-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,evan-red))))
   `(mew-face-body-url ((t (:foreground ,evan-orange))))
   `(mew-face-body-comment ((t (:foreground ,evan-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,evan-green))))
   `(mew-face-body-cite2 ((t (:foreground ,evan-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,evan-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,evan-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,evan-red))))
   `(mew-face-mark-review ((t (:foreground ,evan-blue))))
   `(mew-face-mark-escape ((t (:foreground ,evan-green))))
   `(mew-face-mark-delete ((t (:foreground ,evan-red))))
   `(mew-face-mark-unlink ((t (:foreground ,evan-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,evan-green))))
   `(mew-face-mark-unread ((t (:foreground ,evan-red-2))))
   `(mew-face-eof-message ((t (:foreground ,evan-green))))
   `(mew-face-eof-part ((t (:foreground ,evan-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,evan-cyan :background ,evan-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,evan-bg :background ,evan-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,evan-bg :background ,evan-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,evan-blue))))
   `(mingus-pausing-face ((t (:foreground ,evan-magenta))))
   `(mingus-playing-face ((t (:foreground ,evan-cyan))))
   `(mingus-playlist-face ((t (:foreground ,evan-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,evan-magenta))))
   `(mingus-song-file-face ((t (:foreground ,evan-yellow))))
   `(mingus-artist-face ((t (:foreground ,evan-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,evan-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,evan-red+1))))
   `(mingus-stopped-face ((t (:foreground ,evan-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,evan-yellow))))
   `(nav-face-button-num ((t (:foreground ,evan-cyan))))
   `(nav-face-dir ((t (:foreground ,evan-green))))
   `(nav-face-hdir ((t (:foreground ,evan-red))))
   `(nav-face-file ((t (:foreground ,evan-fg))))
   `(nav-face-hfile ((t (:foreground ,evan-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,evan-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,evan-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,evan-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,evan-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,evan-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,evan-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,evan-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,evan-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,evan-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,evan-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,evan-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,evan-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,evan-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,evan-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,evan-fg))))
   `(neo-root-dir-face ((t (:foreground ,evan-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,evan-blue))))
   `(neo-file-link-face ((t (:foreground ,evan-fg))))
   `(neo-expand-btn-face ((t (:foreground ,evan-blue))))
   `(neo-vc-default-face ((t (:foreground ,evan-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,evan-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,evan-fg))))
   `(neo-vc-edited-face ((t (:foreground ,evan-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,evan-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,evan-red :background ,evan-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,evan-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,evan-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,evan-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,evan-fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,evan-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,evan-fg :weight bold))))
   `(org-checkbox ((t (:background ,evan-bg+2 :foreground ,evan-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,evan-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,evan-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,evan-green+3))))
   `(org-formula ((t (:foreground ,evan-yellow-2))))
   `(org-headline-done ((t (:foreground ,evan-green+3))))
   `(org-hide ((t (:foreground ,evan-bg-1))))
   `(org-level-1 ((t (:foreground ,evan-orange))))
   `(org-level-2 ((t (:foreground ,evan-green+4))))
   `(org-level-3 ((t (:foreground ,evan-blue-1))))
   `(org-level-4 ((t (:foreground ,evan-yellow-2))))
   `(org-level-5 ((t (:foreground ,evan-cyan))))
   `(org-level-6 ((t (:foreground ,evan-green+2))))
   `(org-level-7 ((t (:foreground ,evan-red-4))))
   `(org-level-8 ((t (:foreground ,evan-blue-4))))
   `(org-link ((t (:foreground ,evan-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,evan-green+4))))
   `(org-scheduled-previously ((t (:foreground ,evan-red))))
   `(org-scheduled-today ((t (:foreground ,evan-blue+1))))
   `(org-sexp-date ((t (:foreground ,evan-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,evan-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,evan-orange))))
   `(org-todo ((t (:weight bold :foreground ,evan-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,evan-red :weight bold :underline nil))))
   `(org-column ((t (:background ,evan-bg-1))))
   `(org-column-title ((t (:background ,evan-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,evan-fg :background ,evan-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,evan-bg :background ,evan-red-1))))
   `(org-ellipsis ((t (:foreground ,evan-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,evan-cyan :underline t))))
   `(org-document-title ((t (:foreground ,evan-blue))))
   `(org-document-info ((t (:foreground ,evan-blue))))
   `(org-habit-ready-face ((t :background ,evan-green)))
   `(org-habit-alert-face ((t :background ,evan-yellow-1 :foreground ,evan-bg)))
   `(org-habit-clear-face ((t :background ,evan-blue-3)))
   `(org-habit-overdue-face ((t :background ,evan-red-3)))
   `(org-habit-clear-future-face ((t :background ,evan-blue-4)))
   `(org-habit-ready-future-face ((t :background ,evan-green-1)))
   `(org-habit-alert-future-face ((t :background ,evan-yellow-2 :foreground ,evan-bg)))
   `(org-habit-overdue-future-face ((t :background ,evan-red-4)))
;;;;; outline
   `(outline-1 ((t (:foreground ,evan-orange))))
   `(outline-2 ((t (:foreground ,evan-green+4))))
   `(outline-3 ((t (:foreground ,evan-blue-1))))
   `(outline-4 ((t (:foreground ,evan-yellow-2))))
   `(outline-5 ((t (:foreground ,evan-cyan))))
   `(outline-6 ((t (:foreground ,evan-green+2))))
   `(outline-7 ((t (:foreground ,evan-red-4))))
   `(outline-8 ((t (:foreground ,evan-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,evan-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,evan-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,evan-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,evan-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,evan-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,evan-fg :background ,evan-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,evan-bg :background ,evan-orange))))
   `(proof-error-face ((t (:foreground ,evan-fg :background ,evan-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,evan-bg :background ,evan-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,evan-bg :background ,evan-orange))))
   `(proof-locked-face ((t (:background ,evan-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,evan-bg :background ,evan-orange))))
   `(proof-queue-face ((t (:background ,evan-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,evan-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,evan-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,evan-bg))))
   `(proof-warning-face ((t (:foreground ,evan-bg :background ,evan-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,evan-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,evan-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,evan-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,evan-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,evan-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,evan-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,evan-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,evan-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,evan-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,evan-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,evan-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,evan-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,evan-blue))))
   `(rcirc-other-nick ((t (:foreground ,evan-orange))))
   `(rcirc-bright-nick ((t (:foreground ,evan-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,evan-blue-2))))
   `(rcirc-server ((t (:foreground ,evan-green))))
   `(rcirc-server-prefix ((t (:foreground ,evan-green+1))))
   `(rcirc-timestamp ((t (:foreground ,evan-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,evan-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,evan-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,evan-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,evan-bg :background ,evan-magenta))))
   `(reb-match-1 ((t (:foreground ,evan-bg :background ,evan-blue))))
   `(reb-match-2 ((t (:foreground ,evan-bg :background ,evan-orange))))
   `(reb-match-3 ((t (:foreground ,evan-bg :background ,evan-red))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,evan-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,evan-green))))
   `(rpm-spec-doc-face ((t (:foreground ,evan-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,evan-red))))
   `(rpm-spec-macro-face ((t (:foreground ,evan-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,evan-red))))
   `(rpm-spec-package-face ((t (:foreground ,evan-red))))
   `(rpm-spec-section-face ((t (:foreground ,evan-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,evan-blue))))
   `(rpm-spec-var-face ((t (:foreground ,evan-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,evan-orange))))
   `(rst-level-2-face ((t (:foreground ,evan-green+1))))
   `(rst-level-3-face ((t (:foreground ,evan-blue-1))))
   `(rst-level-4-face ((t (:foreground ,evan-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,evan-cyan))))
   `(rst-level-6-face ((t (:foreground ,evan-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,evan-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,evan-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,evan-red+1 :background ,evan-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,evan-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Evan for sml
   `(sml/global ((,class (:foreground ,evan-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,evan-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,evan-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,evan-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,evan-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,evan-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,evan-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,evan-orange))))
   `(sml/git ((,class (:foreground ,evan-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,evan-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,evan-red-2))))
   `(sml/outside-modified ((,class (:foreground ,evan-orange))))
   `(sml/modified ((,class (:foreground ,evan-red))))
   `(sml/vc-edited ((,class (:foreground ,evan-green+2))))
   `(sml/charging ((,class (:foreground ,evan-green+4))))
   `(sml/discharging ((,class (:foreground ,evan-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,evan-red+1 :background ,evan-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,evan-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,evan-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,evan-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-red)))
      (t
       (:underline ,evan-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-orange)))
      (t
       (:underline ,evan-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-yellow)))
      (t
       (:underline ,evan-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,evan-green)))
      (t
       (:underline ,evan-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,evan-green+2))))
   `(speedbar-directory-face ((t (:foreground ,evan-cyan))))
   `(speedbar-file-face ((t (:foreground ,evan-fg))))
   `(speedbar-highlight-face ((t (:foreground ,evan-bg :background ,evan-green+2))))
   `(speedbar-selected-face ((t (:foreground ,evan-red))))
   `(speedbar-separator-face ((t (:foreground ,evan-bg :background ,evan-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,evan-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,evan-fg
                                    :background ,evan-bg))))
   `(tabbar-selected ((t (:foreground ,evan-fg
                                      :background ,evan-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,evan-fg
                                        :background ,evan-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,evan-bg
                                       :background ,evan-bg-1))))
   `(term-color-red ((t (:foreground ,evan-red-2
                                     :background ,evan-red-4))))
   `(term-color-green ((t (:foreground ,evan-green
                                       :background ,evan-green+2))))
   `(term-color-yellow ((t (:foreground ,evan-orange
                                        :background ,evan-yellow))))
   `(term-color-blue ((t (:foreground ,evan-blue-1
                                      :background ,evan-blue-4))))
   `(term-color-magenta ((t (:foreground ,evan-magenta
                                         :background ,evan-red))))
   `(term-color-cyan ((t (:foreground ,evan-cyan
                                      :background ,evan-blue))))
   `(term-color-white ((t (:foreground ,evan-fg
                                       :background ,evan-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,evan-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,evan-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,evan-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,evan-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,evan-cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,evan-bg :background ,evan-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,evan-bg :background ,evan-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,evan-bg :background ,evan-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,evan-yellow-2 :background ,evan-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,evan-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,evan-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,evan-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,evan-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,evan-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,evan-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,evan-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,evan-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,evan-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,evan-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,evan-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,evan-bg+1 :foreground ,evan-bg+1))))
   `(whitespace-hspace ((t (:background ,evan-bg+1 :foreground ,evan-bg+1))))
   `(whitespace-tab ((t (:background ,evan-red-1))))
   `(whitespace-newline ((t (:foreground ,evan-bg+1))))
   `(whitespace-trailing ((t (:background ,evan-red))))
   `(whitespace-line ((t (:background ,evan-bg :foreground ,evan-magenta))))
   `(whitespace-space-before-tab ((t (:background ,evan-orange :foreground ,evan-orange))))
   `(whitespace-indentation ((t (:background ,evan-yellow :foreground ,evan-red))))
   `(whitespace-empty ((t (:background ,evan-yellow))))
   `(whitespace-space-after-tab ((t (:background ,evan-yellow :foreground ,evan-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,evan-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,evan-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,evan-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,evan-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,evan-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,evan-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,evan-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,evan-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,evan-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,evan-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,evan-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,evan-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,evan-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,evan-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,evan-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,evan-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,evan-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,evan-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,evan-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,evan-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,evan-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,evan-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,evan-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,evan-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,evan-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,evan-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,evan-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,evan-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,evan-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,evan-bg :background ,evan-blue+1))))
   `(cscope-separator-face ((t (:foreground ,evan-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,evan-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,evan-bg-1 :foreground ,evan-bg-1))))
   ))

;;; Theme Variables
(evan-with-color-variables
  (custom-theme-set-variables
   'evan
;;;;; ansi-color
   `(ansi-color-names-vector [,evan-bg ,evan-red ,evan-green ,evan-yellow
                                          ,evan-blue ,evan-magenta ,evan-cyan ,evan-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,evan-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,evan-red ,evan-orange ,evan-yellow ,evan-green ,evan-green+4
                    ,evan-cyan ,evan-blue+1 ,evan-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,evan-fg . ,evan-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,evan-red-1)
       ( 40. . ,evan-red)
       ( 60. . ,evan-orange)
       ( 80. . ,evan-yellow-2)
       (100. . ,evan-yellow-1)
       (120. . ,evan-yellow)
       (140. . ,evan-green-1)
       (160. . ,evan-green)
       (180. . ,evan-green+1)
       (200. . ,evan-green+2)
       (220. . ,evan-green+3)
       (240. . ,evan-green+4)
       (260. . ,evan-cyan)
       (280. . ,evan-blue-2)
       (300. . ,evan-blue-1)
       (320. . ,evan-blue)
       (340. . ,evan-blue+1)
       (360. . ,evan-magenta)))
   `(vc-annotate-very-old-color ,evan-magenta)
   `(vc-annotate-background ,evan-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar evan-add-font-lock-keywords nil
  "Whether to add font-lock keywords for evan color names.
In buffers visiting library `evan-theme.el' the evan
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar evan-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after evan activate)
;;   "Maybe also add font-lock keywords for evan colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or evan-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "evan-theme.el")))
;;     (unless evan-colors-font-lock-keywords
;;       (setq evan-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car evan-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc evan-colors-alist))))))
;;     (font-lock-add-keywords nil evan-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after evan activate)
;;   "Also remove font-lock keywords for evan colors."
;;   (font-lock-remove-keywords nil evan-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'evan)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; evan-theme.el ends here
