;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Custom stuff for GNU/Linux systems
;;;;; ==================================
;;;;;
;;;;; Version: 20110505
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'color-theme)
(require 'color-theme-tangotango)
;; (require 'zenburn)

(setq color-theme-load-all-themes nil)

;;; Set font (Menlo default)
(if (display-graphic-p)
    (cjp-set-font-size "10" "Monospace"))

(if (display-graphic-p)
    (color-theme-tangotango)
  (color-theme-initialize)
  (color-theme-hober))

;;; Move scrollbars to right side of frames
(menu-bar-right-scroll-bar)

;;; Make Super another Meta key (useful for Mac keyboard)
(setq x-super-keysym 'super)

;;; Remove menubar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; Change binding for outline minor mode
(add-hook 'outline-minor-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c")
                               outline-mode-prefix-map)))


;;; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")

(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries

(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below

(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key (kbd "C-c C-c") cm-map)

(add-hook 'java-mode-hook 'outline-minor-mode)
