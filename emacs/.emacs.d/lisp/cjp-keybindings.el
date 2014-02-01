;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom Keybindings
;;;; ==================
;;;;
;;;; General, Mac, and Linux keybindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom Binding Prefix
;;; ---------------------
;;;
;;; Use `C-c c` as a prefix for more complicated stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapc (lambda (arg)
        (global-set-key (kbd (concat "C-c SPC " (car arg))) (cadr arg)))
      '(("l" find-library)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Highlight symbol
;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-prev)))
;; (global-set-key [(control meta f3)] 'highlight-symbol-query-replace)

;;; Org mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;;; Visual regexp
(global-set-key (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "C-c q") 'vr/query-replace)

;;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;;; Multiple cursors
(global-set-key (kbd "C-M-?") 'mc/edit-lines)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M->") 'mc/mark-all-like-this)

;;; Undo-tree
(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(global-set-key (kbd "C-M-/") 'undo-tree-visualize)

;;; iedit
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)
(define-key iedit-mode-keymap (kbd "M-n") 'iedit-next-occurrence)
(define-key iedit-mode-keymap (kbd "M-p") 'iedit-prev-occurrence)

(global-set-key (kbd "C-M-g") 'magit-status)

;;; Swap these round from usual; I find it more logical
(global-set-key (kbd "C-x +") 'what-cursor-position)
(global-set-key (kbd "C-x =") 'balance-windows)

;;; Sets current frame or window to width of 80 characters
(global-set-key (kbd "C-x W") 'fix-horizontal-size)

(global-set-key (kbd "C-c s") 'dictionary-lookup-definition)
(global-set-key (kbd "C-c S") 'dictionary-search)
(global-set-key (kbd "C-c m") 'dictionary-match-words)

;;; Rebind keys, including backups to old command. M-x doesn't work with same
;;; keys on all systems, so bind to C-x X-m too (And C-c for good measure, in
;;; case your finger slips)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x m") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)

(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x M") 'smex)
(global-set-key (kbd "C-x C-M") 'smex)
(global-set-key (kbd "C-c C-M") 'smex)

(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c C-c C-x C-m") 'execute-extended-command)

;;; Default keybindings of C-c <left> and C-c <right> are annoying to type if
;;; going back more than one or two window configurations. Use super key for
;;; higher level Emacs command like these to avoid interfering with modes.
(global-set-key (kbd "C-s-<left>") 'winner-undo)
(global-set-key (kbd "C-s-<right>") 'winner-redo)

(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>")    'windmove-up)
(global-set-key (kbd "s-<down>")  'windmove-down)

;;; Easier bindings than shift-left etc. (nearer home row). "C-x u" binding had
;;; to be undefined before it could be made to run windmove-left.
(define-key undo-tree-map (kbd "C-x u") nil)
(global-set-key (kbd "C-x o") 'other-window)

;;; Get rid of `find-file-read-only' and replace it with something more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;;; I like M-g for goto-line
(global-set-key (kbd "M-g") 'goto-line)

;;; Spell word at point (Usually M-$)
(global-set-key (kbd "M-s") 'ispell-word)
(global-set-key (kbd "M-S") 'dictionary-lookup-definition)

;;; I often use dired mode; this prevents mistakes if quickly typing usual
;;; shortcut of C-x d
(global-set-key (kbd "C-x C-d") 'ido-dired)

;;; I often hit C-x s by mistaken when I want C-x C-s, so bind it to the same
;;; command
(global-set-key (kbd "C-x s") 'save-buffer)

;;; Backward kill word is used so often that this is useful, but kill-region is
;;; still needed, so move to C-x C-k
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
;;; (global-set-key (kbd "C-#") 'kill-region)

;;; Since re-mapped C-w to backward-kill-word, and moved cut option to C-\,
;;; should move copy command too (M-w -> M-\). Re-map old M-\ -> M-w
;;; (global-set-key (kbd "M-w") 'delete-horizontal-space)
;;; (global-set-key (kbd "M-\\") 'kill-ring-save)
;;; (global-set-key (kbd "M-#") 'kill-ring-save)

;;; M-/ is used often to expand words as a basic tab completion, so map command
;;; to somewhere easier to press quickly, M-o (previously undefined), or M-i if
;;; I miss 'o' with my fingers.  M-/ also now bound to undo-tree-redo.
(global-set-key (kbd "M-o") 'dabbrev-expand)
(global-set-key (kbd "M-i") 'dabbrev-expand)

(global-set-key (kbd "C-'") 'other-window)
(global-set-key (kbd "C-\"") 'other-window)
(global-set-key (kbd "C-x B") 'ido-switch-buffer-other-window)

;;; C-h h is usually view-hello-file. Forget it, and use handy C-h C-h to lookup
;;; stuff in info docs.
(global-set-key (kbd "C-h h") 'help-for-help)
(global-set-key (kbd "C-h C-h") 'cjp-lookup-thing-at-point)
(global-set-key (kbd "C-h C-u") 'cjp-load-url-w3m)

;;; Open the current buffer with privileges given by `sudo'
(global-set-key (kbd "C-x C-V") 'find-alternative-file-with-sudo)

;;; Type ( and have Emacs add the ) and put point in middle
;;; (setq skeleton-pair t)
;;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)

;;; Unbind keys
(global-unset-key (kbd "C-z")) ; Usually suspend-frame. Annoying.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when macosxp
  ;; Toggle fullscreen as usual
  ;; (global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)
  
  ;; Set cmd-H to hide Emacs and cmd-shift-h to hide others, as usual in Mac OS
  ;; X. Usually bound to mark-paragraph
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-H") 'ns-do-hide-others)
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

  ;; Easily open files with 'open' and show directories in Finder
  (define-key dired-mode-map (kbd "o") 'cjp-mac-guess-open-file)
  (define-key dired-mode-map (kbd "C-M-f") 'cjp-mac-show-finder)
  (define-key dired-mode-map (kbd "e") 'cjp-mac-textedit-file)
  (define-key dired-mode-map (kbd "q") 'cjp-mac-quicklook-file)
  (define-key dired-mode-map (kbd "C-M-t") 'cjp-mac-open-terminal)

  ;; alt key on Mac is Super, but it also lets you type foreign accents and
  ;; other useful characters. Use self insert for these purposes.
  (global-set-key (kbd "s-3") '(lambda () (interactive) (insert "#"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Linux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when linuxp
  ;; Outline-minor-mode key map
  (define-prefix-command 'cm-map nil "Outline-")

  (define-key cm-map "q" 'hide-sublevels) ; Hide everything but the top-level headings
  (define-key cm-map "t" 'hide-body)      ; Hide everything but headings (all body lines)
  (define-key cm-map "o" 'hide-other)     ; Hide other branches
  (define-key cm-map "c" 'hide-entry)     ; Hide this entry's body
  (define-key cm-map "l" 'hide-leaves)    ; Hide body lines in this entry and sub-entries
  (define-key cm-map "d" 'hide-subtree)   ; Hide everything in this entry and sub-entries

  (define-key cm-map "a" 'show-all)       ; Show (expand) everything
  (define-key cm-map "e" 'show-entry)     ; Show this heading's body
  (define-key cm-map "i" 'show-children)  ; Show this heading's immediate child sub-headings
  (define-key cm-map "k" 'show-branches)  ; Show all sub-headings under this heading
  (define-key cm-map "s" 'show-subtree)   ; Show (expand) everything in this heading & below

  (define-key cm-map "u" 'outline-up-heading)               ; Up
  (define-key cm-map "n" 'outline-next-visible-heading)     ; Next
  (define-key cm-map "p" 'outline-previous-visible-heading) ; Previous
  (define-key cm-map "f" 'outline-forward-same-level)       ; Forward - same level
  (define-key cm-map "b" 'outline-backward-same-level)      ; Backward - same level
  (global-set-key (kbd "C-c C-c") cm-map)

  ;; Since C-s-f toggles fullscreen on the Mac, maxmimise Emacs' frame on Linux
  ;; with the same binding.
  (global-set-key (kbd "C-s-f") 'toggle-frame-maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; F1
(global-set-key (kbd "<f1>") 'ace-jump-char-mode)
(global-set-key (kbd "<M-f1>") 'ace-jump-word-mode)
(global-set-key (kbd "<C-f1>") 'ace-jump-line-mode)

;;; F2
(global-set-key (kbd "<f2>") 'fix-horizontal-size-to-buffer)
(global-set-key (kbd "<C-f2>") 'cjp-window-setup-toggle)

;;; F3 and F4 reserved for macros

;;; F5
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<C-f5>") 'recompile)

;;; F6
(global-set-key (kbd "<f6>") 'bookmark-jump)
(global-set-key (kbd "<C-f6>") 'bookmark-bmenu-list)

;;; F7
(when macosxp
  (global-set-key (kbd "<f7>") 'cjp-mac-show-finder)
  (global-set-key (kbd "<C-f7>") 'cjp-mac-open-terminal)
  (global-set-key (kbd "<C-M-f7>") (lambda () (interactive)
                                     (cjp-mac-open-terminal t))))

;;; F8
(global-set-key (kbd "<f8>") 'cjp-ispell-guess-usage)
(global-set-key (kbd "<C-f8>") 'dictionary-search)
(global-set-key (kbd "<M-f8>") 'dictionary-match-words)

;;; F9 reserved for Exposé (all windows)

;;; F10
(global-set-key (kbd "<f10>") 'epa-sign-region)
(global-set-key (kbd "<C-f10>") 'epa-encrypt-region)
(global-set-key (kbd "<M-f10>") 'epa-decrypt-region)

;;; F11
(global-set-key (kbd "<f11>") 'cjp-browse-url-on-line)

;;; F12
(global-set-key (kbd "<f12>") 'gtd-inbox)
(global-set-key (kbd "<C-f12>") 'gtd-select)
(global-set-key (kbd "<M-f12>") 'gtd-action-list)
(global-set-key (kbd "<C-M-f12>") 'gtd-functions)
(when linuxp
  (global-set-key (kbd "<C-s-f12>") 'gtd-functions))

;;; F13-16 (Apple extended keyboard only)
(when macosxp
  (global-set-key (kbd "<f13>") 'cjp-mac-guess-open-file)
  (global-set-key (kbd "<f14>") 'cjp-browse-url-on-line)
  (global-set-key (kbd "<C-f14>") 'cjp-browse-buffer)
  (global-set-key (kbd "<f15>") 'cjp-find-with-google)
  (global-set-key (kbd "<f16>") (lambda () (interactive) (cjp-set-frame-uni t)))
  (global-set-key (kbd "<C-f16>") (lambda () (interactive) (cjp-set-frame-uni))))
