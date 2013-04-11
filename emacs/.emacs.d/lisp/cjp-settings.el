;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Generic Emacs Configuration
;;;; ===========================
;;;;
;;;; System-agnostic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Requirements and libraries
;;; --------------------------
;;;
;;; These don't require their own section with other commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smallurl)
(require 'smooth-scrolling)
(require 'htmlize)
(require 'pydoc-info)
(require 'cl)

(load-library "regex-tool")
(load-library "smooth-scrolling")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unicode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp slime nav
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'elisp-slime-nav)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow delimeters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Regexp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight Symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-symbol)
;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-prev)))
;; (global-set-key [(control meta f3)] 'highlight-symbol-query-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'powerline)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CPerl mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; javadoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'javadoc-help)

;; (add-hook 'java-mode-hook (lambda ()
;;                            (local-set-key (kbd "C-h C-h") 'javadoc-lookup)
;;                            (local-set-key (kbd "C-S-h C-S-h") 'javadoc-help)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'magit)
(setq magit-omit-untracked-dir-contents t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iedit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'iedit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ace-jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ace-jump-mode)
(setq ace-jump-mode-case-sensitive-search nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete-config)

(setq ac-comphist-file (cjp-emacs-structure-dir ".ac-comphist.dat")
      ac-fuzzy-enable t)

(add-to-list 'ac-dictionary-directories
             (cjp-emacs-structure-dir "auto-complete/dict" "lisp"))
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IELM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start ielm with AC, ElDoc, and Paredit. Make it inherit local variables from
;; the buffer it was invoked from.
(defvar ielm-invoked-from-buffer nil)

(add-hook 'ielm-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-functions
                               ac-source-variables
                               ac-source-features
                               ac-source-symbols
                               ac-source-words-in-same-mode-buffers))
            (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
            (auto-complete-mode 1)
            (eldoc-mode 1)
            (paredit-mode 1)
            (ielm-change-working-buffer ielm-invoked-from-buffer)))

(defadvice ielm (before change-working-buffer activate)
  (setq ielm-invoked-from-buffer (current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'w3m-load)

(setq browse-url-browser-function 'w3m-browse-url
      w3m-default-save-directory "~/Documents/inbox"
      w3m-use-tab nil
      w3m-use-tab-menubar nil
      w3m-key-binding "info")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bookmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bookmark+)

;; Choose a location of bookmarks file.  Save bookmarks file every time I put a
;; new bookmark in the file (not just when Emacs quits)
(setq bookmark-default-file (cjp-emacs-structure-dir "bookmarks")
      bookmark-save-flag 1
      bmkp-bmenu-state-file (cjp-emacs-structure-dir ".emacs-bmk-bmenu-state.el")
      bmkp-bmenu-commands-file
      (cjp-emacs-structure-dir ".emacs-bmk-bmenu-commands.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DocView
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'doc-view-mode-hook (lambda () (auto-revert-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing from Google Chrome
;;; --------------------------
;;;
;;; Chrome extension `Edit with Emacs` supplies edit-server.el, which has to be
;;; loaded for Emacs to get the content from Chrome.
;;;
;;; Further details: http://www.emacswiki.org/emacs/Edit_with_Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (display-graphic-p)
  (require 'edit-server)
  (setq edit-server-new-frame nil)
  (edit-server-start)

  ;; Use markdown mode, but still use C-c C-c to send back to Chrome
  (add-hook 'edit-server-start-hook
            (lambda ()
              (markdown-mode)
              (local-set-key (kbd "C-c C-c") 'edit-server-done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pretty Lambda
;;; -------------
;;;
;;; Turn 'lambda' into the Greek letter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pretty-lambdada)

;; (setq cjp-lispy-modes '(lisp-mode-hook paredit-mode-hook))

;; (mapc (lambda (x) (add-hook x 'pretty-lambda)) cjp-lispy-modes)

(add-hook 'lisp-interaction-mode-hook 'pretty-lambda)
(add-hook 'emacs-lisp-mode-hook 'pretty-lambda)
(add-hook 'lisp-mode-hook 'pretty-lambda)
(add-hook 'slime-mode-hook 'pretty-lambda)
(add-hook 'slime-mode-hook 'pretty-lambda)
(add-hook 'slime-repl-mode-hook 'pretty-lambda)
(add-hook 'scheme-mode-hook 'pretty-lambda)
(add-hook 'inferior-scheme-mode-hook 'pretty-lambda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show error messages in minibuffer, not as a GUI menu
(load-library "flymake-cursor")

;; Use pyflakes with flymake
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)

(setq dictionary-default-strategy "re")
(setq dictionary-use-single-buffer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hide and show details (`ls -l` stuff) with '(' and ')'
(require 'dired-details+)
(require 'dired+)
(setq dired-details-hidden-string ""
      dired-details-initially-hide nil
      dired-omit-files (concat dired-omit-files "\\|^\\..+$")) ; dired-omit-mode,
                                                               ; ignore dotfiles
(setq-default dired-omit-mode nil) ; this is buffer-local variable

;; Make return key open files in another window, except if item at point is a
;; directory, and then open in the current window.
(define-key dired-mode-map (kbd "RET") (lambda ()
                                          (interactive)
                                          (if (cjp-dired-directoryp)
                                              (dired-find-file)
                                            (dired-find-file-other-window))))

(defadvice dired-details-toggle (after fit-dired-frame activate)
  "Resize dired buffer (horizontally) after toggling details."
  (fix-horizontal-size-to-buffer))      ; In cjp-library

;; Don't show '..' since '^' does this; show human file sizes
(setq dired-listing-switches "-Alh")

;; The default fonts don't look nice with Tango theme, at least to my eyes
(setq diredp-compressed-file-suffix '((background dark)
                                      (:foreground "Red"))
      diredp-rare-priv '((background dark)
                         (:background "#FFFF00008080" :foreground "White")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CUA
;;; ---
;;;
;;; Turn on for rectangle mode only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq cua-enable-cua-keys nil)
(cua-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smex)

;; Start smex, saving into Emacs structure
(setq smex-save-file (cjp-emacs-structure-dir ".smex-items"))
;; Smex updates its list of possible commands when run; don't let it
(setq smex-auto-update t)
;; Update smex when Emacs has been idle for (default 60) seconds
(smex-auto-update)
(smex-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify)

;; Instead of <2> etc. after buffer name when opening multiple files with the
;; same name, Change it to "name" : "directory name"
(setq uniquify-buffer-name-style 'forward
      uniquify-separator ":")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Winner / Windmove
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on winner mode to move back and forwards between window configurations
;; with C-c left and C-c right respectively
(winner-mode 1)

;; Move to other windows with shift-(left|right|up|down)
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use sbcl
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; (require 'slime)
;; (slime-setup)
;; (slime-setup '(slime-fancy))

;; auto-complete for slime
;; (require 'ac-slime)
;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'slime-repl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recent files
;;; ------------
;;;
;;; From http://www.masteringemacs.org/articles/2011/01/27/
;;; find-files-faster-recent-files-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)

;; Tramp mode messes this up, causing Emacs to IO block for a short time. (From
;; http://www.emacswiki.org/emacs/RecentFiles)
(setq recentf-auto-cleanup 'never)

;; 50 files ought to be enough.
(setq recentf-save-file (cjp-emacs-structure-dir ".recentf") ; default is ~/.recentf
      recentf-max-saved-items 100
      recentf-exclude '("\.recentf" "\.ido\.last" "\.aux" "~$" ".*Dropbox/gtd.*"))

;; Enable recent files mode.
(recentf-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet)
(yas-global-mode 1)

;; Store my personal snippets in ~/emacs/snippets, still load the stock ones
(setq yas/root-directory
      (append (cjp-emacs-structure-dir-map '("personal" "contributed") "snippets")
              (list (cjp-emacs-structure-dir "yasnippet/snippets" "lisp"))))

;; Load snippets from all directories
(mapc 'yas/load-directory yas/root-directory)

;; If there are multiple snippets to choose from, use ido by default in
;; minibuffer.
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/x-prompt
                             yas/completing-prompt
                             yas/no-prompt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ElDoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'c-eldoc)

(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
(add-hook 'inferior-python-mode 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'scheme-mode-hook 'turn-on-eldoc-mode)
(add-hook 'inferior-scheme-mode-hook 'turn-on-eldoc-mode)
(add-hook 'geiser-repl-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'slime-mode-hook 'turn-on-eldoc-mode)
(add-hook 'slime-repl-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(setq c-eldoc-includes "-I./ -I../ -I/usr/include/ -I/usr/local/include/ ")

;; Make ElDoc aware of ParEdit's most used commands (ElDoc will automatically
;; refresh the minibuffer)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq eshell-directory-name (cjp-emacs-structure-dir ".eshell")
      eshell-scroll-to-bottom-on-input t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markdown
;;; --------
;;;
;;; Using kramdown from ruby gem.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)

(setq markdown-command "kramdown"
      markdown-italic-underscore t ; Use underscores for italics
      markdown-indent-on-enter nil
      markdown-enable-math t) ; Enable syntax highlighting (LaTeX)

;; Webgen uses markdown syntax in .page files
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mark\\'" . markdown-mode))

;; Rebind tab key as yas/expand
(add-hook 'markdown-mode-hook (lambda ()
                                 (local-set-key (kbd "<tab>") 'yas/expand)
                                 (outline-minor-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python and related modes
;;; ------------------------
;;;
;;; Using python.el, not python-mode.el. The latter doesn't seem to be able to
;;; send the contents of a buffer to the interpreter easily, as python.el can
;;;  (with C-c C-c).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ac-python)

;; Use python-mode with files with these extensions
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))

;; Turn on auto-complete in python shells
(add-hook 'inferior-python-mode-hook (lambda () (auto-complete-mode 1)))

;; Use python major mode if 'python' is in hashbang.
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Use python as the python interpreter (can be changed to "ipython" in time
;; when it works)
(setq python-python-command "python")

;; Check files for pep8 mistakes
(autoload 'python-pep8 "python-pep8")
(autoload 'pep8 "python-pep8")

;; displays "\" at the end of lines that wrap
(setq longlines-show-hard-newlines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pymacs & Ropemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ropemacs-enable-shortcuts nil)
(setq ropemacs-local-prefix nil)
(setq ropemacs-enable-autoimport nil)
(setq ropemacs-confirm-saving t)
(setq ropemacs-global-prefix "C-c r")

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacx1s-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(defun load-ropemacs ()
  (interactive)
  (pymacs-load "ropemacs" "rope-")
  (ac-ropemacs-initialize)
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'ac-sources 'ac-source-ropemacs)))
  (if (eq major-mode 'python-mode)
      (ropemacs-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq Info-default-directory-list
      (append (cjp-get-dir-structure-in "info")
              Info-default-directory-list))

(setq Info-directory-list Info-default-directory-list)

(info-lookup-add-help
    :mode 'lisp-mode
    :regexp "[^][()'\" \t\n]+"
    :ignore-case t
    :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(info-lookup-maybe-add-help
 :mode 'emacs-lisp-mode
 :regexp "[^][()`',\" \t\n]+"
 :doc-spec '(("(elisp)Index"          nil "^ -+ .*: " "\\( \\|$\\)")
             ;; Commands with key sequences appear in nodes as `foo' and
             ;; those without as `M-x foo'.
             ("(emacs)Command Index"  nil "`\\(M-x[ \t\n]+\\)?" "'")
             ;; Variables normally appear in nodes as just `foo'.
             ("(emacs)Variable Index" nil "`" "'")
             ;; Almost all functions, variables, etc appear in nodes as
             ;; " -- Function: foo" etc.  A small number of aliases and
             ;; symbols appear only as `foo', and will miss out on exact
             ;; positions.  Allowing `foo' would hit too many false matches
             ;; for things that should go to Function: etc, and those latter
             ;; are much more important.  Perhaps this could change if some
             ;; sort of fallback match scheme existed.
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq cjp-scheme-program "racket")

(setq scheme-program-name cjp-scheme-program)

;; Geiser is a minor mode built on scheme-mode, supporting racket (PLT-Scheme)
;; and guile. (See info doc.)
;; To install geiser, extract its tarball to geiser directory, then:
;; mkdir build && cd build && ../configure && make all
;(setq load-path (append (list (cjp-emacs-structure-dir "geiser/build/elisp"
;                                                       "lisp"))
;                        load-path))
;(require 'geiser-install)
(setq geiser-active-implementations '(racket)
      geiser-repl-history-filename (cjp-emacs-structure-dir ".geiser-history")
      geiser-repl-autodoc-p nil
      geiser-mode-autodoc-p nil)

;; Shamelessly stolen from info-look.el, scheme-mode
(info-lookup-maybe-add-help
 :mode 'geiser-repl-mode
 :regexp "[^()`',\" \t\n]+"
 :ignore-case t
 ;; Aubrey Jaffer's rendition from <URL:ftp://ftp-swiss.ai.mit.edu/pub/scm>
 :doc-spec '(("(r5rs)Index" nil
	      "^[ \t]+-+ [^:]+:[ \t]*" "\\b")))

;; Quack
(require 'quack)
(setq quack-default-program cjp-scheme-program
      quack-run-scheme-always-prompts-p nil)

;; http://synthcode.com/wiki/scheme-complete
(autoload 'scheme-smart-complete "scheme-complete" nil t)

(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
  (lambda ()
    (make-local-variable 'eldoc-documentation-function)
    (setq eldoc-documentation-function 'scheme-get-current-symbol-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paredit
;;; -------
;;;
;;; Taken from http://www.emacswiki.org/emacs/ParEdit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'geiser-repl-mode-hook      (lambda () (paredit-mode +1)))
(add-hook 'inferior-scheme-mode-hook  (lambda () (paredit-mode +1)))
(add-hook 'slime-mode-hook            (lambda () (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook       (lambda () (paredit-mode +1)))

;; Use C-w to backwards kill words, consistent with global custom
;; settings. Also undefine C-left and C-right, to use these with winner mode.
(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key (kbd "C-w") 'paredit-backward-kill-word)
            (define-key paredit-mode-map (kbd "C-<left>") nil)
            (define-key paredit-mode-map (kbd "C-<right>") nil)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; js2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use SSH in TRAMP by default
(setq tramp-default-method "ssh")

;; Don't make backup files when using TRAMP
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; Store information here (not default ~/.emacs.d/tramp)
(setq tramp-persistency-file-name (cjp-emacs-structure-dir ".tramp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add current directory to mode line of shell windows
;; (defun add-mode-line-dirtrack ()
;;  (add-to-list 'mode-line-buffer-identification
;;               '(:propertize (" " default-directory " ") face dired-directory)))
;; (add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;; Make sure passwords not echoed in shell
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key comint-mode-map (kbd "<down>") 'comint-next-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calendar and Diary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq diary-file (cjp-emacs-structure-dir "diary") ; Choose my custom diary file
      calendar-week-start-day 1 ; Start Calendar on Monday
      european-calendar-style 't) ; European date format (DD/MM/YYYY)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abbrev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default abbrev-mode t) ; Operate on startup, save in specified file
(setq abbrev-file-name (cjp-emacs-structure-dir ".abbrev_defs")
      save-abbrevs t ; Save abbrevs when files are saved
      dabbrev-abbrev-char-regexp "\\sw\\|\\s_") ; Recognise understores too

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-install)

;; Org-mode setup
(setq org-agenda-ndays 7                  ; Show next 7 days in agenda
      org-deadline-warning-days 14        ; Show upcoming events 14 days prior
      org-agenda-show-all-dates t         ; Show dates even if totally free
      org-agenda-skip-deadline-if-done t  ; Don't show things already done
      org-agenda-skip-scheduled-if-done t ; Don't show things already done
      org-agenda-start-on-weekday nil     ; Always start with today
      org-return-follows-link t
      org-agenda-files (cjp-emacs-structure-dir-map '("gtd.org"))
      org-default-notes-file (cjp-emacs-structure-dir "notes.org" "org")
      org-directory (cjp-emacs-structure-dir "org"))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key outline-minor-mode-map (kbd "<C-tab>") 'org-cycle)
            (define-key outline-minor-mode-map (kbd "<S-tab>") 'org-global-cycle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUCTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'reftex)

;; Load AUCTeX
(load "auctex.el" nil t t)

;; These allow AUCTeX to parse TeX files automatically. Creates 'auto'
;; directory with parse info for each TeX file, got annoying so disabled for
;; now
;; (setq TeX-auto-save t)

(setq TeX-parse-self t
      TeX-PDF-mode t ; Use pdflatex as default mode in AuCTEX, always
      ;; TeX-electric-sub-and-superscript nil
      )

;; Enable math mode and auto-fill when typing LaTeX, and RefTeX
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-source-correlate-mode 1)))

;; Use tex parser so that TeX commands aren't checked
(add-hook 'LaTeX-mode-hook (lambda () (setq ispell-parser 'tex)))

(setq TeX-source-correlate-method 'synctex)

(setq reftex-plug-into-AUCTeX t ; Setup RefTeX with AUCTeX automatically
      reftex-section-prefixes '((0 . "part-") ; Use `-', not `:'.
                                (1 . "cha-")
                                (t . "sec-"))
      reftex-cite-format "\\citet[][]{%l}" ; Change citation format to natbib
                                           ; \citet format
      )

;; Highlight keywords from the natbib package
(setq font-latex-match-reference-keywords
      '(("citet" "[{")))

;; Have AUCTeX ask which is master file for multi-document TeX
(setq-default TeX-master nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)

(setq ido-save-directory-list-file (cjp-emacs-structure-dir ".ido.last"))
(ido-mode t)
(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always) ; If a buffer name that doesn't exist is
                                     ; chosen, just make a new one without
                                     ; prompting

;; Ignore the .aux extensions that TeX programs create
(setq completion-ignored-extensions
      (cons "*.aux" completion-ignored-extensions))

;; Order extensions by how I use them
(setq ido-file-extensions-order '(".tex" ".txt" ".md" ".py" ".sh" ".el" ".xml" ".htm"))

;; Ignore files defined in variable completion-ignored-extensions
(setq ido-ignore-extensions t)

;; Default keybinding is backspace key, but I use C-w in the non-Ido-mode
;; minibuffers often, so this is more conventient for muscle memory
(define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)

;; Stops Ido searching for similar-named files if I use C-x C-s to create a new
;; file and buffer
(setq ido-auto-merge-work-directories-length -1)

;; Keep annoying buffers out of my face
(setq ido-ignore-buffers (list (rx (or (and bos  " ")
                                       (and bos
                                            (or "*Completions*"
                                                "*Shell Command Output*"
                                                "*vc-diff*")
                                            eos)))))

;; Allow spaces when using ido-find-file
(add-hook 'ido-make-file-list-hook
          (lambda ()
            (define-key ido-file-dir-completion-map (kbd "SPC") 'self-insert-command)))

;; Use Ido for completing-read, such as describe-variable (C-h v)
;; From http://www.emacswiki.org/emacs/InteractivelyDoThings#toc13
;; (defvar ido-enable-replace-completing-read t
;;  "If t, use ido-completing-read instead of completing-read if possible.

;; Set it to nil using let in around-advice for functions where the
;; original completing-read is required.  For example, if a function
;; foo absolutely must use the original completing-read, define some
;; advice like this:

;;    (defadvice foo (around original-completing-read-only activate)
;;      (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
;; (defadvice completing-read
;;  (around use-ido-when-possible activate)
;;  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
;;          (and (boundp 'ido-cur-list)
;;               ido-cur-list)) ; Avoid infinite loop from ido calling this
;;      ad-do-it
;;    (let ((allcomp (all-completions "" collection predicate)))
;;      (if allcomp
;;          (setq ad-return-value
;;                (ido-completing-read prompt
;;                                     allcomp
;;                                     nil require-match initial-input hist def))
;;        ad-do-it))))

;; Don't guess filenames at all when I'm in dired; it's never what I want.
;; Also, turn off ido-completing-read, as it messes up dired-do-rename, and
;; probably other stuff too.
;; (add-hook 'dired-mode-hook
;;          (lambda ()
;;             (set (make-local-variable 'ido-use-filename-at-point) nil)
;;             (set (make-local-variable 'ido-enable-replace-completing-read) nil)))

;; python.el doesn't like ido-completing-read either
;; (add-hook 'python-mode-hook
;;          (lambda ()
;;             (set (make-local-variable 'ido-enable-replace-completing-read) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Found from http://www.emacswiki.org/emacs/CocoAspell
(setq ispell-program-name "aspell"
      ispell-dictionary "english"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))

;; Save personal dictionary in emacs structure
(setq ispell-personal-dictionary
      (cjp-emacs-structure-dir ".aspell-personal-dictionary"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use ibuffer for my buffer menu (C-x C-b)
(defalias 'list-buffers 'ibuffer)

;; ibuffer defaults to opening files with ibuffer-find-file; I prefer ido
(add-hook 'ibuffer-load-hook (lambda ()
                                (define-key ibuffer-mode-map
                                  (kbd "C-x C-f") 'ido-find-file)))

;; `* !' is what dired uses to clear all marks
(add-hook 'ibuffer-load-hook (lambda ()
                               (define-key ibuffer-mode-map
                                 (kbd "* !") 'ibuffer-unmark-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GTD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gtd)
;; (gtd-use-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A huge number forces windows to be split vertically, like C-x 3 does
;; (setq split-height-threshold 900)

(setq tab-always-indent 'complete)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)

;; If using customize, save generated elisp here, not .emacs
(setq custom-file (cjp-emacs-structure-dir ".customize.el"))

;; If saving a .el file in my emacs structure, automatically byte compile it.
;; From stackoverflow.com/questions/154097/whats-in-your-emacs/2277001#2277001
(add-hook 'after-save-hook
          (lambda ()
             (when (string-match
                    (concat (expand-file-name
                             (cjp-emacs-structure-dir "lisp/")) ".*\.el$")
                    buffer-file-name)
               (byte-compile-file buffer-file-name))))

;; Put auto save files here
(setq auto-save-list-file-prefix (cjp-emacs-structure-dir ".auto-save-list/.saves-"))

;; Store tetris scores
(setq tetris-score-file (cjp-emacs-structure-dir ".tetris-scores"))

;; Make scripts executable when saved by default (chmod +x)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; These functions area disabled by default for new users. I want them!
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Mode to use for the initial scratch buffer
;; (setq-default initial-major-mode 'python-mode)

;; Word moving commands move point between CamelCaseWords
(global-subword-mode 1)

;; Don't always ask if I want to make a new file or buffer, just do it
(setq confirm-nonexistent-file-or-buffer nil)

;; I use this function a lot so create a shortcut. M-x bc invokes it
(defalias 'bc 'emacs-lisp-byte-compile)

;; Auto-fill mode is useful in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Remove the "This buffer is for notes" text that shows at the top of the
;; scratch buffer when Emacs loads
(setq initial-scratch-message nil)

;; Store all backup files in one folder, not all over filesystem
(setq backup-directory-alist (list (cons "." (cjp-emacs-structure-dir "backup/")))
      version-control t                 ; Use version numbers for backups
      kept-new-versions 2               ; Number of newest versions to keep
      kept-old-versions 2               ; Number of oldest versions to keep
      delete-old-versions t             ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t)  ; Copy linked files, don't rename

;; Store all autosave files in one folder, not all over filesystem
(add-to-list 'auto-save-file-name-transforms
             `(".*" ,(cjp-emacs-structure-dir "autosaves/") t) t)

;; From
;; emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
;; Highlights comments like /* FIXME: do something */ in C-like (C, C++, Obj-C,
;; etc.) languages
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; Move mouse to top-right corner once it gets too close to cursor.  Move back
;; once mouse moved away
(mouse-avoidance-mode 'exile)

;; Forces lines longer than buffer width to overlap in a nice way. I don't
;; think I'm too keen on it, so turned it off for the time being.
(global-visual-line-mode 0)

;; Use nxml-mode for XML files
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;; When viewing pdf (for example), have it auto-revert. Useful if viewing a
;; LaTeX document with AUCTeX
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Modifies kill line and copy line (C-x C-k and M-w) in place. If something is
;; selected, copy/cut as usual. If nothing is selected, copy/cut the current
;; line
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; Replace yes/no by y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Because I know where to find the help file
(setq inhibit-splash-screen t)

;; Require C-x C-c prompt. I've closed too often by accident
(global-set-key (kbd "C-x C-c")
                (lambda () (interactive)
                   (cond ((y-or-n-p "Quit? ")
                          (save-buffers-kill-emacs)))))

;; Always flash for parens
(show-paren-mode 1)

;; Set mode of buffer automatically based on filename or other indications (see
;; set-auto-mode documentation), so can quickly make a temp. buffer (like
;; *Scratch*) called 'test.txt' to make it open in text-mode, or 'test.js' for
;; javascript-mode, etc.
(setq default-major-mode (lambda ()
     (let ((buffer-file-name (or buffer-file-name (buffer-name))))
          (set-auto-mode))))

;; Open new buffers (without files or filename extensions) in markdown-mode
(add-to-list 'auto-mode-alist '("" . markdown-mode) t)

;; Open log files in text mode, for now
(add-to-list 'auto-mode-alist '("\\.log\\'" . text-mode))

;; Keep ispell word as M-s even when editing git commit logs
(add-hook 'log-edit-mode-hook
          (lambda () (define-key log-edit-mode-map (kbd "M-s") 'ispell-word)))

;; I like this mode; seems to be on by default under emacs-snapshot on
;; GNU/Linux systems
(transient-mark-mode 1)

;; Make the compilation window appear smallish (not half of frame as default)
(setq compilation-window-height 10)

;; Set default path to my inbox
;; (setq default-directory "~/Documents/Inbox/")

;; True by default in Carbon Emacs. Set here for Aquamacs and other distros
(setq x-select-enable-clipboard t)

;; Remove the annoying toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Mute annoying beep
(setq visible-bell t)

;; Stop cursor from blinking
(blink-cursor-mode -1)

;; Let emacsclient send stuff to existing Emacs process
(if (display-graphic-p)
    (server-start))

;; Don't use tabs
(setq-default indent-tabs-mode nil)

;; Set auto-fill-mode to fill to column 80
(setq default-fill-column 80)

;; Set tab key to two spaces
(setq-default c-basic-offset 4)

;; Tab binary character in files interpreted as mod-4
(setq-default tab-width 4)

;; Show column number as well as line number
(setq column-number-mode t)

;; My prefered code indentation style
(setq c-set-style "gnu")

;; When double-clicking a file to open in Emacs, make sure it opens in a new
;; window in the current frame; the default (nil) causes Emacs to create a new
;; frame.
(setq display-buffer-reuse-frames t)

;; When lines wrap, `next-line' drops to the next real line, not the next
;; visual line
(setq line-move-visual t)

;; From
;; masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-safe-themes t)

(if (display-graphic-p)
  (progn (add-to-list 'custom-theme-load-path (cjp-emacs-structure-dir "themes"))
         (load-theme 'tangotango t))
  (color-theme-initialize)
  (color-theme-hober))
