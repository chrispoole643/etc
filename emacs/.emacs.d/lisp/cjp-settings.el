;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Generic Emacs Configuration
;;;; ===========================
;;;;
;;;; System-agnostic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Melpa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cjp-required-packages '(;; Common Lisp
                                ac-slime
                                redshank
                                slime

                                ;; LaTeX
                                auctex
                                reftex

                                ;; C/C++
                                c-eldoc

                                ;; Clojure
                                cider
                                clojure-mode

                                ;; Python
                                ein
                                elpy
                                pydoc-info
                                python-info
                                pyvenv

                                ;; Elisp
                                elisp-slime-nav
                                litable
                                paredit

                                ;; Scheme
                                geiser

                                ;; JavaScript
                                js2-mode

                                ;; Markdown
                                markdown-mode
                                markdown-mode+

                                ;; Color themes
                                color-theme
                                monokai-theme
                                solarized-theme
                                tangotango-theme

                                ;; Org
                                org
                                ox-reveal

                                ;; HTML
                                zencoding-mode

                                ;; Helm
                                helm
                                ac-helm
                                helm-dictionary
                                helm-swoop

                                ;; Miscellaneous
                                ace-isearch
                                ace-jump-mode
                                auto-complete
                                bookmark+
                                cl-lib
                                dictionary
                                diminish
                                dired+
                                dired-details+
                                edit-server
                                expand-region
                                flymake-cursor
                                flx-ido
                                framemove
                                highlight-symbol
                                htmlize
                                iedit
                                irfc
                                magit
                                multiple-cursors
                                outline-magic
                                persp-mode
                                popwin
                                powerline
                                pretty-lambdada
                                regex-tool
                                restclient
                                scpaste
                                smex
                                smooth-scrolling
                                undo-tree
                                visual-regexp
                                w3m
                                writeroom-mode
                                yasnippet)
  "Required packages to be pulled from melpa.")

(require 'package)

(setq package-user-dir (cjp-emacs-structure-dir "elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(setq package-load-list '(all))
(package-initialize)

(when (or (not (file-exists-p package-user-dir))
       (< (length (directory-files package-user-dir)) 3))
  (unless package-archive-contents
    (package-refresh-contents))
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
        cjp-required-packages))

(add-hook 'package-menu-mode-hook 'hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Requirements and libraries
;;; --------------------------
;;;
;;; These don't require their own section with other commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'generic-x)
(require 'smallurl)
(require 'multiple-cursors)
(require 'iedit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Treat all themes as safe
(setq custom-safe-themes t)

;; Make the fringe stand out from the background
(setq solarized-distinct-fringe-background nil)

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; Make the modeline high contrast: makes it easy to notice the current buffer
(setq solarized-high-contrast-mode-line t)

;;; Draw the underline at the same place as the descent line: looks better
(setq x-underline-at-descent-line t)

(load-theme 'solarized-light t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ace-isearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ace-isearch)
(global-ace-isearch-mode t)

(setq ace-isearch-input-idle-delay 0.4
      ace-isearch-input-length 10
      ace-isearch-function-from-isearch 'helm-swoop-from-isearch
      ace-isearch-submode 'ace-jump-char-mode
      ace-isearch-use-ace-jump 'printing-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm
;;; ----
;;;
;;; Good setup advice from https://tuhdo.github.io/helm-intro.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)
(helm-mode 1)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t    ; Open helm buffer inside current window
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount 8             ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;;; Fuzzy match where possible
(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t)

;;; M-x doesn't work with same keys on all systems, so bind to C-x X-m too (And C-c for
;;; good measure, in case your finger slips)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-c C-m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-d") 'helm-find-files) ; Replace ido-dired, too
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c C-f") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)
(define-key helm-map (kbd "C-w") 'backward-kill-word)

(define-key helm-map (kbd "M-n") 'helm-next-source)
(define-key helm-map (kbd "M-p") 'helm-previous-source)

;;; If the thing at point is a directory, go into the directory (as though hitting
;;; <tab>). Else, open it. If the directory is `.' or `..', open in dired as usual.
(define-key helm-find-files-map (kbd "<return>")
  '(lambda () (interactive) (let ((sel (helm-get-selection)))
                         (if (and (file-directory-p sel)
                                  (not (helm-ff-dot-file-p sel)))
                             (helm-execute-persistent-action)
                           (helm-maybe-exit-minibuffer)))))

;;; If the first two items in helm-find-files results are '.' and '..', move cursor down by two
(add-hook 'helm-after-update-hook
          (lambda () (when (and (helm-file-completion-source-p)
                           (not (helm-empty-source-p))
                           (string-match "/\\.$" "file/." ))
                  (helm-next-line 2))))

;;; Use thing at point when invoking helm-man-woman
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;;; helm-swoop

(require 'helm-swoop)

(define-key isearch-mode-map (kbd "M-s") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-s") 'helm-multi-swoop-all-from-helm-swoop)
(define-key helm-multi-swoop-map (kbd "M-s") 'helm-mult)

;;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

(setq helm-swoop-use-line-number-face t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Semantic mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hl-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restclient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Don't change window focus to the output window when submitting a command
(eval-after-load "restclient-autoloads"
  '(add-hook 'restclient-mode-hook
            (lambda () (local-set-key (kbd "C-c C-c")
                                      '(lambda () (interactive)
                                        (restclient-http-send-current nil t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writeroom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defaults to 80. Allow a bit more if using in conjunction with org mode, where
;; the document might have indented lines
(setq writeroom-width 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Popwin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'popwin)
(popwin-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (with-eval-after-load "persp-mode-autoloads"
;;   ;; switch off animation of restoring window configuration
;;   (setq wg-morph-on nil)
;;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diminish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'diminish)

;;; Alphanumeric unicode characters with circles around them are listed on
;;; https://en.wikipedia.org/wiki/Enclosed_Alphanumerics
;;; Ⓐ Ⓑ Ⓒ Ⓓ Ⓔ Ⓕ Ⓖ Ⓗ Ⓘ Ⓙ Ⓚ Ⓛ Ⓜ Ⓝ Ⓞ Ⓟ Ⓠ Ⓡ Ⓢ Ⓣ Ⓤ Ⓥ Ⓦ Ⓧ Ⓨ Ⓩ

(eval-after-load "auto-complete" '(diminish 'auto-complete-mode " Ⓐ"))
(eval-after-load "abbrev" '(diminish 'abbrev-mode " Ⓐ"))
(eval-after-load "anzu" '(diminish 'anzu-mode " Ⓐ"))
(eval-after-load "elpy" '(diminish 'elpy-mode " Ⓔ"))
(eval-after-load "simple" '(diminish 'auto-fill-function " Ⓕ"))
(eval-after-load "helm" '(diminish 'helm-mode " Ⓗ"))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode " Ⓜ"))
(eval-after-load "org-indent" '(diminish 'org-indent-mode " Ⓞ"))
(eval-after-load "paredit" '(diminish 'paredit-mode " Ⓟ"))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode " Ⓤ"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ein
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ein)
(setq ein:use-auto-complete t)
;;; Or, to enable "superpack" (a little bit hacky improvements):
;; (setq ein:use-auto-complete-superpack t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RFC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'irfc)
(setq irfc-assoc-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Zencoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redshank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'redshank-loader)
(eval-after-load "redshank-loader"
  `(redshank-setup '(lisp-mode-hook
                     slime-repl-mode-hook) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'powerline)
(powerline-default-theme)




;; A string is printed verbatim in the mode line except for %-constructs:
;;   %b -- print buffer name.      %f -- print visited file name.
;;   %F -- print frame name.
;;   %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;; 	%& is like %*, but ignore read-only-ness.
;; 	% means buffer is read-only and * means it is modified.
;; 	For a modified read-only buffer, %* gives % and %+ gives *.
;;   %s -- print process status.   %l -- print the current line number.
;;   %c -- print the current column number (this makes editing slower).
;;         To make the column number update correctly in all cases,
;; 	`column-number-mode' must be non-nil.
;;   %i -- print the size of the buffer.
;;   %I -- like %i, but use k, M, G, etc., to abbreviate.
;;   %p -- print percent of buffer above top of window, or Top, Bot or All.
;;   %P -- print percent of buffer above bottom of window, perhaps plus Top,
;;         or print Bottom or All.
;;   %n -- print Narrow if appropriate.
;;   %t -- visited file is text or binary (if OS supports this distinction).
;;   %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
;;   %Z -- like %z, but including the end-of-line format.
;;   %e -- print error message about full memory.
;;   %@ -- print @ or hyphen.  @ means that default-directory is on a
;;         remote machine.
;;   %[ -- print one [ for each recursive editing level.  %] similar.
;;   %% -- print %.   %- -- print infinitely many dashes.
;; Decimal digits after the % specify field width to which to pad.




(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (powerline-selected-window-active))
                        (mode-line (if active 'mode-line 'mode-line-inactive))
                        (face1 (if active 'powerline-active1 'powerline-inactive1))
                        (face2 (if active 'powerline-active2 'powerline-inactive2))
                        (separator-left (intern (format "powerline-%s-%s"
                                                        (powerline-current-separator)
                                                        (car powerline-default-separator-dir))))
                        (separator-right (intern (format "powerline-%s-%s"
                                                         (powerline-current-separator)
                                                         (cdr powerline-default-separator-dir))))
                        (lhs (list (powerline-raw "%*" nil 'l)
                                   (when powerline-display-buffer-size
                                     (powerline-buffer-size nil 'l))
                                   (when powerline-display-mule-info
                                     (powerline-raw mode-line-mule-info nil 'l))
                                   (powerline-buffer-id nil 'l)
                                   (when (and (boundp 'which-func-mode) which-func-mode)
                                     (powerline-raw which-func-format nil 'l))
                                   (powerline-raw " ")
                                   (funcall separator-left mode-line face1)
                                   (when (boundp 'erc-modified-channels-object)
                                     (powerline-raw erc-modified-channels-object face1 'l))
                                   (powerline-major-mode face1 'l)
                                   (powerline-process face1)
                                   (powerline-minor-modes face1 'l)
                                   (powerline-narrow face1 'l)
                                   (powerline-raw " " face1)
                                   (funcall separator-left face1 face2)
                                   (powerline-vc face2 'r)
                                   (when (bound-and-true-p nyan-mode)
                                     (powerline-raw (list (nyan-create)) face2 'l))))
                        (rhs (list (powerline-raw global-mode-string face2 'r)
                                   (funcall separator-right face2 face1)
                                   (unless window-system
                                     (powerline-raw (char-to-string #xe0a1) face1 'l))
                                   (powerline-raw "%4l" face1 'l)
                                   (powerline-raw ":" face1 'l)
                                   (powerline-raw "%3c" face1 'r)
                                   (funcall separator-right face1 mode-line)
                                   (powerline-raw " ")
                                   (powerline-raw "%6p" nil 'r)
                                   (when powerline-display-hud
                                     (powerline-hud face2 face1)))))
                   (concat (powerline-render lhs)
                           (powerline-fill face2 (powerline-width rhs))
                           (powerline-render rhs))))))










(setq powerline-default-separator 'wave)

;;; Anzu

(global-anzu-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CPerl mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; cperl-mode is preferred to perl-mode
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

(setq magit-omit-untracked-dir-contents t)

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

;;; Start ielm with AC, ElDoc, and Paredit. Make it inherit local variables from
;;; the buffer it was invoked from.
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

;; (setq browse-url-browser-function 'w3m-browse-url
;;       w3m-default-save-directory "~/Documents/inbox"
;;       w3m-use-tab nil
;;       w3m-use-tab-menubar nil
;;       w3m-key-binding "info")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bookmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bookmark+)

;;; Choose a location of bookmarks file.  Save bookmarks file every time I put a
;;; new bookmark in the file (not just when Emacs quits)
(setq bookmark-default-file (cjp-emacs-structure-dir "bookmarks")
      bookmark-save-flag 1
      bmkp-bmenu-state-file (cjp-emacs-structure-dir ".emacs-bmk-bmenu-state.el")
      bmkp-bmenu-commands-file
      (cjp-emacs-structure-dir ".emacs-bmk-bmenu-commands.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DocView
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When viewing pdf (for example), have it auto-revert. Useful if viewing a
;;; LaTeX document with AUCTeX
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;; Higher quality PDFs please
(setq doc-view-resolution 300)

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

;;; Show error messages in minibuffer, not as a GUI menu
(load-library "flymake-cursor")

;;; Use pyflakes with flymake
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

(require 'dired)
(require 'dired+)
(require 'dired-details+)

;;; Hide and show details (`ls -l` stuff) with '(' and ')'
(setq dired-details-hidden-string ""
      dired-details-initially-hide nil
      ;; dired-omit-mode, ignore dotfiles
      dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;;; This is buffer-local variable
(setq-default dired-omit-mode nil)

;;; Make return key open files in another window, except if item at point is a
;;; directory, and then open in the current window.
(define-key dired-mode-map (kbd "RET") (lambda ()
                                         (interactive)
                                         (if (cjp-dired-directoryp)
                                             (dired-find-file)
                                           (dired-find-file-other-window))))

(defadvice dired-details-toggle (after fit-dired-frame activate)
  "Resize dired buffer (horizontally) after toggling details."
  (fix-horizontal-size-to-buffer))

;;; Don't show '..' since '^' does this; show human file sizes
(setq dired-listing-switches "-Alh")

;;; The default fonts don't look nice with Tango theme, at least to my eyes
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
(setq cua-rectangle-mark-key (kbd "<C-M-return>"))
(cua-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smex)

;;; Start smex, saving into Emacs structure
(setq smex-save-file (cjp-emacs-structure-dir ".smex-items"))
;;; Smex updates its list of possible commands when run; don't let it
(setq smex-auto-update t)
;;; Update smex when Emacs has been idle for (default 60) seconds
(smex-auto-update)
(smex-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify)
;;; Instead of <2> etc. after buffer name when opening multiple files with the
;;; same name, Change it to "name" : "directory name"
(setq uniquify-buffer-name-style 'forward
      uniquify-separator ":")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Winner / Windmove / FrameMove
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Turn on winner mode to move back and forwards between window configurations
;;; with C-c left and C-c right respectively
(winner-mode 1)
(require 'framemove)
(setq framemove-hook-into-windmove t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'slime)

;;; Use sbcl
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;;(slime-setup '(slime-fancy))

;;; auto-complete for slime

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recent files
;;; ------------
;;;
;;; From http://www.masteringemacs.org/articles/2011/01/27/
;;; find-files-faster-recent-files-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)

;;; Tramp mode messes this up, causing Emacs to IO block for a short time. (From
;;; http://www.emacswiki.org/emacs/RecentFiles)
(setq recentf-auto-cleanup 'never)

;;; 50 files ought to be enough.
(setq ;; default is ~/.recentf
      recentf-save-file (cjp-emacs-structure-dir ".recentf")
      recentf-max-saved-items 1024
      recentf-exclude '("\.recentf" "\.ido\.last" "\.aux" "~$"))

;;; Enable recent files mode.
(recentf-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(yas-global-mode 1)

;;; Store my personal snippets in ~/emacs/snippets, still load the stock ones
(add-to-list 'yas/root-directory (cjp-emacs-structure-dir "contributed" "snippets"))
(add-to-list 'yas/root-directory (cjp-emacs-structure-dir "personal" "snippets"))

;;; Load snippets from all directories
;(mapc 'yas/load-directory yas/root-directory)

;;; If there are multiple snippets to choose from, use ido by default in
;;; minibuffer.
(setq yas-prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/x-prompt
                             yas/completing-prompt
                             yas/no-prompt))

;;; Yasnippet doesn't play well with ansi-term
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ElDoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'c-eldoc)

(mapc (lambda (x) (add-hook x 'turn-on-eldoc-mode))
      '(python-mode-hook
        inferior-python-mode
        emacs-lisp-mode-hook
        scheme-mode-hook
        inferior-scheme-mode-hook
        geiser-repl-mode-hook
        lisp-mode-hook
        slime-mode-hook
        slime-repl-mode-hook
        lisp-interaction-mode-hook
        c-mode-hook))

(setq c-eldoc-includes "-I./ -I../ -I/usr/include/ -I/usr/local/include/ ")

;;; Make ElDoc aware of ParEdit's most used commands (ElDoc will automatically
;;; refresh the minibuffer)
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
      ;; Use underscores for italics
      markdown-italic-underscore t
      markdown-indent-on-enter nil
      ;; Enable syntax highlighting (LaTeX)
      markdown-enable-math t)

;;; Webgen uses markdown syntax in .page files
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mark\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outline minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'outline-magic)

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python and related modes
;;; ------------------------
;;;
;;; Using python.el, not python-mode.el. The latter doesn't seem to be able to
;;; send the contents of a buffer to the interpreter easily, as python.el can
;;; (with C-c C-c).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use python-mode with files with these extensions
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))

;;; Turn on auto-complete in python shells
(add-hook 'inferior-python-mode-hook (lambda () (auto-complete-mode 1)))

;;; Use python major mode if 'python' is in hashbang.
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; Check files for pep8 mistakes
(autoload 'python-pep8 "python-pep8")
(autoload 'pep8 "python-pep8")

;;; displays "\" at the end of lines that wrap
(setq longlines-show-hard-newlines t)

;;; elpy
(elpy-enable)
(when (executable-find "ipython")
  (elpy-use-ipython))
(elpy-use-ipython)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq Info-default-directory-list
      (append (cjp-get-dir-structure-in "info")
              (cjp-get-dir-structure-in "elpa")
              Info-default-directory-list))

(setq Info-directory-list Info-default-directory-list)

;; (info-lookup-add-help
;;  :mode 'lisp-mode
;;  :regexp "[^][()'\" \t\n]+"
;;  :ignore-case t
;;  :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

;; (info-lookup-maybe-add-help
;;  :mode 'emacs-lisp-mode
;;  :regexp "[^][()`',\" \t\n]+"
;;  :doc-spec '(("(elisp)Index"          nil "^ -+ .*: " "\\( \\|$\\)")
;;              ;; Commands with key sequences appear in nodes as `foo' and
;;              ;; those without as `M-x foo'.
;;              ("(emacs)Command Index"  nil "`\\(M-x[ \t\n]+\\)?" "'")
;;              ;; Variables normally appear in nodes as just `foo'.
;;              ("(emacs)Variable Index" nil "`" "'")
;;              ;; Almost all functions, variables, etc appear in nodes as
;;              ;; " -- Function: foo" etc.  A small number of aliases and
;;              ;; symbols appear only as `foo', and will miss out on exact
;;              ;; positions.  Allowing `foo' would hit too many false matches
;;              ;; for things that should go to Function: etc, and those latter
;;              ;; are much more important.  Perhaps this could change if some
;;              ;; sort of fallback match scheme existed.
;;              ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'quack)

(setq cjp-scheme-program "mit-scheme")

(setq scheme-program-name cjp-scheme-program)

;;; Geiser is a minor mode built on scheme-mode, supporting racket (PLT-Scheme)
;;; and guile. (See info doc.)
;; (setq load-path (append (list (cjp-emacs-structure-dir "geiser/build/elisp"
;;                                                       "lisp"))
;;                        load-path))
;; (require 'geiser-install)
;; (setq geiser-active-implementations '(racket)
;;       geiser-repl-history-filename (cjp-emacs-structure-dir ".geiser-history")
;;       geiser-repl-autodoc-p nil
;;       geiser-mode-autodoc-p nil)

;;; Shamelessly stolen from info-look.el, scheme-mode
;; (info-lookup-maybe-add-help
;;  :mode 'geiser-repl-mode
;;  :regexp "[^()`',\" \t\n]+"
;;  :ignore-case t
;;  ;; Aubrey Jaffer's rendition from <URL:ftp://ftp-swiss.ai.mit.edu/pub/scm>
;;  :doc-spec '(("(r5rs)Index" nil
;;               "^[ \t]+-+ [^:]+:[ \t]*" "\\b")))

;;; Quack

(setq quack-default-program cjp-scheme-program
      quack-run-scheme-always-prompts-p nil)

;;; http://synthcode.com/wiki/scheme-complete
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

;;; Use C-w to backwards kill words, consistent with global custom
;;; settings. Also undefine C-left and C-right, to use these with winner mode.
(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key (kbd "C-w") 'paredit-backward-kill-word)
            (define-key paredit-mode-map (kbd "C-<left>") nil)
            (define-key paredit-mode-map (kbd "C-<right>") nil)))

;;; Stop SLIME's REPL from grabbing DEL,
;;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; js2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use SSH in TRAMP by default
(setq tramp-default-method "ssh")

;;; Don't make backup files when using TRAMP
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;;; Store information here (not default ~/.emacs.d/tramp)
(setq tramp-persistency-file-name (cjp-emacs-structure-dir ".tramp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Add current directory to mode line of shell windows
;; (defun add-mode-line-dirtrack ()
;;  (add-to-list 'mode-line-buffer-identification
;;               '(:propertize (" " default-directory " ") face dired-directory)))
;; (add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;;; Make sure passwords not echoed in shell
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key comint-mode-map (kbd "<down>") 'comint-next-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calendar and Diary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ;; Choose my custom diary file
      diary-file (cjp-emacs-structure-dir "diary")
      ;; Start Calendar on Monday
      calendar-week-start-day 1
      ;; European date format (DD/MM/YYYY)
      european-calendar-style 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abbrev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Operate on startup, save in specified file
(setq-default abbrev-mode t)
(setq abbrev-file-name (cjp-emacs-structure-dir ".abbrev_defs")
      ;; Save abbrevs when files are saved
      save-abbrevs t
      ;; Recognise understores too
      dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-gtd)
(require 'ox-reveal)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Use better defaults when opening files
(eval-after-load "org" '(setq org-file-apps (if macosxp
                                                org-file-apps-defaults-macosx
                                              org-file-apps-defaults-gnu)))

(setq org-attach-directory (expand-file-name "~/Support/Attachments/"))

(add-to-list 'org-capture-templates
             '("b" "PBC Entry" entry (file+headline "" "Inbox") "* PBC: %?"))

;; Structure templates
;; http://dl.dropboxusercontent.com/u/3968124/sacha-emacs.html#sec-1-7-17

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUCTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'reftex)

;;; These allow AUCTeX to parse TeX files automatically. Creates 'auto'
;;; directory with parse info for each TeX file, got annoying so disabled for
;;; now
;; (setq TeX-auto-save t)

(setq TeX-parse-self t
      ;; Use pdflatex as default mode in AuCTEX, always
      TeX-PDF-mode t
      ;; TeX-electric-sub-and-superscript nil
      )

;;; Enable math mode and auto-fill when typing LaTeX, and RefTeX
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-source-correlate-mode 1)))

;;; Use tex parser so that TeX commands aren't checked
(add-hook 'LaTeX-mode-hook (lambda () (setq ispell-parser 'tex)))

(setq TeX-source-correlate-method 'synctex)

(setq ;; Setup RefTeX with AUCTeX automatically
      reftex-plug-into-AUCTeX t
      ;; Use `-', not `:'
      reftex-section-prefixes '((0 . "part-")
                                (1 . "cha-")
                                (t . "sec-"))
      ;; Change citation format to natbib (\citet format)
      reftex-cite-format "\\citet[][]{%l}")

;;; Highlight keywords from the natbib package
(setq font-latex-match-reference-keywords
      '(("citet" "[{")))

;;; Have AUCTeX ask which is master file for multi-document TeX
(setq-default TeX-master nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ido & Flx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(require 'flx-ido)
(setq ido-save-directory-list-file (cjp-emacs-structure-dir ".ido.last"))
(ido-mode t)
(setq ido-everywhere t
      ido-enable-flex-matching t
      ;; If a buffer name that doesn't exist is chosen, just make a new one without prompting
      ido-create-new-buffer 'always
      ;; Use flx faces
      ido-use-faces nil)



;;; Ignore the .aux extensions that TeX programs create
(setq completion-ignored-extensions
      (cons "*.aux" completion-ignored-extensions))

;;; Order extensions by how I use them
(setq ido-file-extensions-order '(".tex" ".txt" ".md" ".py" ".sh" ".el" ".xml" ".htm"))

;;; Ignore files defined in variable completion-ignored-extensions
(setq ido-ignore-extensions t)

;;; Default keybinding is backspace key, but I use C-w in the non-Ido-mode
;;; minibuffers often, so this is more conventient for muscle memory
(define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)

;;; Stops Ido searching for similar-named files if I use C-x C-s to create a new
;;; file and buffer
(setq ido-auto-merge-work-directories-length -1)

;;; Keep annoying buffers out of my face
(setq ido-ignore-buffers (list (rx (or (and bos  " ")
                                       (and bos
                                            (or "*Completions*"
                                                "*Shell Command Output*"
                                                "*vc-diff*")
                                            eos)))))

;;; Allow spaces when using ido-find-file
(add-hook 'ido-make-file-list-hook
          (lambda ()
            (define-key ido-file-dir-completion-map (kbd "SPC") 'self-insert-command)))

;;; Use Ido for completing-read, such as describe-variable (C-h v)
;;; From http://www.emacswiki.org/emacs/InteractivelyDoThings#toc13
;; (defvar ido-enable-replace-completing-read t
;;  "If t, use ido-completing-read instead of completing-read if possible.

;; Set it to nil using let in around-advice for functions where the
;; original completing-read is required.  For example, if a function
;; foo absolutely must use the original completing-read, define some
;; advice like this:

;;    (defadvice foo (around original-completing-read-only activate)
;;      (let (ido-enable-replace-completing-read) ad-do-it))")

;;; Replace completing-read wherever possible, unless directed otherwise
;;; (defadvice completing-read
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

;;; Don't guess filenames at all when I'm in dired; it's never what I want.
;;; Also, turn off ido-completing-read, as it messes up dired-do-rename, and
;;; probably other stuff too.
;; (add-hook 'dired-mode-hook
;;          (lambda ()
;;             (set (make-local-variable 'ido-use-filename-at-point) nil)
;;             (set (make-local-variable 'ido-enable-replace-completing-read) nil)))

;;; python.el doesn't like ido-completing-read either
;; (add-hook 'python-mode-hook
;;          (lambda ()
;;             (set (make-local-variable 'ido-enable-replace-completing-read) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Found from http://www.emacswiki.org/emacs/CocoAspell
(setq ispell-program-name "aspell"
      ispell-dictionary "english"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))

;;; Save personal dictionary in emacs structure
(setq ispell-personal-dictionary
      (cjp-emacs-structure-dir ".aspell-personal-dictionary"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use ibuffer for my buffer menu (C-x C-b)
(defalias 'list-buffers 'ibuffer)

;;; ibuffer defaults to opening files with ibuffer-find-file; I prefer ido
(add-hook 'ibuffer-load-hook (lambda ()
                               (define-key ibuffer-mode-map
                                 (kbd "C-x C-f") 'ido-find-file)))

;;; `* !' is what dired uses to clear all marks
(add-hook 'ibuffer-load-hook (lambda ()
                               (define-key ibuffer-mode-map
                                 (kbd "* !") 'ibuffer-unmark-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unicode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Save (a longer) minibuffer history
(savehist-mode t)
(setq history-length 1024)

;;; A huge number forces windows to be split vertically, like C-x 3 does
;; (setq split-height-threshold 900)

(setq tab-always-indent 'complete)

;;; Enable narrowing
(put 'narrow-to-region 'disabled nil)

;;; If using customize, save generated elisp here, not .emacs
(setq custom-file (cjp-emacs-structure-dir ".customize.el"))

;;; If saving a .el file in my emacs structure, automatically byte compile it.
;;; From stackoverflow.com/questions/154097/whats-in-your-emacs/2277001#2277001
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (string-match
;;                    (concat "\.emacs\.d" ".*\.el$")
;;                    buffer-file-name)
;;               (byte-compile-file buffer-file-name))))

;;; Put auto save files here
(setq auto-save-list-file-prefix (cjp-emacs-structure-dir ".auto-save-list/.saves-"))

;;; Store tetris scores
(setq tetris-score-file (cjp-emacs-structure-dir ".tetris-scores"))

;;; Make scripts executable when saved by default (chmod +x)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; These functions area disabled by default for new users. I want them!
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Mode to use for the initial scratch buffer
;; (setq-default initial-major-mode 'python-mode)

;;; Word moving commands move point between CamelCaseWords
;;; FIXME causes ERC issue --- http://osdir.com/ml/bug-gnu-emacs-gnu/2014-05/msg00914.html
;; (global-subword-mode 1)

;;; In Emacs 24.3.50+ (from git), modeline lists "," - stop this
(let ((entry (assq 'subword-mode minor-mode-alist)))
  (when entry (setcdr entry '(nil))))

;;; Don't always ask if I want to make a new file or buffer, just do it
(setq confirm-nonexistent-file-or-buffer nil)

;;; I use this function a lot so create a shortcut. M-x bc invokes it
(defalias 'bc 'emacs-lisp-byte-compile)

;;; Auto-fill mode is useful in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Remove the "This buffer is for notes" text that shows at the top of the
;;; scratch buffer when Emacs loads
(setq initial-scratch-message nil)

;;; Store all backup files in one folder, not all over filesystem
(setq backup-directory-alist (list (cons "." (cjp-emacs-structure-dir "backup/")))
      ;; Use version numbers for backups
      version-control t
      ;; Number of newest versions to keep
      kept-new-versions 2
      ;; Number of oldest versions to keep
      kept-old-versions 2
      ;; Ask to delete excess backup versions?
      delete-old-versions t
      ;; Copy linked files, don't rename
      backup-by-copying-when-linked t)

;;; Store all autosave files in one folder, not all over filesystem
(let ((save-dir (cjp-emacs-structure-dir "autosaves/")))
  (when (not (file-exists-p save-dir)) (make-directory save-dir t))
  (add-to-list 'auto-save-file-name-transforms
               `(".*" ,save-dir t) t))

;;; From
;;; emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
;;; Highlights comments like /* FIXME: do something */ in C-like (C, C++, Obj-C,
;;; etc.) languages
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):"
                                       1 font-lock-warning-face t)))))

;;; Move mouse to top-right corner once it gets too close to cursor.  Move back
;;; once mouse moved away
(mouse-avoidance-mode 'exile)

;;; Forces lines longer than buffer width to overlap in a nice way. I don't
;;; think I'm too keen on it, so turned it off for the time being.
(global-visual-line-mode 0)

;;; Use nxml-mode for XML files
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;;; Modifies kill line and copy line (C-x C-k and M-w) in place. If something is
;;; selected, copy/cut as usual. If nothing is selected, copy/cut the current
;;; line
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

;;; Replace yes/no by y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; Because I know where to find the help file
(setq inhibit-splash-screen t)

;;; Require C-x C-c prompt. I've closed too often by accident
(global-set-key (kbd "C-x C-c")
                (lambda () (interactive)
                  (cond ((y-or-n-p "Quit? ")
                         (save-buffers-kill-emacs)))))

;;; Always flash for parens
(show-paren-mode 1)

;;; Set mode of buffer automatically based on filename or other indications (see
;;; set-auto-mode documentation), so can quickly make a temp. buffer (like
;;; *Scratch*) called 'test.txt' to make it open in text-mode, or 'test.js' for
;;; javascript-mode, etc.
(setq default-major-mode (lambda ()
                           (let ((buffer-file-name (or buffer-file-name (buffer-name))))
                             (set-auto-mode))))

;;; Open new buffers (without files or filename extensions) in org-mode
(add-to-list 'auto-mode-alist '("" . org-mode) t)

;;; Open log files in text mode, for now
(add-to-list 'auto-mode-alist '("\\.log\\'" . text-mode))

;;; Keep ispell word as M-s even when editing git commit logs
(add-hook 'log-edit-mode-hook
          (lambda () (define-key log-edit-mode-map (kbd "M-s") 'ispell-word)))

;;; I like this mode; seems to be on by default under emacs-snapshot on
;;; GNU/Linux systems
(transient-mark-mode 1)

;;; Make the compilation window appear smallish (not half of frame as default)
(setq compilation-window-height 10)

;;; Set default path to my inbox
;; (setq default-directory "~/Documents/Inbox/")

;;; True by default in Carbon Emacs. Set here for Aquamacs and other distros
(setq x-select-enable-clipboard t)

;;; Remove the annoying toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;; Mute annoying beep
(setq visible-bell t)

;;; Stop cursor from blinking
(blink-cursor-mode -1)

;;; Let emacsclient send stuff to existing Emacs process
(if (display-graphic-p)
    (server-start))

;;; Don't use tabs
(setq-default indent-tabs-mode nil)

;;; Set auto-fill-mode to fill to column 89 (ideal for a 90 char width)
(setq default-fill-column 89)

;;; Set tab key to two spaces
(setq-default c-basic-offset 4)

;;; Tab binary character in files interpreted as mod-4
(setq-default tab-width 4)

;;; Show column number as well as line number
(setq column-number-mode t)

;;; My prefered code indentation style
(setq c-set-style "k&r")

;;; When double-clicking a file to open in Emacs, make sure it opens in a new
;;; window in the current frame; the default (nil) causes Emacs to create a new
;;; frame
(setq display-buffer-reuse-frames t)

;;; When lines wrap, `next-line' drops to the next real line, not the next
;;; visual line
(setq line-move-visual t)

;;; From
;;; masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

;;; If I open a symlink file that is backed by a VC'd regular file, don't ask me if I
;;; want to follow the link, just do it
(setq vc-follow-symlinks t)

;;; Use hl-line mode everywhere
(global-hl-line-mode)

;;; Fix scrolling when using the mouse wheel or trackpad
(setq mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 5))
      scroll-conservatively 101)

;;; Usually suspend-frame. Annoying
(global-unset-key (kbd "C-z"))
