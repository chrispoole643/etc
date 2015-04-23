;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs Configuration
;;;; ===================
;;;;
;;;; Loads generally-required elisp, then platform-specific files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Check to see if running on Mac OS X or some GNU/Linux distro
(defvar macosxp (string-match "darwin" (symbol-name system-type)))
(defvar linuxp (string-match "gnu/linux" (symbol-name system-type)))

(setq user-emacs-directory "~/.emacs.d/")

;;; Bootstrap with my library functions
;;; (`cjp-library' contains `cjp-get-dir-structure-in')
(let ((file (concat user-emacs-directory "lisp/cjp-library.el")))
  (if (file-exists-p file) (load-file file)))

;;; Add lisp directory tree to load-path
(setq load-path (append (cjp-get-dir-structure-in "lisp")
                        load-path))

;;; Load private files
(mapc (lambda (file)
        (let ((absfile (expand-file-name (concat "~/.emacs.d-private/" file))))
          (when (file-exists-p absfile)
            (org-babel-load-file absfile))))
      '("cjp-library-private.org" "cjp-settings-private.org"))

;;; Load all my settings, as well as contributed functions
(load-library "library-contributed")
(load-library "cjp-settings")

;;; Load stuff for GNU/Linux systems only
(when linuxp
  (load-library "cjp-library-linux")
  (load-library "cjp-settings-linux"))

;;; Load stuff for Mac OS X only
(when macosxp
  (load-library "cjp-library-mac")
  (load-library "cjp-settings-mac"))

(load-library "cjp-keybindings")
