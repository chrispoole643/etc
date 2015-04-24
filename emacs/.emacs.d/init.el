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
(let ((file (concat user-emacs-directory "lisp/cjp-library.org")))
  (if (file-exists-p file) (org-babel-load-file file)))

;;; Add lisp directory tree to load-path
(setq load-path (append (cjp-get-dir-structure-in "lisp")
                        load-path))

;;; Load private files
(mapc (lambda (file)
        (let ((absfile (expand-file-name (concat "~/.emacs.d-private/" file))))
          (when (file-exists-p absfile)
            (cjp-org-load-file absfile))))
      '("cjp-library-private" "cjp-settings-private"))

;;; Load all my settings, as well as contributed functions
(cjp-org-load-file "library-contributed")
(load-library "cjp-settings")

;;; Load stuff for GNU/Linux systems only
(when linuxp
  (cjp-org-load-file "cjp-library-linux")
  (cjp-org-load-file "cjp-settings-linux"))

;;; Load stuff for Mac OS X only
(when macosxp
  (cjp-org-load-file "cjp-library-mac")
  (cjp-org-load-file "cjp-settings-mac"))

(cjp-org-load-file "cjp-keybindings")

