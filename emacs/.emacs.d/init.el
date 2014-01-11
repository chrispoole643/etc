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
(let ((file (concat user-emacs-directory "lisp/cjp-library.el"))
      (compiled-file (concat user-emacs-directory "lisp/cjp-library.elc")))
  (cond ((file-exists-p compiled-file) (load-file compiled-file))
        ((file-exists-p file) (byte-compile-file file t))))

;;; Add lisp directory tree to load-path
(setq load-path (append (cjp-get-dir-structure-in "lisp")
                        load-path))

;;; Load all my settings, as well as contributed functions
(load-library "library-contributed")
(load-library "cjp-settings")

;;; Load private files
(mapc (lambda (file)
          (let ((absfile (concat "/Users/Chris/.emacs.d-private/"
                                 file)))
            (if (file-exists-p absfile) (load-file absfile))))
        '("cjp-library-private.elc" "cjp-settings-private.elc"))

;;; Load custom faces
(load-library "cjp-faces")

;;; Load stuff for GNU/Linux systems only
(when linuxp
  (load-library "cjp-library-linux")
  (load-library "cjp-settings-linux"))

;;; Load stuff for Mac OS X only
(when macosxp
  (load-library "cjp-library-mac")
  (load-library "cjp-settings-mac"))

(load-library "cjp-keybindings")

;;; Always start a python process, for quick maths, and shell
(when (display-graphic-p)
  ;; (with-temp-buffer
  ;;   (python-mode)
  ;;   (insert "import os, math, time"))
  ;; (run-python nil t)
  (save-window-excursion (eshell)))
