
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Emacs Configuration
;;;;; ===================
;;;;;
;;;;; Loads generally-required elisp, then platform-specific files
;;;;;
;;;;; Version: 20110519
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Check to see if running on Mac OS X or some GNU/Linux distro
(defvar macosxp (string-match "darwin" (symbol-name system-type)))
(defvar linuxp (string-match "gnu/linux" (symbol-name system-type)))

(setq user-emacs-directory "~/.emacs.d/")

;;; Bootstrap with my library functions (`cjp-library' contains
;;; `cjp-get-dir-structure-in')
(load-file (concat user-emacs-directory "lisp/cjp-library-private.elc"))
(load-file (concat user-emacs-directory "lisp/cjp-library.elc"))

;;; Add lisp directory tree to load-path
(setq load-path (append (cjp-get-dir-structure-in "lisp")
                        load-path))

;;; Load all my settings, as well as contributed functions
(load-library "library-contributed")
(load-library "cjp-settings-private")
(load-library "cjp-settings")
(load-library "cjp-faces")

;;; Load stuff for GNU/Linux systems only
(when linuxp
  (load-library "cjp-settings-linux"))

;;; Load stuff for Mac OS X only
(when macosxp
  (load-library "cjp-library-mac")
  (load-library "cjp-settings-mac"))

(load-library "cjp-keybindings")

;;; Always start a python process, for quick maths, and shell
(when (display-graphic-p)
  (with-temp-buffer
    (python-mode)
    (insert "import os, math, time, numpy, scipy")
    (python-send-buffer))
  (run-python nil t)
  (save-window-excursion (eshell)))
