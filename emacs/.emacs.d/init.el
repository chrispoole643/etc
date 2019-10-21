;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs Configuration
;;;; ===================
;;;;
;;;; Bootstrapping setup to be able to load the content kept in org-mode
;;;; files. Loads generally-required files first, then those that are
;;;; platform-specific.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use straight.el instead of the packages system
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Load and configure use-package to use straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(add-hook 'package-menu-mode-hook 'hl-line-mode)

;;; Load org mode early to ensure that the orgmode ELPA version gets picked up, not the
;;; shipped version
(use-package org)

;;; Check to see if running on Mac OS X or some GNU/Linux distro
(defvar macosxp (string-match "darwin" (symbol-name system-type)))
(defvar linuxp (string-match "gnu/linux" (symbol-name system-type)))
(defvar workp (string-match "ibm" (system-name)))

;;; Bootstrap with my library functions
(let ((file (concat user-emacs-directory "lisp/cjp-library.org")))
  (if (file-exists-p file) (org-babel-load-file file)))

;;; Add lisp directory tree to load-path
(setq load-path (append (cjp-get-dir-structure-in "lisp") load-path))

;;; Load private files
(mapc (lambda (file)
        (let ((expanded-file (expand-file-name (concat "~/.emacs.d-private/" file ".org"))))
          (when (file-exists-p expanded-file)
            (org-babel-load-file expanded-file))))
      '("cjp-library-private" "cjp-settings-private"))

;;; Load all my settings, as well as contributed functions
(cjp-org-load-file "library-contributed")
(cjp-org-load-file "cjp-settings")

;;; Load stuff for GNU/Linux systems only
(when linuxp
  (cjp-org-load-file "cjp-library-linux")
  (cjp-org-load-file "cjp-settings-linux"))

;;; Load stuff for Mac OS X only
(when macosxp
  (cjp-org-load-file "cjp-library-mac")
  (cjp-org-load-file "cjp-settings-mac"))
