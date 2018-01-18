;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs Configuration
;;;; ===================
;;;;
;;;; Bootstrapping setup to be able to load the content kept in org-mode
;;;; files. Loads generally-required files first, then those that are
;;;; platform-specific.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define org and melpa as package sources, and install `use-package' if it's not
;;; already there. Always ensure packages being loaded are there (or else it'll
;;; automatically download from melpa)

(require 'package)
(setq user-emacs-directory "~/.emacs.d/")
(setq package-user-dir (concat user-emacs-directory "packages"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(when (>= emacs-major-version 25) (setq package-archive-priorities '(("org" . 3)
								     ("melpa" . 2)
								     ("gnu" . 1))))
(setq package-load-list '(all))
(package-initialize)
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(add-hook 'package-menu-mode-hook 'hl-line-mode)

;;; Load org mode early to ensure that the orgmode ELPA version gets picked up, not the
;;; shipped version
(use-package org-plus-contrib
  :pin org)
(use-package org
  :ensure org-plus-contrib
  :pin org)

;;; Check to see if running on Mac OS X or some GNU/Linux distro
(defvar macosxp (string-match "darwin" (symbol-name system-type)))
(defvar linuxp (string-match "gnu/linux" (symbol-name system-type)))
(defvar workp (string-match "ibm" system-name))

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
