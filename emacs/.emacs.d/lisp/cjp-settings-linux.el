;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom stuff for GNU/Linux systems
;;;; ==================================
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set font (Menlo default)
(if (display-graphic-p)
    (cjp-set-font-size "12" "Inconsolata" t))

;;; Make Super another Meta key (useful for Mac keyboard)
(setq x-super-keysym 'super)

;;; Remove menubar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;; Resize Emacs frame on startup, and place at top-left of screen
(if (display-graphic-p)
    (setq default-frame-alist
          (append '((left . 60) (top . 0) (width . 90) (height . 30))
                  default-frame-alist)))

;;; Change binding for outline minor mode
(add-hook 'outline-minor-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c")
                               outline-mode-prefix-map)))

;;; GTD structure is different on my Thinkpad
(setq gtd-dir "~/gtd/")
(setq gtd-dir-support-project "~/documents/projects/")
(setq gtd-dir-support-sm "~/documents/someday-maybe/")

;;; Browse URLs with Chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;; Opening files using xdg-open in org-mode doesn't work. This appears to be /a/
;;; solution, perhaps not the best one long
;;; term. http://lists.gnu.org/archive/html/emacs-orgmode/2014-04/msg00912.html
(setq process-connection-type nil)
