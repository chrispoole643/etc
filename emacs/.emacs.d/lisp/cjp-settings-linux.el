;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom stuff for GNU/Linux systems
;;;; ==================================
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set font (Menlo default)
(if (display-graphic-p)
    (cjp-set-font-size "10" "Inconsolata" t))

;;; Make Super another Meta key (useful for Mac keyboard)
(setq x-super-keysym 'super)

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
