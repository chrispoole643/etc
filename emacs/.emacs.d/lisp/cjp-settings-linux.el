;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom stuff for GNU/Linux systems
;;;; ==================================
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set font (Menlo default)
(if (display-graphic-p)
    (cjp-set-font-size "10" "Monospace"))

;; Move scrollbars to right side of frames
(menu-bar-right-scroll-bar)

;; Make Super another Meta key (useful for Mac keyboard)
(setq x-super-keysym 'super)

;; Remove menubar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Change binding for outline minor mode
(add-hook 'outline-minor-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c")
                               outline-mode-prefix-map)))

;; GTD structure is different on my Thinkpad
(setq gtd-dir "~/gtd/")
(setq gtd-dir-support-project "~/documents/projects/")
(setq gtd-dir-support-sm "~/documents/someday-maybe/")

(add-hook 'java-mode-hook 'outline-minor-mode)
