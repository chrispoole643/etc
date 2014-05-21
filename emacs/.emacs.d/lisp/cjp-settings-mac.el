;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom stuff for Mac OS X systems only
;;;; ======================================
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path and proxy changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH" (cjp-get-zsh-env-var "PATH"))
(setq exec-path (split-string (getenv "PATH") ":"))
(add-to-list 'exec-path "/Applications/Emacs.app/Contents/MacOS/bin" t)

(setenv "PYTHONPATH" (cjp-get-zsh-env-var "PYTHONPATH"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use 'o' in w3m buffers to open current page in default Mac browser
(add-hook 'w3m-mode-hook
          (lambda () (define-key w3m-mode-map (kbd "o")
                  (lambda () (interactive)
                    (browse-url-default-macosx-browser w3m-current-url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUCTeX
;;; ------
;;;
;;; Functions here to pass Skim the page information for SyncTeX from
;;; bleedingmind.com/index.php/2010/06/17/synctex-on-linux-and-mac-os-x-with-emacs/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skim-make-url ()
  (concat
   (TeX-current-line)
   " '" (expand-file-name (funcall file (TeX-output-extension) t)
                          (file-name-directory (TeX-master-file)))
   "' '" (buffer-file-name) "'"))

(setq LaTeX-command "latex -synctex=1")

(add-hook 'LaTeX-mode-hook
          (lambda ()
             (add-to-list 'TeX-expand-list '("%q" skim-make-url))
             (setq TeX-view-program-list
                   '(("Skim"
                      "/Applications/Skim.app/Contents/SharedSupport/displayline -br %q")))
             (setq TeX-view-program-selection '((output-pdf "Skim")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;; -------------
;;;
;;; First few from
;;; reddit.com/r/emacs/comments/ab5m7/whats_your_emacs_23_os_x_setup/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set font (Menlo default)
(if (display-graphic-p)
    (cjp-set-font-size "11"))

;;; Eenable sRGB
(setq ns-use-srgb-colorspace t)

;;; Use default Mac OS X browser, and move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;;; Resize Emacs frame on startup, and place at top-left of screen.
;;; Height for MacBook Pro 15" screen
(if (display-graphic-p)
    (setq default-frame-alist
          (append '((left . 60) (top . 0) (width . 90) (height . 56))
                  default-frame-alist)))

;;; I prefer cmd key for meta, and the right cmd as super. Left alt is super,
;;; but right alt is unset, allowing me to type (right) alt-3 and get a #
;;; symbol.
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super
      mac-right-command-modifier 'super
      ns-right-alternate-modifier 'none)

;;; In Emacs 23 (Cocoa) in Snow Leopard, Apple delete key deletes backward, not
;;; forward as is usual. This fixes this behaviour.
(if (display-graphic-p)
    (normal-erase-is-backspace-mode 1))

;;; Turn off menubar if using Emacs in terminal
(if (not (display-graphic-p))
    (menu-bar-mode -1))

;;; When dragging files onto Emacs icon in dock, don't create a new frame
(setq ns-pop-up-frames nil)
