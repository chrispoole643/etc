;;; powerline-srgb-offset.el --- Fix Powerline sRGB Colour for OS X

;; Copyright (C) 2015 Billy He

;; Author: Billy He <qiushihe@me.com>
;; URL: https://github.com/qiushihe/dotpiles/blob/master/emacs/user-packages/powerline-srgb-offset.el

;;; Commentary:
;;
;; Be sure to activate the fix *before* activating a powerline theme:
;;
;; (load-library "powerline-srgb-offset")
;; (powerline-srgb-offset-activate "solarized-dark")
;; (powerline-default-theme)
;;
;; To register colours for additional themes:
;;
;; (powerline-srgb-offset-add-theme
;;   "solarized-light"
;;     '('("mode-line" "#6A8188")
;;     '("powerline-active1" "#96A5A6")
;;     '("powerline-active2" "#788E95")
;;     '("mode-line-inactive" "#96A5A6")
;;     '("powerline-inactive1" "#F2ECDD")
;;     '("powerline-inactive2" "#A4B0B1")))
;;
;; The colour values above should be the *wrong* colours displayed.

;; Note: You may load this extension before powerline is loaded as long as powerline is loaded by
;;       the time you activate the fix.
;;

;;; Code:

(setq powerline-srgb-offset-themes (make-hash-table :test 'equal))

(setq powerline-srgb-offset-theme nil)

(defun powerline-srgb-offset-add-theme (name colours)
  (let ((theme (make-hash-table :test 'equal)))
    (mapc (lambda (colour)
      (puthash (nth 0 (nth 1 colour)) (nth 1 (nth 1 colour)) theme))
      colours)
    (puthash name theme powerline-srgb-offset-themes)))

(defun powerline-srgb-offset-diff-colour (colour1 colour2)
  (let ((rgb1 (color-name-to-rgb colour1))
    (rgb2 (color-name-to-rgb colour2)))
    (color-rgb-to-hex
      (- (nth 0 rgb1) (nth 0 rgb2))
      (- (nth 1 rgb1) (nth 1 rgb2))
      (- (nth 2 rgb1) (nth 2 rgb2)))))

(defun powerline-srgb-offset-fix-colour (wrong-colour correct-colour)
  (powerline-srgb-offset-diff-colour
    correct-colour
    (powerline-srgb-offset-diff-colour wrong-colour correct-colour)))

(defun powerline-srgb-offset-advice-pl-background-color (orig-fun &rest args)
  (let* ((name (nth 0 args))
    (res (apply orig-fun args))
    (theme (and
      powerline-srgb-offset-theme
      (gethash powerline-srgb-offset-theme powerline-srgb-offset-themes)))
    (colour (and theme (gethash (prin1-to-string name) theme))))
    (or (and colour (powerline-srgb-offset-fix-colour colour res)) res)))

(defun powerline-srgb-offset-activate (theme)
  (setq powerline-srgb-offset-theme theme)
  (advice-add 'pl/background-color :around #'powerline-srgb-offset-advice-pl-background-color))

(powerline-srgb-offset-add-theme
  "solarized-dark"
  '('("mode-line" "#A4B0B0")
    '("powerline-active1" "#788E95")
    '("powerline-active2" "#96A5A6")
    '("mode-line-inactive" "#788E95")
    '("powerline-inactive1" "#014554")
    '("powerline-inactive2" "#6A8188")))

(powerline-srgb-offset-add-theme
  "solarized-light"
  '('("mode-line" "#6A8188")
    '("powerline-active1" "#96A5A6")
    '("powerline-active2" "#788E95")
    '("mode-line-inactive" "#96A5A6")
    '("powerline-inactive1" "#F2ECDD")
    '("powerline-inactive2" "#A4B0B1")))

;;; powerline-srgb-offset.el ends here
