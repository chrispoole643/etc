;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Library Functions (Mac)
;;;; =======================
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cjp-mac-open-terminal (&optional shell)
  "Change to current file's directory in Terminal.app.

If in a dired buffer, change to current directory. Buffers with
no file open home directory.

Use a prefix argument to change to current file's directory in
existing eshell buffer."
  (interactive "P")
  (let ((dir (or (and (eq major-mode 'dired-mode)
                      (cjp-tilde-to-longform dired-directory))
                 (and (eq major-mode 'eshell-mode)
                      default-directory)
                 (and buffer-file-name
                      (file-name-directory (buffer-file-name)))
                 (error "Buffer has no directory associated"))))
    (setq dir (concat "'" dir "'"))
    (if shell
        (progn
          (when (get-buffer "*eshell*")
            (switch-to-buffer-other-window "*eshell*")
            (goto-char (point-max))
            (insert "cd " dir)
            (eshell-send-input)))
      (do-applescript
       (format "
tell application \"Terminal\"
	try
		if (count of windows) is 0 then
			do script \"cd %s\"
		else
			do script \"cd %s\" in window 1
		end if
		activate
	on error
		beep
	end try
end tell" dir dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cjp-mac-show-finder (&optional directoryp)
  "If in dired buffer, show file at point in Finder.

If in buffer with an associated file, show it in Finder.

If passed a prefix argument (e.g., C-u), show the directory that
contains the file instead."
   (interactive "P")
   (let* ((item
          (or (and (eq major-mode 'dired-mode)
                   (cond (directoryp
                          (cjp-tilde-to-longform default-directory))
                         ((not (dired-get-marked-files))
                          (cjp-tilde-to-longform (dired-get-file-for-visit)))
                         (t
                          (dired-get-marked-files))))
              (and (eq major-mode 'eshell-mode)
                   default-directory)
              (and buffer-file-name
                   (if directoryp
                       (file-name-directory buffer-file-name)
                     buffer-file-name))
              (error "No file associated with buffer")))
         (applescript-reveal-line (cjp-form-applescript-reveal-string item)))
     (do-applescript
      (format "
tell application \"Finder\"
	reveal %s
    activate
end tell
" applescript-reveal-line))))

(defun cjp-tilde-to-longform (string &optional initial)
  "Replace INITIAL with expansion of `~' in STRING. If INITIAL
isn't given, use `~'."
  (replace-regexp-in-string (or initial "~") (expand-file-name "~") string t))

(defun cjp-form-applescript-reveal-string (items)
  "Take list of items (i.e., file locations), return a string in
form appropriate for AppleScript `reveal` verb."
  (if (stringp items)
      (concat "{\"" (cjp-tilde-to-longform items) "\" as POSIX file}")
    (let ((return-string ""))
      (while items
        (setq return-string (concat
                             return-string
                             "\""
                             (cjp-tilde-to-longform (car items))
                             "\" as POSIX file"
                             (if (not (eq (cdr items) nil))
                                 ", ")))
        (setq items (cdr items)))
      (concat "{" return-string "}"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open files with external programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cjp-mac-guess-open-file (&rest args)
  "Open current file with /usr/bin/open.

If in a dired buffer, open marked files, or file at point."
  (interactive)
  (apply 'cjp-guess-open-file-external "/usr/bin/open" args))

(defun cjp-mac-textedit-file ()
  "Open file with TextEdit.

If in dired buffer, edit file at point."
  (interactive)
  (apply 'cjp-guess-open-file-external "/usr/bin/open" '("-a" "TextEdit")))

(defun cjp-mac-quicklook-file ()
  "When invoked for the first time, view file with
quicklook. When invoked again, kill the process to return to
Emacs.

If in dired buffer, view file at point."
  (interactive)
  (if (get-process "qlmanage")
      (kill-process "qlmanage")
    (apply 'cjp-guess-open-file-external "/usr/bin/qlmanage" '("-p"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cjp-find-with-google (&optional search-term)
  "Search for SEARCH-TERM with Google in the default Mac browser.

If called interactively, search for the highlighted region, or
request a string."
  (interactive)
  (let ((search-string (or search-term
                           (if mark-active
                               (buffer-substring (region-beginning) (region-end)))
                           (read-string "Search for: "))))
    (browse-url-default-macosx-browser
     (format "https://encrypted.google.com/search?q=%s" search-string))))

(defun cjp-browse-url-on-line ()
  "Search for a URL on the current line, and open it in the
default Mac browser."
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (re-search-forward "[a-zA-Z]+://" (point-at-eol) t)
    (browse-url-default-macosx-browser (thing-at-point 'url))))

(defun cjp-get-zsh-env-var (var)
  "Return VAR variable defined for MacBook Pro"
  (with-temp-buffer
    (insert-file-contents "~/.zshrc-mac")
    (goto-char (point-min))
    (re-search-forward (concat "export " var "=\"\\([^\"]+\\)\""))
    (cjp-tilde-to-longform (match-string 1) "$HOME")))

(defun cjp-check-uni-proxy ()
  "Set HTTP_PROXY environment variable if at uni, else remove it.

Works in conjunction with marco polo script."
  (interactive)
  (if (file-exists-p "~/.atuni")
      (setenv "HTTP_PROXY" "http://wwwcache.lancs.ac.uk:8080")
    (setenv "HTTP_PROXY" nil)))

(defun cjp-show-growl-notification (&optional arg-msg)
  "Show growl notification with string ARG-MSG, or prompt for
message if called interactively."
  (interactive "P")
  (let ((msg (or arg-msg
                 (read-string "Message: "))))
    (start-process "growlnotify" nil "/usr/local/bin/growlnotify"
                   "Emacs" "-m" (format "%s" msg) "-a" "Emacs" "-n" "Emacs")))
