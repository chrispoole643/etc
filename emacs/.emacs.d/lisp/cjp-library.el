;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Library Functions
;;;; =================
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window sizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cjp-window-setup (pane-number)
  "Creates favourite window layout. Prompts for choice of either
two or three panes."
  (interactive "nNumber of panes: ")
  (cond ((eq pane-number 3)
         (delete-other-windows)
         (split-window-horizontally)
         (split-window-horizontally)
         (windmove-right)
         (windmove-right)
         (balance-windows)
         (split-window-vertically)
         (windmove-left)
         (windmove-left))
        ((eq pane-number 2)
         (delete-other-windows)
         (split-window-horizontally)
         (windmove-right)
         (balance-windows)
         (split-window-vertically)
         (windmove-left))))

(setq cjp-window-pane-count 3)

(defun cjp-window-setup-toggle ()
  "Toggle between my favourite window layouts."
  (interactive)
  (if (eq cjp-window-pane-count 3)
      (progn
        (cjp-window-setup 2)
        (setq cjp-window-pane-count 2))
    (cjp-window-setup 3)
    (setq cjp-window-pane-count 3)))

(defun cjp-get-buffer-cols-max-width ()
  "Return maximum number of columns used in the current buffer."
  (interactive)
  (let ((count 0) (num-of-lines (count-lines (point-min) (point-max))))
    (save-excursion
      (goto-char (point-min))
      (if (eq major-mode 'dired-mode)
          (forward-line))
      (while (< (line-number-at-pos) num-of-lines)
        (goto-char (point-at-eol))
        (setq count (max count (current-column)))
        (forward-line))
        (1+ count))))

(defun fix-horizontal-size-to-buffer ()
  "Resize current buffer to width of longest line."
  (interactive)
  (fix-horizontal-size (cjp-get-buffer-cols-max-width)))

(defun cjp-set-frame-uni (&optional fullp)
  "Fits current frame to external display at uni."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) 66)
  (if fullp
      (set-frame-width (selected-frame) 177)
    (set-frame-width (selected-frame) 86)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cjp-dired-directoryp ()
  "Return true if file at point in a dired buffer is a
directory."
  (interactive)
  (file-directory-p (dired-file-name-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open with external program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cjp-guess-open-file-external (program &rest args)
  "Open current file with PROGRAM.

If in a dired buffer, open file at point."
  (interactive "sProgram to open file with: ")
  ;; Send file-name(s) as a list of string(s), always
  (let ((file-list (or (and (eq major-mode 'dired-mode)
                             (or (dired-get-marked-files)
                                 (list (dired-get-file-for-visit))))
                        (and (boundp 'buffer-file-name)
                             buffer-file-name
                             (list buffer-file-name))
                        (error "No file associated with %s" (buffer-name)))))
    (apply 'cjp-open-file-external program file-list args)))

(defun cjp-open-file-external (program file-list &rest args)
  (if (null file-list)
      (error "File not associated with buffer or not found")
      (apply 'start-process (file-name-nondirectory program) nil program
         (append args file-list))))

(defun cjp-check-if-file-exists (file)
  (if (not (file-exists-p file))
      (error "File %s doesn't exist" file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ispell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq cjp-code-buffer-list '(python-mode lisp-interaction-mode emacs-lisp-mode
                                        c-mode java-mode js2-mode python-mode))

(defun cjp-ispell-guess-usage ()
  "If current buffer has major mode listed in
`cjp-code-buffer-list`, run ispell on comments and strings
only. Else, run ispell as usual."
  (interactive)
  (cond ((member major-mode cjp-code-buffer-list)
         (ispell-comments-and-strings))
        ((equal major-mode 'erc-mode)
         (save-excursion
           (ispell-region (erc-bol) (point-at-eol))))
        (t (ispell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cjp-tilde-to-longform (string &optional initial)
  "Replace INITIAL with expansion of `~' in STRING. If INITIAL
isn't given, use `~'."
  (replace-regexp-in-string (or initial "~") (expand-file-name "~") string t))

(defun cjp-search-elisp-docs (topic)
  "Search Elisp docs for TOPIC."
  (interactive "sTopic: ")
  (info "Elisp")
  (Info-virtual-index topic))

(defun cjp-copy-info-node-address ()
  "Puts '(info (File) Node)' string in kill ring to send to
others. Just `Info-copy-current-node-name' with the assumption of
`0' argument."
  (interactive)
  (unless Info-current-node
    (error "No current Info node"))
  (let ((node (if (stringp Info-current-file)
                  (concat "(" (file-name-nondirectory Info-current-file) ") "
                          Info-current-node))))
    (setq node (concat "(info \"" node "\")"))
    (unless (stringp Info-current-file)
      (setq node (format "(Info-find-node '%S '%S)"
                         Info-current-file Info-current-node)))
    (kill-new node)
    (message "%s" node)))

(defun cjp-get-directories-in (directory)
  "Return list of directories in DIRECTORY, with DIRECTORY itself as the head.

Ignore directories starting with `.'."
  (interactive)
  (if (not (file-directory-p directory))
      (error "%s isn't a directory" directory)
    (let ((dirs (directory-files directory t "^[^.]"))
          (return-dirs (list directory)))
      (while dirs
        (if (file-directory-p (car dirs))
            (add-to-list 'return-dirs (car dirs) t))
        (setq dirs (cdr dirs)))
      return-dirs)))

(defun cjp-lookup-thing-at-point (&optional arg)
  "Lookup thing at point in relevant info docs.

If given a prefix argument, lookup thing at point in all
manuals."
  (interactive "P")
  (let ((search-term (info-lookup-guess-default 'symbol major-mode)))
    (if arg
        (info-apropos search-term)
      (info-lookup-symbol search-term))))

(defun cjp-load-url-w3m ()
  "Load URL at point in w3m buffer, without a prompt."
  (interactive)
  (w3m-browse-url (thing-at-point 'url)))

(defun cjp-emacs-structure-dir (item &optional dir)
  "Return directory path of ITEM under DIR (in emacs structure) as a string.

If DIR is not passed, assume root of emacs directory. DIR should
not end with a directory separator."
  (interactive)
  (concat user-emacs-directory (if dir (concat dir "/")) item))

(defun cjp-emacs-structure-dir-map (items &optional dir)
  "Return list of directory paths of ITEMS under DIR in emacs structure.

If DIR is not passed, assume root of emacs directory. DIR should
not end with a directory separator."
  (interactive)
  (mapcar (lambda (item)
            (cjp-emacs-structure-dir item dir)) items))

(defun cjp-get-dir-structure-in (dir)
  "Return directory structure (as a list of strings) in DIR,
inside emacs directory structure.

DIR is included as the head of the list."
  (interactive)
  (cjp-get-directories-in (cjp-emacs-structure-dir dir)))

(defun cjp-find-double-words (arg)
  "Find double words in current buffer. Start from current point
unless given a prefix argument, in which case start from top.

A word is any sequence of `a-z', `A-Z' or `'' characters. Words
can be separated by one or more spaces, or a comma and spaces, or
a full stop and spaces, or any number of newlines.

If a double word set is found, move point to the end of this
set. If not, leave point in original position."
  (interactive "P")
  (let ((original-point (point)))
    (if arg (goto-char (point-min)))
    (if (re-search-forward "\\(\\b[a-zA-Z']+\\b\\)\\( +\\|, +\\|. +\\|
+\\)\\1" nil t)
        (match-beginning 1)
      (message "No double words found.")
      (goto-char original-point))))

(defun cjp-resize-window (window)
  "If more than two windows in current frame and WINDOW is one of
them, change WINDOW height to 10 lines."
  (interactive "bWindow: ")
  (save-selected-window
    (when (and (> (length (window-list)) 1)
               (get-buffer-window window))
      (switch-to-buffer-other-window window)
      (enlarge-window (- 10 (window-height))))))

(defun cjp-resize-shell-window ()
  "Resize `*shell*' window to a height of 10 lines."
  (interactive)
  (cjp-resize-window "*shell*"))

(defun cjp-occur-elisp ()
  "Show comment headings in current elisp buffer.

Assumes my formatting of elisp comments."
  (interactive)
  (occur "^;;;; \\w")
  (switch-to-buffer-other-window "*Occur*"))

(defun cjp-update-version-number (file)
  "Update version number in FILE.

Format of version number is YYYYMMDD."
  (save-window-excursion
    (find-file file)
    (goto-char (point-min))
    (re-search-forward "Version: [0-9]+")
    (replace-match (concat "Version: " (format-time-string "%Y%m%d")))
    (save-buffer)
    (kill-buffer)))

(defun cjp-toggle-dedicated-window ()
  "Toggle whether current window is dedicated to the current
buffer."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window))))
  (if (window-dedicated-p (selected-window))
      (message "Window is now dedicated.")
    (message "Window is no longer dedicated.")))

(setq cjp-last-window nil)

(defun cjp-switch-to-last-window ()
  "Switch to the last window used."
  (interactive)
  (if cjp-last-window
      (progn
        (previous-multiframe-window)
        (setq cjp-last-window nil))
    (next-multiframe-window)
    (setq cjp-last-window t)))

(defun wc ()
  "Count words in buffer using `wc'."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun cjp-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to
  NEW-NAME."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (new-name1 (or new-name (read-string "New name: " name))))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name1)
          (message "A buffer named '%s' already exists!" new-name1)
        (progn (rename-file name new-name1 t)
               (rename-buffer new-name1)
               (set-visited-file-name new-name1)
               (set-buffer-modified-p nil))))))

(defun cjp-reset-buffers (&optional kill-erc-buffers)
  "Remove all buffers that aren't `*scratch*', `*Python*',
`*eshell*', or ERC. If KILL-ERC-BUFFERS is true (or function
called interactively with a prefix argument), kill ERC buffers
too."
  (interactive "P")
  (list-buffers)
  (ibuffer-unmark-all "*")
  (ibuffer-mark-by-name-regexp (regexp-opt '("*scratch*" "*Python*"
                                             "*eshell*")))
  (if (not kill-erc-buffers) (ibuffer-mark-by-mode-regexp "ERC"))
  (ibuffer-toggle-marks)
  (ibuffer-do-delete)
  (pop-to-buffer "*scratch*")
  (delete-other-windows))

(defun cjp-browse-buffer ()
  "View the current buffer using the default web browser. Works
with bzipped HTML files too."
  (interactive)
  (let ((file (buffer-file-name)))
    (with-temp-buffer
      (insert-file-contents file)
      (browse-url-of-region (point-min) (point-max)))))

(defun capitalize-title (title)
  "Capitalize words in TITLE, lowercasing common words. The last
word is always capitalized."
  (let ((title-split (split-string title " "))
        (special-words '("a" "an" "and" "as" "but" "by" "en" "for" "if" "in"
                         "of" "on" "or" "the" "to" "via"))
        (new-title ""))
    (while title-split
      (let ((word (car title-split)))
        (if (eq (length title-split) 1)
            (setq new-title (concat new-title (capitalize word)))
          (setq new-title
                (concat new-title
                        (if (or (member word special-words)
                                (member word (mapcar 'upcase special-words)))
                            (downcase word)
                          (capitalize word)) " "))))
      (setq title-split (cdr title-split)))
    new-title))

(defun cjp-set-font-size (size &optional font allframes)
  "Set frame font (either FONT or by default `Menlo') to SIZE."
  (interactive "sFont size: ")
  (if (>= emacs-major-version 23)
      (set-frame-font (concat (or font (if macosxp "Menlo" "Monospace"))
                              "-" size) t allframes)))

(defun cjp-recompile-emacs-setup ()
  "Force recompile the Emacs setup directory."
  (interactive)
  (byte-recompile-directory (cjp-emacs-structure-dir "lisp") 0 t))

(defun cjp-scratch ()
  "Switch to the scratch buffer, or create it if it doesn't
  exist."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))
