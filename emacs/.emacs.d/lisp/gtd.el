;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GTD Library
;;;; ===========
;;;;
;;;; Functions for use with GTD methodology.
;;;;
;;;; Installation
;;;; ------------
;;;;
;;;; 1. Add file to load-path
;;;; 2. (require 'gtd)
;;;; 3. Bind some keys:
;;;;    (global-set-key (kbd "<f12>") 'gtd-inbox)
;;;;    (global-set-key (kbd "<C-f12>") 'gtd-select)
;;;;    (global-set-key (kbd "<C-M-f12>") 'gtd-functions)
;;;;
;;;; Use
;;;; ---
;;;;
;;;; 1. Use gtd-update to update action lists and quadrant files after editing
;;;;    project files
;;;; 2. Use gtd-kill-all-buffers to kill open gtd-related buffers
;;;; 3. Use gtd-weekly-review to get a temp buffer showing a checklist of items
;;;;    to review
;;;;
;;;; ---
;;;;
;;;; See chrispoole.com/emacs-gtd for more.
;;;;
;;;; Version: 20130328
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 2011--2013 Chris Poole <chris@chrispoole.com>
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along with
;;; this program.  If not, see <http://www.gnu.org/licenses/>.


(eval-when-compile
  (autoload 'ibuffer-unmark-all "ibuffer")
  (autoload 'ibuffer-do-delete "ibuffer")
  (autoload 'ibuffer-mark-by-file-name-regexp "ibuffer"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gtd-mode-map (make-sparse-keymap)
  "GTD keymap.")

(define-key gtd-mode-map (kbd "C-M-t") 'gtd-top-insert)
(define-key gtd-mode-map (kbd "C-M-b") 'gtd-bottom-insert)
(define-key gtd-mode-map (kbd "C-M-u") 'gtd-show-support-dir)
(define-key gtd-mode-map (kbd "C-M-f") 'gtd-show-file)
(define-key gtd-mode-map (kbd "C-M-p") 'gtd-show-project-file)
(define-key gtd-mode-map (kbd "M-m") 'gtd-smart-back-to-indentation)

(define-minor-mode gtd-mode
  "Toggle GTD mode." nil " GTD" gtd-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gtd-use-menu ()
  "Call to turn on the GTD menu in the global menubar."
  (easy-menu-define gtd-menu global-map "GTD menu."
    '("GTD"
      ["Inbox" gtd-inbox t]))
  (easy-menu-remove-item global-map '("menu-bar") "GTD")
  (easy-menu-add-item nil nil gtd-menu "help-menu"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gtd-no-autofill t
  "If true, set fill-column in GTD files to 1000. Prevents single
  tasks dropping into new lines.")

(defvar gtd-update-after-save t
  "If true, run `gtd-update' after saving a GTD file.")

(defvar gtd-keep-done-tasks t
  "If true, prepend completed tasks (those marked '[x]' in action
  lists) with 'DONE ' instead of removing them.")

(defvar gtd-done-append-weeknum t
  "If true, append 'DONE' to tasks with the number of the week
  the task was completed on. For example, 'DONE-W08'.")

(defvar gtd-no-truncate t
  "If true, don't truncate lines in GTD files.")

(defvar gtd-align-project-names nil
  "If true, align `[...]' project names in generated action
  files.")

(defvar gtd-auto-use-mode t
  "If true, automatically turn on GTD minor mode when visiting
  files in `gtd-dir'.")

(defvar gtd-weekly-review-text
  "- Put all loose notes and papers into the inbox\n
- Review last week's calendar
  - Add any relevant notes made to the inbox\n
- Review next week's calendar entries
  - Add actions about any arrangements or preparations required\n
- Process inbox
  - List action items, projects and someday/maybes
  - List waiting-fors and calendar events\n
- Review project lists
  - Evaluate each one, ensuring each has a next action
  - Think about further action steps to capture
  - Quickly draft any extra thoughts\n
- Review waiting-for list
  - Record any required follow-ups as next actions\n
- Review someday/maybe list
  - Remove anything that is no longer of interest
  - Move anything that's become active to the projects list\n
- File away reference notes and materials\n
- Add any projects, actions, waiting-fors, calendar
  events or someday-maybes in your head\n
- Think of any new, creative things to add to the system\n
- Shred any private paperwork no longer needed"
  "List of items to appear in the weekly review.")

(defvar gtd-dir "~/Dropbox/gtd/"
  "Location of GTD directory. Requires a trailing slash.")

(defvar gtd-dir-support-project "~/Documents/Projects/"
  "Location of GTD project support directory. Requires a trailing
  slash.")

(defvar gtd-dir-support-sm "~/Documents/Someday-maybe/"
  "Location of GTD someday/maybe support directory. Requires a
  trailing slash.")

(defvar gtd-dir-support-action "actions"
  "Directory to store files relating to single action steps,
  within `gtd-dir-support-project'.")

(defvar gtd-dir-support-misc "miscellaneous"
  "Directory to store files relating to miscellaneous
  someday/maybe items, within `gtd-dir-support-sm'.")

(defvar gtd-dir-al "action-list"
  "Name of action lists directory. No trailing slash.")

(defvar gtd-dir-project "project"
  "Name of projects directory. No trailing slash.")

(defvar gtd-dir-sm "someday-maybe"
  "Name of someday/maybe directory. No trailing slash.")

(defvar gtd-dir-goal "goal"
  "Name of goals directory. No trailing slash.")

(defvar gtd-open-extensions '("page" "txt" "mark" "md" "css" "htm" "html" "js")
  "List of file extensions that can be opened in Emacs directly
  when running `gtd-show-file'.")

;;; Regexen

(defvar gtd-re-action "[-a-zA-Z0-9]+"
  "Regular expression matching context names.")

(defvar gtd-re-action-line (concat "[-\\*] @\\(" gtd-re-action "\\) \\(\.+\\)")
  "Regular expression to match @context lines in project
  files. The first group matches 'context' while the second group
  matches the task text.")

(defvar gtd-re-next-action (concat "[-\\*] @" gtd-re-action " ")
  "Regular expression to match the @context line in project
  files, in order to remove that line (with
  `gtd-update-project-files').")

(defvar gtd-re-following-action (concat "[-\\*] @@\\(" gtd-re-action " \\)")
  "Regular expression to match the following action (tagged like
  @@context) in project files, so it can become the next
  action.")

(defvar gtd-re-completed-tasks "^[-\\*] \\[[xX]\\] \\(\.+?\\)\\s-+\\[\\(\.+\\)\\]$"
  "Regular expression to match completed tasks (those marked by
  `[x]') in generated action lists. The first group matches the
  task text, and the second group the file that it came from.")

(defvar gtd-re-file "#f:\\(\.+?\\)\\(\\s-+\\[\\(\.+\\)\\]\\)?$"
  "Regular expression matching `#f:' syntax. The first group
  matches the filename, while the third group matches the project
  name.")

(defvar gtd-re-align "^[-\\*] \\[[xX]?\\]\\(\.+?\\)\\(\\s-+\\)\\[\\(\.+?\\)\\]$"
  "Regular expression to align project names in generated action
  files. Group one matches the action itself. Group two is the
  column to be aligned. Group three is used in
  `gtd-show-support-dir', matching the project name. Alignment in
  action lists only performed if `gtd-align-project-names' is
  true.")

(defvar gtd-re-project-title "^#+ \\(\.+\\)?[Pp]rojects?"
  "Regular expression matching `## Projects' title in project
  list or someday/maybe list.")

(defvar gtd-re-nodot "^[^#\\.].*[^~#]$"
  "Regular expression matching anything that doesn't start with a
  `.'. Used when finding names of files and directories that
  aren't hidden.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gtd-location (&rest paths)
  "Return path to PATHS under GTD directory, using `gtd-dir'."
  (expand-file-name (concat gtd-dir (mapconcat 'identity paths "/"))))

(defun gtd-location-support (type &rest paths)
  "Return path to PATHS under GTD support directory. TYPE being
  either \"project\" or \"sm\"."
  (expand-file-name (concat (if (equal type "project")
                                gtd-dir-support-project
                              gtd-dir-support-sm)
                            (mapconcat 'identity paths "/"))))

(defun gtd-location-proj (&rest paths)
  "Return path to PATHS under GTD support directory, using
`gtd-dir-support-project'. Wrapper to `gtd-location-support'."
  (apply 'gtd-location-support "project" paths))

(defun gtd-location-sm (&rest paths)
  "Return path to PATHS under GTD support directory, using
`gtd-dir-support-sm'. Wrapper to `gtd-location-support'."
  (apply 'gtd-location-support "sm" paths))

(defun gtd-functions ()
  "Use `smex' to present a list of available GTD functions,
sorted by most common use. If `smex' isn't available, use `ido'."
  (interactive)
  (cond ((fboundp 'smex)
         (smex-read-and-run smex-ido-cache "gtd-"))
        ((fboundp 'ido-completing-read)
         (call-interactively
          (intern (ido-completing-read
                   "GTD: " (all-completions "gtd-" obarray 'commandp)))))
        (t (error "smex or ido required"))))

(defun gtd-open-file (&rest paths)
  "Open tasks file, ready to enter a new item."
  (let ((already-inboxp (equal (buffer-name) "inbox")))
    (find-file (apply 'gtd-location paths))
    (goto-char (point-min))
    (when (and (equal (car paths) "inbox")
               (eq (length paths) 1)
               already-inboxp)
      (goto-char (point-max))
      (if (and (eq (point-at-bol) (point))
               (> (point) 1))
          (backward-char))
      (when (not (eq (- (point) (point-at-bol)) 2))
        (if (> (point) 1) (newline))
        (insert "- ")))))

(defun gtd-init ()
  "Creates GTD directories if they don't already exist. In
`gtd-dir', `gtd-dir-al', `gtd-dir-project', `gtd-dir-sm', and
`gtd-dir-goal'. In `gtd-dir-support-project',
`gtd-dir-support-action'. In `gtd-dir-support-sm',
`gtd-dir-support-misc'. Also creates inbox and outbox files."
  (interactive)
  (mapc (lambda (dir)
          (if (not (file-directory-p dir))
              (make-directory dir t)))
        (append (mapcar (lambda (dir)
          (gtd-location dir)) (list gtd-dir-al gtd-dir-project
                                    gtd-dir-sm gtd-dir-goal))
                (list (concat gtd-dir-support-sm gtd-dir-support-misc)
                      (concat gtd-dir-support-project
                              gtd-dir-support-action))))
  (mapc (lambda (file)
          (if (not (file-regular-p (gtd-location (car file))))
              (with-temp-buffer
                (write-region (cadr file) nil (gtd-location (car file))))))
        `(("inbox" "- ") ("outbox" "- ")
          (,(concat gtd-dir-project "/list")
           "## Projects\n\n\n\n## Actions\n\n")
          (,(concat gtd-dir-sm "/list")
           "## Potential projects\n\n\n\n## Collections\n\n\n\n
## Miscellaneous\n\n"))))

(defun gtd-choose-open-file (dir)
  "Open a particular GTD file in DIR using ido."
  (gtd-open-file dir (gtd-completing dir)))

(defun gtd-completing (prompt &optional directory)
  "Present ido-style menu of files at PROMPT to choose
from (guessing directory from PROMPT).

If DIRECTORY is given, search for files here instead."
  (ido-completing-read (concat (capitalize prompt) ": ")
                       (directory-files (gtd-location (or directory prompt))
                                        nil gtd-re-nodot)))

(defun gtd-get-regular-files-in (directory)
  "Return a list of all regular files in DIRECTORY, including
subdirectories."
  (let ((dir-files (directory-files directory t gtd-re-nodot))
        (files '()))
    (while dir-files
      (setq files
            (append (cond ((file-regular-p (car dir-files))
                           (list (car dir-files)))
                          ((file-directory-p (car dir-files))
                           (gtd-get-regular-files-in (car dir-files)))) files))
      (setq dir-files (cdr dir-files))) files))

(defun gtd-delete-line ()
  "Delete the current line, including the trailing newline. Thus,
it's equivalent to `kill-whole-line', without storing the line in
the kill ring."
  (delete-region (point-at-bol) (point-at-eol))
  (delete-char 1))

(defun gtd-projects-list ()
  "Open GTD projects list."
  (gtd-open-file gtd-dir-project "list"))

(defun gtd-someday-maybe-list ()
  "Open GTD someday/maybe list."
  (gtd-open-file gtd-dir-sm "list"))

(defun gtd-project ()
  "Open a particular GTD project file (using ido)."
  (gtd-choose-open-file gtd-dir-project))

(defun gtd-goal ()
  "Open a particular GTD goal file (using ido)."
  (gtd-choose-open-file gtd-dir-goal))

(defun gtd-someday-maybe ()
  "Open a particular GTD goal file (using ido)."
  (gtd-choose-open-file gtd-dir-sm))

(defun gtd-action-list ()
  "Open a particular GTD action file."
  (interactive)
  (let* ((context (gtd-completing gtd-dir-al))
         (full-context (gtd-location gtd-dir-al context)))
    (if (file-regular-p full-context)
        (gtd-open-file gtd-dir-al context)
      (error "Not a regular file"))))

(defun gtd-clean-project-name (project-name)
  "Return PROJECT-NAAME with `/' turned into `_'. Currently does
nothing, just returns PROJECT-NAME."
  (replace-regexp-in-string "[/:]" "_" project-name)
  ;; project-name
  )

(defun gtd-get-project-name ()
  "Return cleaned project name on current line."
  (save-excursion
    (if (equal (file-name-nondirectory (buffer-file-name)) "list")
        (progn (goto-char (point-at-bol))
               (if (re-search-forward "^[-\\*] \\(\.+\\)$" (point-at-eol) t)
                   (gtd-clean-project-name (match-string 1))))
      (file-name-nondirectory (buffer-file-name)))))

(defun gtd-ask-project-name ()
  "Either get the project name on the current line or prompt for
it."
  (let ((project-name (gtd-get-project-name)))
    (or (if project-name (list project-name t))
        (list (read-string "Project name: ") nil))))

(defun gtd-create-project ()
  "Create project support directory in `gtd-dir-support-project'
as well as project file in GTD project directory (using `gtd-dir'
and `gtd-dir-project'). Doesn't work with someday/maybe list."
  (interactive)
  (let* ((ask-project-name (gtd-ask-project-name))
         (project-name (car ask-project-name))
         (line-exists (cadr ask-project-name))
         (project-support-dir (gtd-location-proj project-name))
         (project-file (gtd-location gtd-dir-project project-name)))
    (if (not (file-directory-p project-support-dir))
        (make-directory project-support-dir t))
    (when (not (file-regular-p project-file))
      (find-file project-file)
      (set-buffer-modified-p t)
      (save-buffer)
      (kill-buffer))
    (if (not line-exists)
        (insert (concat "- " project-name)))
    (gtd-sort-projects)
    (goto-char (point-min))
    (re-search-forward (regexp-quote (car ask-project-name)) nil t)
    (goto-char (point-at-bol))))

(defun gtd-delete-project ()
  "Confirm and delete project support directory and project
file. See `gtd-create-project' for more."
  (interactive)
  (let* ((projectp (string-match (gtd-location gtd-dir-project)
                                   (buffer-file-name)))
         (ask-project-name (gtd-ask-project-name))
         (project-name (car ask-project-name))
         (line-exists (cadr ask-project-name))
         (project-support-dir (gtd-location-support (if projectp
                                   "project" "sm") project-name))
         (project-file (gtd-location (if projectp
                                         gtd-dir-project
                                       gtd-dir-sm) project-name)))
    (when (yes-or-no-p (concat "Delete \"" project-name "\"? "))
      (if (file-directory-p project-support-dir)
          (delete-directory project-support-dir t))
      (if (file-regular-p project-file)
          (delete-file project-file))
      (if line-exists
        (gtd-delete-line))
      (save-buffer))))

(defun gtd-move-project ()
  "Move a project and its associated support directory and file
from the projects list to the someday/maybe list, or vice
versa. Assumes that point is on a line in the project list or
someday/maybe list with a project name."
  (interactive)
  (save-window-excursion
    (let* ((project-name (gtd-get-project-name))
           (project-support-dir (gtd-location-proj project-name))
           (sm-support-dir (gtd-location-sm project-name))
           (project-file (gtd-location gtd-dir-project project-name))
           (sm-file (gtd-location gtd-dir-sm project-name))
           (projectp (string-match (gtd-location gtd-dir-project)
                                     (buffer-file-name)))
           (current-buffers (remove nil
                                    (mapcar 'buffer-file-name (buffer-list)))))
      (gtd-delete-line)
      (save-buffer)
      (find-file (gtd-location (if projectp
                                   gtd-dir-sm gtd-dir-project) "list"))
      (goto-char (point-min))
      (re-search-forward gtd-re-project-title nil t)
      (forward-line 2)
      (insert (concat "- " project-name "\n"))
      (save-buffer)
      (if (not (member (buffer-file-name) current-buffers))
          (kill-buffer)
        (revert-buffer t t)
        (goto-char (point-min)))
      (if projectp
          (progn (if (file-regular-p project-file)
                     (rename-file project-file sm-file))
                 (if (file-directory-p project-support-dir)
                     (rename-file project-support-dir sm-support-dir)))
        (if (file-regular-p sm-file)
            (rename-file sm-file project-file))
        (if (file-directory-p sm-support-dir)
            (rename-file sm-support-dir project-support-dir))))))

(defun gtd-rename-project ()
  "Rename the project on the current line, including its support
directory if it has one. Doesn't work with someday/maybe list."
  (interactive)
  (let* ((project-name (gtd-get-project-name))
         (project-file (gtd-location gtd-dir-project project-name))
         (project-support-dir (gtd-location-proj project-name))
         (new-name (read-string "New name: " project-name))
         (new-name-clean (gtd-clean-project-name new-name))
         (new-file (gtd-location gtd-dir-project new-name-clean))
         (new-support-dir (gtd-location-proj new-name-clean)))
    (if (not (equal new-name ""))
        (progn (goto-char (point-at-bol))
               (delete-region (point-at-bol) (point-at-eol))
               (insert (concat "- " new-name))
               (save-buffer)
               (if (file-regular-p project-file)
                   (rename-file project-file new-file))
               (if (file-directory-p project-support-dir)
                   (rename-file project-support-dir new-support-dir)))
      (error (concat "Project \"" project-name "\" has no file"))
      (error "New project name required"))))

(defun gtd-select ()
  "Choose and open a file in GTD system."
  (interactive)
  (let ((choice (ido-completing-read
                 "GTD file: "
                 (append (directory-files (gtd-location) nil gtd-re-nodot)
                         '("projects-list" "someday-maybe-list")))))
    (if (file-regular-p (gtd-location choice))
        (gtd-open-file choice)
      (funcall (intern (concat "gtd-" choice))))))

(defun gtd-kill-all-buffers ()
  "Kill all GTD-related buffers."
  (interactive)
  (if (fboundp 'ibuffer)
      (progn
        (list-buffers)
        (ibuffer-unmark-all "*")
        (ibuffer-mark-by-file-name-regexp (regexp-quote (gtd-location)))
        (ibuffer-mark-by-file-name-regexp (regexp-quote gtd-dir-support-sm))
        (ibuffer-mark-by-file-name-regexp (regexp-quote
                                           gtd-dir-support-project))
        (ibuffer-do-delete)
        (kill-buffer "*Ibuffer*"))
    (error "ibuffer required to kill all GTD buffers")))

(defun gtd-update ()
  "Check completed tasks, and generate GTD action lists."
  (interactive)
  (gtd-update-project-files)
  (gtd-generate-actions)
  ;; (mapc 'gtd-concat-quadrants gtd-quad-list)
  )

(defun gtd-search ()
  "Search all GTD-related files for a regex."
  (interactive)
  (dired (gtd-location))
  (dired-maybe-insert-subdir (gtd-location gtd-dir-project))
  (dired-maybe-insert-subdir (gtd-location gtd-dir-sm))
  (dired-mark-files-containing-regexp (read-string "Regex: ")))

(defun gtd-get-filename (&optional fullpath)
  "Add filename of current buffer (or file(s) in dired) to kill
ring, to yank into a GTD project file for linking.

Use a prefix argument when called interactively (or set FULLPATH
to true) to get filenames with absolute paths."
  (interactive "P")
  (let* ((items (or (and (eq major-mode 'dired-mode)
                         (or (dired-get-marked-files)
                             (list (dired-get-file-for-visit))))
                    (and buffer-file-name
                         (list buffer-file-name))
                    (error "No file associated with buffer")))
         (output-string (mapconcat (lambda (item)
                                     (concat "#f:"
                                             (if fullpath item
                                               (file-name-nondirectory item))))
                                   items "\n")))
    (if (kill-new output-string)
        (message (concat "Added filename" (and (> (length items) 1) "s")
                         " to kill ring.")))))

(defun gtd-show-file (&optional force)
  "When on a line in a GTD file with '#f:...' notation, show this
file or directory in a dired buffer.

The file will only be opened in Emacs if it's found, unless FORCE
is true (using a prefix argument interactively).

If the file's extension matches one in `gtd-open-extensions',
open it directly."
  (interactive "P")
  (save-excursion
    (goto-char (point-at-bol))
    (if (re-search-forward gtd-re-file (point-at-eol) t)
        (let* ((file (match-string 1))
               (project-name (or (match-string 3)
                                 (file-name-nondirectory
                                  (buffer-file-name))))
               (gtdlistp (equal project-name "list"))
               (projectp (string-match (gtd-location gtd-dir-project)
                                       (buffer-file-name)))
               (actionp (string-match (gtd-location gtd-dir-al)
                                      (buffer-file-name)))
               (somedayp (string-match (gtd-location gtd-dir-sm)
                                       (buffer-file-name)))
               (project-dir-name (cond ((and gtdlistp (not somedayp))
                                        gtd-dir-support-action)
                                       ((and gtdlistp somedayp)
                                        gtd-dir-support-misc)
                                       (t project-name)))
               (full-file (if (string-match (expand-file-name "~/") file)
                              file
                            (gtd-location-support
                             (if (or actionp projectp) "project" "sm")
                             project-dir-name file))))
          (when (file-directory-p full-file)
            (dired full-file))
          (when (or force (file-regular-p full-file))
            (if (member (file-name-extension full-file)
                        gtd-open-extensions)
                (find-file full-file)
              (dired (file-name-directory full-file))
              (dired-goto-file full-file)))))))

(defun gtd-show-project-file ()
  "If on a line with a project name (in the someday/maybe or
project list), or in an action list, open the associated project
file. Jump to the relevant line if possible."
  (interactive)
  (goto-char (point-at-bol))
  (let* ((item "")
         (file (if (re-search-forward gtd-re-align (point-at-eol) t)
                   (progn (setq item (match-string 1))
                          (gtd-location gtd-dir-project (match-string 3)))
                 (gtd-get-project-name))))
    (when (file-regular-p file)
      (find-file file)
      (goto-char (point-min))
      (re-search-forward (regexp-quote item) nil t)
      (goto-char (point-at-bol)))))

(defun gtd-show-support-dir ()
  "Show dired buffer of the project at point's support
directory. Works inside a project file, as well as someday/maybe
files."
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (re-search-forward gtd-re-align (point-at-eol) t)
    (let* ((project-name (or (match-string 3) (gtd-get-project-name)))
           (gtdlistp (equal project-name "list"))
           (projectp (string-match (gtd-location gtd-dir-project)
                                   (buffer-file-name)))
           (actionp (string-match (gtd-location gtd-dir-al)
                                  (buffer-file-name)))
           (somedayp (string-match (gtd-location gtd-dir-sm)
                                   (buffer-file-name)))
           (project-dir-name (cond ((and gtdlistp (not somedayp))
                                    gtd-dir-support-action)
                                   ((and gtdlistp somedayp)
                                    gtd-dir-support-misc)
                                   (t project-name)))
           (support-dir (gtd-location-support (if (or actionp projectp)
                                                  "project" "sm")
                                              project-dir-name)))
      (if (file-directory-p support-dir)
          (dired support-dir)
        (error (concat "Project support directory \""
                       support-dir "\" doesn't exist"))))))

(defun gtd-inbox ()
  "Open GTD inbox."
  (interactive)
  (gtd-open-file "inbox"))

(defun gtd-weekly-review ()
  "Create an unsaved file in `gtd-dir' with a list of items to do
  during the weekly review."
  (interactive)
  (find-file (gtd-location "*gtd-weekly-review*"))
  (delete-region (point-min) (point-max))
  (insert gtd-weekly-review-text)
  (goto-char (point-min)))

(defun gtd-top-insert ()
  "Move to top of buffer and insert `- @' to start a new list item.

If in project list, instead prompt for and create a new project,
inserting its name into the buffer at the correct point."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward gtd-re-project-title nil t)
      (progn (forward-line 2)
             (open-line 1)
             (gtd-create-project))
    (open-line 1)
    (insert "- @")))

(defun gtd-bottom-insert ()
  "Move to bottom of buffer and insert `- ' to start a new list item.

If in project list, insert `@' too."
  (interactive)
  (goto-char (point-max))
  (if (not (eq (point) (point-at-bol)))
      (newline))
  (insert "- ")
  (if (string-match (gtd-location gtd-dir-project) (buffer-file-name))
      (insert "@")))

(defun gtd-smart-back-to-indentation ()
  "Calls `back-to-indentation' unless on a line with a
checkbox (`- []') or list (`- '), where it places point inside
the brackets or to the right of the first space after the hyphen,
respectively."
  (interactive)
  (cond ((string-match "^[-\\*] \\[\\]" (buffer-substring (point-at-bol)
                                                     (point-at-eol)))
         (goto-char (point-at-bol))
         (forward-char 3))
        ((string-match "^ *- " (buffer-substring (point-at-bol)
                                                   (point-at-eol)))
         (goto-char (point-at-bol))
         (re-search-forward "^ *- @*" (point-at-eol) t))
        (t (back-to-indentation))))

(defun gtd-sort-projects ()
  "Sort lines alphabetically between `# Projects' and the next
heading in the project or someday/maybe list."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((begin (progn (re-search-forward gtd-re-project-title nil t)
                        (forward-line 2)
                        (point)))
          (end (progn (re-search-forward "^#+" nil t)
                      (forward-line -1)
                      (point))))
      (sort-lines nil begin end)
      (save-buffer))))

;;; Removed covey quadrant code, as not using it anymore. Keeping around in case
;;; it's useful later.
;; (defun gtd-concat-quadrants (quad)
;;   "Concatenate all Covey quadrants for each action context, and
;; save in file QUAD."
;;   (save-window-excursion
;;     (let ((contexts (remove nil (mapcar (lambda (file)
;;                                           (if (file-directory-p file) file))
;;                                         (directory-files
;;                                          (gtd-location gtd-dir-al)
;;                                          t gtd-re-nodot))))
;;           (collection '())
;;           quadfile
;;           (current-buffers (remove nil
;;                                    (mapcar 'buffer-file-name (buffer-list)))))
;;       (while contexts
;;         (setq quadfile (concat (car contexts) "/" quad))
;;         (if (file-exists-p quadfile)
;;             (setq collection
;;                   (append
;;                    (list (cons (file-name-nondirectory (car contexts))
;;                                quadfile))
;;                    collection)))
;;         (setq contexts (cdr contexts)))
;;       (find-file (gtd-location quad))
;;       (delete-region (point-min) (point-max))
;;       (mapc (lambda (file)
;;               (insert (concat "## " (car file) "\n\n"))
;;               (insert-file-contents (cdr file))
;;               (goto-char (point-max))
;;               (insert "\n")) collection)
;;       (save-buffer)
;;       (if (not (member (buffer-file-name) current-buffers))
;;           (kill-buffer)
;;         (revert-buffer t t)
;;         (goto-char (point-min))))))

(defun gtd-generate-actions ()
  "Traverse each project file, picking up the first (next) action
and adding it to the relevant action file. Removes action list
directory first."
  ;; Remove generated content
  (save-window-excursion
    (if (file-directory-p (gtd-location gtd-dir-al))
        (delete-directory (gtd-location gtd-dir-al) t))
    (make-directory (gtd-location gtd-dir-al) t)
    ;; Turn @action lines of each project into a hashmap
    (let ((projects (directory-files (gtd-location gtd-dir-project)
                                     t gtd-re-nodot))
          (actions (make-hash-table :test 'equal))
          (current-buffers (remove nil
                                   (mapcar 'buffer-file-name (buffer-list)))))
      (while projects
        (with-temp-buffer
          (insert-file-contents (car projects))
          (goto-char (point-min))
          (while (re-search-forward gtd-re-action-line nil t)
            (save-excursion
              (puthash (match-string 1)
                       (concat (gethash (match-string 1) actions) "- [] "
                               (match-string 2) "  ["
                               (file-name-nondirectory (car projects)) "]\n")
                       actions))))
        (setq projects (cdr projects)))
      ;; Write each action file, (including @waiting)
      (maphash (lambda (action items)
                 (find-file (gtd-location gtd-dir-al action))
                 (delete-region (point-min) (point-max))
                 (insert items)
                 (if gtd-align-project-names
                     (align-regexp (point-min) (point-max) gtd-re-align 2 2 t))
                 (save-buffer)
                 (if (not (member (buffer-file-name) current-buffers))
                     (kill-buffer)
                   (revert-buffer t t)
                   (goto-char (point-min)))) actions)
      ;; If waiting file exists (for example), and buffer is visible, and then
      ;; all items are ticked off such that on the next run of
      ;; gtd-generate-actions the file is erased, since there are no more
      ;; @waiting lines to find in project files, the above kill-buffer function
      ;; won't run (since it won't have created a new waiting file). The result
      ;; is that the waiting buffer is left, without an associated file. To
      ;; remove, find all buffers currently open that are in GTD directory
      ;; structure, and if they match the conditions:
      (mapc (lambda (buffer)
              (if (and (string-match (gtd-location) buffer)
                       (not (file-exists-p buffer))
                       (not (buffer-modified-p (get-file-buffer buffer))))
                  (kill-buffer (get-file-buffer buffer))))
            current-buffers))))

(defun gtd-update-project-files ()
  "Look though all generated action files and find completed
  actions. Remove these from their related project list."
  (save-window-excursion
    (let ((generated-action-files (gtd-get-regular-files-in
                                   (gtd-location gtd-dir-al)))
          (project (make-hash-table :test 'equal))
          (current-buffers (remove nil
                                   (mapcar 'buffer-file-name (buffer-list)))))
      (while generated-action-files
        (with-temp-buffer
          (insert-file-contents (car generated-action-files))
          (goto-char (point-min))
          (while (re-search-forward gtd-re-completed-tasks nil t)
            (puthash (match-string 2) (concat (gethash (match-string 2) project)
                                              (match-string 1)
                                              "\n") project)))
        (setq generated-action-files (cdr generated-action-files)))
      (maphash (lambda (project-files items)
                 (let ((split-items (remove "" (split-string items "\n")))
                       (project-file (gtd-location gtd-dir-project
                                                   project-files)))
                   (when (file-regular-p project-file)
                     (find-file project-file)
                     (goto-char (point-min))
                     (while split-items
                       (if (re-search-forward
                            (concat gtd-re-next-action "\\("
                                    (regexp-quote (car split-items)) "\\)$")
                            nil t 1)
                           (if gtd-keep-done-tasks
                               (replace-match (concat "- DONE"
                                                      (if gtd-done-append-weeknum
                                                          (format-time-string "-W%W"))
                                                      " \\1"))
                             ;; kill-whole-line, without using kill ring
                             (gtd-delete-line)))
                       ;; After removing the completed action step, find the
                       ;; following @@action step and turn it into the next
                       ;; @action. Only do this if there's no @action step in
                       ;; between. (This could happen if there's one file with
                       ;; several separate mini projects.)
                       (let ((current-point (point)))
                         (if (and (re-search-forward gtd-re-following-action
                                                     nil t 1)
                                  (not (string-match
                                        gtd-re-next-action
                                        (buffer-substring current-point
                                                          (point)))))
                             (replace-match "- @\\1")))
                       (setq split-items (cdr split-items)))
                     (save-buffer)
                     (if (not (member (buffer-file-name) current-buffers))
                         (kill-buffer)
                       (revert-buffer t t)
                       (goto-char (point-min)))))) project))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-save-hook
          (lambda () (if (and (string-match (gtd-location) buffer-file-name)
                         gtd-update-after-save)
                    (gtd-update))))

(add-hook 'find-file-hook
          (lambda () (when (string-match (gtd-location) buffer-file-name)
                  (when gtd-auto-use-mode
                    (if (fboundp 'markdown-mode) (markdown-mode))
                    (gtd-mode 1)
                    (auto-revert-mode 1))
                  (if gtd-no-autofill
                      (setq fill-column 1000))
                  (when gtd-no-truncate
                    (setq truncate-lines t)
                    (set (make-local-variable
                          'truncate-partial-width-windows) nil)
                    (set (make-local-variable
                          'sort-fold-case) t)))))



(provide 'gtd)
