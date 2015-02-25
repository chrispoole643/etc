;;; org-gtd.el --- An implementation of GTD with org-mode.

;; Version: 1.
;; Author: Chris Poole <chris@chrispoole.com>
;; Url: https://github.com/chrispoole643/org-gtd

;; Copyright (c) 2014 Chris Poole

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'org)
(require 'org-attach)


;;; Variables

(defvar gtd-agenda-use-full-frame-p t
  "When true, if using a multi-frame Emacs environment, have the
  agenda delete other windows when being displayed.")

;; Define file locations
(setq org-default-notes-file (concat org-directory "inbox.org")
      gtd-projects-file (concat org-directory "projects.org")
      gtd-actions-file (concat org-directory "actions.org")
      gtd-action-lists-dir (concat org-directory "action-lists/")
      gtd-someday-maybe-file (concat org-directory "someday-maybe.org")
      gtd-reference-file (concat org-directory "reference.org")
      gtd-calendar-file (concat org-directory "calendar.org")
      gtd-goals-file (concat org-directory "goals.org"))

;; Setup org mode
(setq ;; Show upcoming deadlines 3 days prior
      org-deadline-warning-days 3
      ;; Hit RET on a link to follow it
      org-return-follows-link t
      ;; Expand headlines to CONTENT or OVERVIEW or LOGDONE
      org-startup-folded "overview"
      ;; Pretty colours in code blocks
      org-src-fontify-natively t
      ;; Indent Headings and hide stars
      org-startup-indented t
      ;; Show inline images
      org-startup-with-inline-images t
      ;; Disable ido and completing in steps for performance and Helm use
      org-completion-use-ido nil
      org-outline-path-complete-in-steps nil
      ;; Split line in the middle with M-RET
      org-M-RET-may-split-line t
      ;; Quickly select TODO states
      org-use-fast-todo-selection t
      ;; Only mark a parent DONE when all children are too
      org-enforce-todo-dependencies t
      ;; Don't log when I complete a repeating event
      org-log-repeat nil
      ;; Same as above, with checkboxes
      org-enforce-todo-checkbox-dependencies t
      ;; When choosing tags, assume one tag per entry, and don't even show the
      ;; temp buffer listing the tags --- hit C-c again to override
      org-fast-tag-selection-single-key t
      ;; Change TODO state using TAG interface
      org-fast-tag-selection-include-todo t
      ;; Don't show the postamble in exported docs
      org-export-html-postamble nil
      ;; Use priority to mark Urgent tasks
      org-default-priority ?B
      org-lowest-priority ?C
      org-highest-priority ?A
      org-priority-start-cycle-with-default nil
      ;; Define stuck projects as level 2 items that aren't a DONE or NEXT
      ;; action, don't have NEXT actions inside them, and don't have items
      ;; tagged as waiting.
      org-stuck-projects '("+LEVEL=2/-DONE-NEXT-DEFER" ("NEXT") ("waiting") "SCHEDULED")
      ;; In column format, I'm only interested in the task and its priority
      org-columns-default-format "%80ITEM(Task){X/} %1PRIORITY(P) %0TODO %0TAGS"
      ;; Change sublist bullet types
      org-list-demote-modify-bullet t)

;; Standard keywords
(setq org-todo-keywords '((sequence "DEFER(f)" "NEXT(n)" "|" "DONE(d)" "CANCEL(c)")))


;;; Functions

(defun gtd-open-file ()
  "Open a GTD org-mode file (using Helm system)."
  (interactive)
  (find-file (concat org-directory
                     (funcall (if (fboundp 'helm-comp-read)
                                  'completing-read 'helm-comp-read)
                              "GTD file: " (directory-files org-directory
                                                            nil "\.org$")))))

(defun gtd-construct-todo-regex ()
  "Construct a regular expression matching any of the todo states
in `org-todo-keywords'."
  (mapconcat (lambda (keyword)
               (string-match "\\([A-Za-z]+\\)" keyword)
               (match-string 0 keyword))
             (delete "|" (cdar org-todo-keywords)) "\\|"))

;;; TODO fix this function --- causes DONE tasks to appear in agenda again
(defun gtd-mark-completed-exported-tasks-as-done ()
  "Find completed entries in exported action lists (e.g., those
marked \"[x]\"), and mark them \"DONE\" in the originating agenda
file."
  (interactive)
  ;; done-tasks becomes a list of paired values, title & tag.
  ;; For example, ("book an appointment" "phone").
  (let ((done-tasks '()))
    ;; Find the completed tasks
    (mapc (lambda (file)
            (when (file-regular-p file)
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (while (re-search-forward (concat "\\[ *[Xx] *\\] \\("
                                                  (gtd-construct-todo-regex)
                                                  "\\) \\(.+\\)")
                                          (point-max) t)
                  (setq done-tasks (append (list (file-name-base file)) done-tasks))
                  (setq done-tasks (append (list (match-string 2)) done-tasks))))))
          (directory-files gtd-action-lists-dir t))
    ;; Look through the agenda files for each completed task, and mark them done
    (while (> (length done-tasks) 0)
      (let ((title (car done-tasks))
            (tag (cadr done-tasks)))
        (org-map-entries (lambda ()
                           (when (equal title (org-get-heading t t))
                             (org-entry-put (point) "TODO" "DONE")))
                         tag 'agenda))
      (setq done-tasks (cddr done-tasks)))))

(defun gtd-export-agendas-and-calendar ()
  "Mark any completed tasks in exported action lists as \"DONE\",
  before generating new action lists. Export scheduled tasks or
  those with set deadlines (that aren't \"DONE\") to an iCalendar
  file too."
  (interactive)
  ;(gtd-mark-completed-exported-tasks-as-done)
  (org-store-agenda-views)
  ;; Iterate through every headline in the agenda files, looking for not-DONE tasks that
  ;; are scheduled or have deadlines, storing their starting character position if found.
  (let ((calendar-hash (make-hash-table :test 'equal))
        (calendar-items nil))
    (org-map-entries (lambda ()
                       (let ((scheduledp (org-get-scheduled-time (point) nil))
                             (deadlinep (org-get-deadline-time (point) nil))
                             (notdonep (not (equal "DONE" (org-get-todo-state))))
                             (filename (org-entry-get (point) "FILE")))
                         (when (and (or scheduledp
                                        deadlinep)
                                    notdonep)
                           (puthash filename (cons (point) (gethash filename calendar-hash))
                                    calendar-hash))))
                     nil 'agenda)
    ;; Turn the hash into an alist
    (maphash (lambda (key value)
               (add-to-list 'calendar-items (cons key value)))
             calendar-hash)
    ;; Build iCalendar export file, restricting the items to only those just
    ;; found. `calendar-items' is an alist where key is a file name and value a list of
    ;; buffer positions pointing to entries that should appear in the calendar.
    (apply 'org-icalendar--combine-files calendar-items (org-agenda-files t)))
  (org-save-all-org-buffers))

;;; This is another way of choosing scheduled or deadlined tasks to export to
;;; iCalendar. It should be used as:

;;; `(let ((org-export-filter-parse-tree-functions '(gtd-filter-scheduled-todo-tasks)))
;;;   (org-icalendar-combine-agenda-files))'.

;;; The idea is to find the scheduled tasks, and ignore the others. However, if a
;;; scheduled task is a subtask, where the parent task isn't scheduled, then the parent
;;; tree will be ignored, so the calendar will always be empty. My idea was to find the
;;; scheduled items, and pull those headlines out of the tree, to be appended to `data',
;;; such that the scheduled task will still show. I ended up using the alternative, which
;;; is to find the positions in all agenda buffers of scheduled tasks, and pass that to
;;; `org-icalendar--combine-files' (which uses them as a restriction).

;; (defun gtd-filter-scheduled-todo-tasks (data backend info)
;;   "Filter iCalendar export to include only TODO tasks that are
;; not done, but which are scheduled or have a deadline.

;; Aim: ignore items that are NOT the main section (like \"Actions\"), and
;; NOT scheduled (or deadlined) tasks that aren't done"
;;   (when (eq backend 'icalendar)
;;     (let ((new-data nil))
;;       (org-element-map data 'headline
;;         (lambda (hl)
;;           (let ((title (org-element-property :raw-value hl))
;;                 (notrootp (> (org-element-property :level hl) 1))
;;                 (scheduledp (org-element-property :scheduled hl))
;;                 (deadlinep (org-element-property :deadline hl))
;;                 (todop (equal 'todo (org-element-property :todo-type hl)))
;;                 (notdonep (not (equal "DONE" (org-element-property :todo-keyword hl)))))
;;             (when (and todop
;;                        notdonep
;;                        notrootp
;;                        (or scheduledp
;;                            deadlinep))
;;               ;; (setq new-data (append (list hl) new-data))
;;               ))))
;;       ;; (setq new-data (append (list 'nil) new-data))
;;       ;; (setq new-data (append (list 'org-data) new-data))
;;       (type-of data)
;;       ;(type-of new-data)
;;       data)))


;;; Capture

(setq org-capture-templates
      '(("i" "Inbox" entry (file+headline "" "Inbox")
         "* %?")
        ("p" "Project" entry (file+headline gtd-projects-file "Projects")
         "* %?\n:PROPERTIES:\n:ATTACH_DIR_INHERIT: t\n:END:")
        ("a" "Action" entry (file+headline gtd-actions-file "Actions")
         "* NEXT %?")
        ("h" "Home action" entry (file+headline gtd-actions-file "Actions")
         "* NEXT %? :home:")
        ("w" "Waiting action" entry (file+headline gtd-actions-file "Actions")
         "* DEFER %? :waiting:")
        ("o" "Office action" entry (file+headline gtd-actions-file "Actions")
         "* NEXT %? :office:")
        ("l" "Laptop action" entry (file+headline gtd-actions-file "Actions")
         "* NEXT %? :laptop:")
        ("r" "Recurring calendar entry" entry (file+headline gtd-calendar-file "Miscellaneous recurring events")
         "* DEFER %?")))


;;; Agenda

(setq ;; Show next x days in agenda
      org-agenda-span 3
      ;; Show dates even if totally free
      org-agenda-show-all-dates nil
      ;; Don't show things already done
      org-agenda-skip-deadline-if-done t
      ;; Don't show things already done
      org-agenda-skip-scheduled-if-done t
      ;; By default set events that only have a start date to be 30 minutes
      org-agenda-default-appointment-duration 30
      ;; Time grid
      org-agenda-time-grid '((daily require-timed)
                             "--------------------"
                             (800 1000 1200 1400 1600 1800 2000 2200))
      ;; Always start with today (nil) or Saturday (6)
      org-agenda-start-on-weekday nil
      ;; Only include project and action lists in agenda
      org-agenda-files (list gtd-projects-file gtd-actions-file gtd-calendar-file gtd-goals-file)
      ;; Don't by default show the action in context
      org-agenda-start-with-follow-mode nil
      ;; Don't show tags in the agendas
      org-agenda-remove-tags t
      ;; Dim blocked tasks
      org-agenda-dim-blocked-tasks t
      ;; Scheduling time stamps in TODO entries become start date. Some calendar
      ;; applications show TODO entries only after that date.
      org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
      ;; Use deadlines in TODO entries as due-dates
      org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
      ;; Add TODO tasks to exported calendar
      org-icalendar-include-todo t
      ;; Don't include any body text in calendar events
      org-icalendar-include-body nil
      ;; Set an alarm for 15 minutes before timed events
      org-icalendar-alarm-time 15
      ;; Export iCalendar events with active timestamps (<...>, not [...]) only
      org-icalendar-with-timestamps 'active
      ;; Remove extra stuff from tags agenda views (what the GTD context views
      ;; use). Keep the others here too (with their original format) in case
      ;; these need modifying also
      org-agenda-prefix-format '((agenda . "  %-12t%s")
                                 (timeline . "  % s")
                                 (todo . " %i %-12:c")
                                 (tags . "")
                                 (search . " %i %-12:c")))

;; Create agendas for each tag (GTD context) defined. Set up the waiting for
;; list separately
(setq org-agenda-custom-commands
      (remove nil (mapcar (lambda (tag)
                            (when (stringp (car tag))
                              (let* ((text (car tag))
                                     (shortcut (string (cdr tag)))
                                     (action-list (concat gtd-action-lists-dir text))
                                     (waitp (equal text "waiting")))
                                (if waitp
                                    `("w" "Waiting for" ((tags ,(concat text "-TODO=\"DONE\""))) nil
                                      (,action-list))
                                  `(,shortcut ,(capitalize text)
                                              ((tags-todo ,(concat "TODO=\"NEXT\"+" text)
                                                          ((org-agenda-overriding-header
                                                            ,(concat "\n@" text " Tasks\n"
                                                                     ;; Add 7 for "@ Tasks" characters
                                                                     (make-string (+ 7 (string-width text)) ?\=)
                                                                     "\n"))
                                                           (org-agenda-sorting-strategy
                                                            '((agenda time-up priority-down tag-up)))))
                                               (agenda ""))
                                              nil
                                              (,action-list))))))
                          org-tag-alist)))

;; During weekly review, show the previous week, as well as the week ahead
(add-to-list 'org-agenda-custom-commands
             '("W" "Weekly review"
               ((stuck "" ((org-agenda-overriding-header (concat "\nWeekly Review\n"
                                                                 (make-string 13 ?\=) "\n"))))
                (agenda "" ((org-agenda-span 14)
                            (org-agenda-start-day "-3d")
                            (org-agenda-show-log t)
                            (org-agenda-start-with-log-mode t)
                            (org-agenda-overriding-header "")
                            (org-agenda-skip-deadline-if-done nil)
                            (org-agenda-skip-scheduled-if-done nil))))) t)

;; Export agendas as action lists
(setq org-agenda-exporter-settings
      '((ps-number-of-columns 2)
        (ps-landscape-mode t)
        (org-agenda-add-entry-text-maxlines 5)
        (htmlize-output-type 'css)
        (org-agenda-prefix-format "[] ")
        (org-agenda-with-colors t)
        (org-agenda-remove-tags t)))


;;; Refiling

(setq org-log-refile nil)
;; Store notes at the top of the tree
(setq org-reverse-note-order t)
(setq org-refile-targets '((gtd-projects-file :maxlevel . 3)
                           (gtd-actions-file :maxlevel . 3)
                           (gtd-someday-maybe-file :maxlevel . 3)
                           (gtd-reference-file :maxlevel . 3)
                           (gtd-calendar-file :maxlevel . 3)
                           (gtd-goals-file :maxlevel . 3)))


;;; Hooks

;; In agenda buffers, C-c C-c isn't bound to anything. Bind to org-agenda-todo,
;; to make it useful (and then save all org buffers)
(add-hook 'org-agenda-mode-hook (lambda ()
                                  (define-key org-agenda-keymap (kbd "C-c C-c")
                                    (lambda ()
                                      (interactive)
                                      (org-agenda-todo "DONE")
                                      (org-agenda-redo)
                                      (org-save-all-org-buffers)))
                                  (hl-line-mode)
                                  (when (and gtd-agenda-use-full-frame-p
                                             (> (length (frame-list)) 1))
                                    (delete-other-windows))))

;; Save org files after refiling
(add-hook 'org-after-refile-insert-hook 'org-save-all-org-buffers)

;; Revert files automatically
(add-hook 'find-file-hook
          (lambda () (when (string-match org-directory buffer-file-name)
                  (auto-revert-mode 1)
                  (setq fill-column 80))))

(provide 'org-gtd)

;;; org-gtd.el ends here





