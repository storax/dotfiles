;;; funcs.el --- storax-org layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun storax/new-capture-template (filename)
  "Create a new capture template"
  (interactive "sNew capture template file name: ")
  (find-file (concat storax-org-caputre-dir filename)))

(defvar storax/capture-history nil)

;; Org capture stuff
(defun storax/capture-prompt (prompt variable)
  "PROMPT for string, save it to VARIABLE and insert it."
  (make-local-variable variable)
  (set variable (read-string (concat prompt ": ") nil storax/capture-history)))

(defun storax/capture-insert (variable)
  "Insert content of VARIABLE."
  (symbol-value variable))

(defun storax/capture-include (what text &rest fmtvars)
  "Ask user to include WHAT.  If user agrees return TEXT."
  (when (y-or-n-p (concat "Include " what "?"))
    (apply 'format text fmtvars)))

(defun storax/get-amazon-delivery-date ()
  "Get Amazon delivery mail from a dispatch mail notification."
  (let* ((contents (with-current-buffer (org-capture-get :original-buffer)
                     (buffer-string)))
         (arrival (save-match-data ; is usually a good idea
                    (and (string-match "Arriving:\n\\(.*\\)" contents)
                         (org-read-date nil nil (match-string 1 contents))))))
    (format "<%s>" arrival)))

(defalias 'oc/prmt 'storax/capture-prompt)
(defalias 'oc/ins 'storax/capture-insert)
(defalias 'oc/inc 'storax/capture-include)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Publish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun storax/org-save-then-publish (&optional force)
  (interactive "P")
  (save-buffer)
  (org-save-all-org-buffers)
  (let ((org-html-head-extra))
    (org-publish-current-project force)))

(defun storax/org-rtd-template-html-header ()
  "Read the rtd template html header."
  (with-temp-buffer
    (insert-file-contents storax-org-rtd-template)
    (buffer-string)))

(defun storax/add-publish-project (name basedir publishdir)
  "Add a publish project."
  (interactive (list (read-string "Project Name: ")
                     (read-directory-name "Project Base Directory: ")
                     (read-directory-name "Project Publish Directory: ")))
  (add-to-list 'org-publish-project-alist
               `(,(concat name "-org")
                 :base-directory ,basedir
                 :publishing-directory ,publishdir
                 :recursive t
                 :section-numbers nil
                 :table-of-contents nil
                 :base-extension "org"
                 :plain-source t
                 :htmlized-source t
                 :html-head ,(storax/org-rtd-template-html-header)
                 :publishing-function org-html-publish-to-html
                 :style-include-default nil
                 :author-info t
                 :creator-info t))
  (add-to-list 'org-publish-project-alist
               `(,(concat name "-media")
                 :base-directory ,basedir
                 :publishing-directory ,publishdir
                 :recursive t
                 :base-extension "pdf\\|png\\|jpg\\|gif"
                 :publishing-function org-publish-attachment
                 :author nil))
  (add-to-list 'org-publish-project-alist
               `(,name :components (,(concat name "-org") ,(concat name "-media")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun storax/org-verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun storax/org-match-at-point-p (match &optional todo-only)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'.

If the optional argument TODO-ONLY is non-nil, do not declare a
match unless headline at point is a todo item.

Credits: http://stackoverflow.com/a/33444799"
  (let ((todo      (org-get-todo-state))
        (tags-list (org-get-tags-at)))
    (eval (cdr (org-make-tags-matcher match)))))

(defun storax/org-agenda-skip-without-match (match)
  "Skip current headline unless it matches MATCH.

Return nil if headline containing point matches MATCH (which
should be a match string of the same format used by
`org-tags-view').  If headline does not match, return the
position of the next headline in current buffer.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines that do not match.

Credis: http://stackoverflow.com/a/33444799"
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (my/org-match-at-point-p match) nil next-headline))))

(defun storax/org-auto-exclude-function (tag)
  "Evaluate if TAG should be excluded."
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(defun storax/org-is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun storax/org-is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (storax/org-find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun storax/org-is-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun storax/org-is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun storax/org-list-sublevels-for-projects-indented ()
  "Set `org-tags-match-list-sublevels' so when restricted to a subtree we list all subtasks.
This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun storax/org-list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun storax/org-toggle-next-task-display ()
  (interactive)
  (setq storax/org-hide-scheduled-and-waiting-next-tasks
        (not storax/org-hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks"
           (if storax/org-hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun storax/org-skip-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (storax/org-is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun storax/org-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  ;; (storax/org-list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (storax/org-is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun storax/org-skip-non-projects ()
  "Skip trees that are not projects."
  ;; (storax/org-list-sublevels-for-projects-indented)
  (if (save-excursion (storax/org-skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((storax/org-is-project-p)
            nil)
           ((and (storax/org-is-project-subtree-p) (not (storax/org-is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun storax/org-skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((storax/org-is-task-p)
        nil)
       (t
        next-headline)))))

(defun storax/org-skip-project-trees-and-habits ()
  "Skip trees that are projects."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((storax/org-is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and storax/org-hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((storax/org-is-project-p)
        next-headline)
       ((and (storax/org-is-task-p) (not (storax/org-is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun storax/org-skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks,
habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks,
habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((storax/org-is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (storax/org-is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (storax/org-is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((storax/org-is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((storax/org-is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((storax/org-is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (storax/org-is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (storax/org-is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((storax/org-is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-non-subprojects ()
  "Skip trees that are not projects."
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (storax/org-is-subproject-p)
        nil
      next-headline)))

(defun storax/org-skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving."
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

;; Erase all reminders and rebuilt reminders for today from the agenda
(defun storax/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun storax/org-agenda-sort (a b)
  "Sorting strategy for agenda items A and B.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((storax/org-agenda-sort-test 'storax/org-is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((storax/org-agenda-sort-test 'storax/org-is-due-deadline a b))

     ; late deadlines next
     ((storax/org-agenda-sort-test-num 'storax/org-is-late-deadline '> a b))

     ; scheduled items for today next
     ((storax/org-agenda-sort-test 'storax/org-is-scheduled-today a b))

     ; late scheduled items next
     ((storax/org-agenda-sort-test-num 'storax/org-is-scheduled-late '> a b))

     ; pending deadlines last
     ((storax/org-agenda-sort-test-num 'storax/org-is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro storax/org-agenda-sort-test (fn a b)
  "Test for agenda sort."
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro storax/org-agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun storax/org-is-not-scheduled-or-deadline (date-str)
  (and (not (storax/org-is-deadline date-str))
       (not (storax/org-is-scheduled date-str))))

(defun storax/org-is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun storax/org-is-late-deadline (date-str)
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun storax/org-is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun storax/org-is-deadline (date-str)
  (or (storax/org-is-due-deadline date-str)
      (storax/org-is-late-deadline date-str)
      (storax/org-is-pending-deadline date-str)))

(defun storax/org-is-scheduled (date-str)
  (or (storax/org-is-scheduled-today date-str)
      (storax/org-is-scheduled-late date-str)))

(defun storax/org-is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun storax/org-is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clocking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar storax/org-keep-clock-running nil)

(defun storax/org-clock-in-to-next (kw)
  "Switch a task from KW TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (storax/org-is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (storax/org-is-project-p))
      "TODO"))))

(defun storax/org-punch-in (arg)
  "Start continuous clocking and set the default task to the selected task.
If no task is selected set the Organization task as the default task."
  (interactive "p")
  (setq storax/org-keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (storax/org-clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (storax/org-clock-in-organization-task-as-default)))))

(defun storax/org-punch-out ()
  (interactive)
  (setq storax/org-keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun storax/org-clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun storax/org-clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in."
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when storax/org-keep-clock-running
            (storax/org-clock-in-default-task)))))))

(defun storax/org-clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find storax/org-organization-task-id 'marker)
    (org-clock-in '(16))))

(defun storax/org-clock-out-maybe ()
  (when (and storax/org-keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (storax/org-clock-in-parent-task)))

(defun storax/org-clock-in-task-by-id (id)
  "Clock in a task by ID."
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun storax/org-clock-in-last-task (arg)
  "Clock in the interrupted task if there is one.
Skip the default task and get the next one.
A prefix ARG forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun storax/org-buffers ()
  "Return a list of org buffers."
  (let (buffers)
    (dolist (b (buffer-list))
      (when (with-current-buffer b (equal major-mode 'org-mode))
        (add-to-list 'buffers (buffer-name b))))
    buffers))

(defun storax/org-hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)))

(defun storax/org-display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(defun storax/org-source-file ()
  "Create a nice abbreviation for the current file."
  (let ((prjname (projectile-project-name))
        (fullfile (buffer-file-name)))
    (if (eq prjname "-")
        fullfile
      (format
       "%s:%s" prjname
       (substring
        fullfile (+ (cl-search prjname fullfile) (length prjname) 1)
        (length fullfile))))))

(defun storax/org-insert-source-link ()
      "Create a source link to the current line in the file."
      (interactive)
      (let ((srcfile (storax/org-source-file))
            (fullfile (buffer-file-name))
            (lineno (line-number-at-pos
                     (if (region-active-p)
                         (region-beginning)
                       (point))))
            (lineendno (line-number-at-pos
                        (if (region-active-p)
                            (region-end)
                          (point))))
            (orgbufs (storax/org-buffers))
            selectedbuf
            linestr)
        (if (equal lineno lineendno)
            (setq linestr (format "l.%s" lineno))
          (setq linestr (format "l.%s-l.%s" lineno lineendno)))
        (unless orgbufs
          (add-to-list
           'orgbufs
           (get-buffer-create (read-from-minibuffer "No org buffer found. New buffer name: ")))
          (with-current-buffer (car orgbufs)
            (org-mode)))
        (if (> (length orgbufs) 1)
            (setq selectedbuf
                  (completing-read
                   "Choose buffer to insert link: "
                   orgbufs nil t nil
                   'storax/org-source-link-file-hist))
          (setq selectedbuf (car orgbufs)))
        (switch-to-buffer-other-window selectedbuf)
        (goto-char (point-max))
        (unless (eq (point) (line-beginning-position))
          (newline))
        (org-insert-heading)
        (insert (org-make-link-string
                 (format "file:%s::%s" fullfile lineno)
                 (format "%s:%s" srcfile linestr)))
        (insert "\n")))

(defun storax/org-find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun storax/org-screenshot (filename &optional size)
  "Save a screenshot to FILENAME and resize it to SIZE."
  (interactive "FFilename: \nsResize: ")
  (call-process "import" nil nil nil filename)
  (call-process "convert" nil nil nil filename "-resize" size filename)
  (insert (concat "[[" filename "]]"))
  (storax/org-display-inline-images))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rifle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun storax/org-rifle-agenda ()
  "Rifle through Org files in agenda files."
  (interactive)
  (helm-org-rifle-files (org-agenda-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Babel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-babel-python-strip-session-chars ()
  "Remove >>> and ... from a Python session output.

Credits: http://kitchingroup.cheme.cmu.edu/blog/2015/03/12/Making-org-mode-Python-sessions-look-better/"
  (when (and (string=
              "python"
              (org-element-property :language (org-element-at-point)))
             (string-match
              ":session"
              (org-element-property :parameters (org-element-at-point))))

    (save-excursion
      (when (org-babel-where-is-src-block-result)
        (goto-char (org-babel-where-is-src-block-result))
        (end-of-line 1)
        ;(while (looking-at "[\n\r\t\f ]") (forward-char 1))
        (while (re-search-forward
                "\\(>>> \\|\\.\\.\\. \\|: $\\|: >>>$\\)"
                (org-element-property :end (org-element-at-point))
                t)
          (replace-match "")
          ;; this enables us to get rid of blank lines and blank : >>>
          (beginning-of-line)
          (when (looking-at "^$")
            (kill-line)))))))

;;; funcs.el ends here
