;;; packages.el --- storax-org layer packages file for Spacemacs.
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

(defvar storax-org-pages-theme-dir
  (concat (file-name-directory load-file-name) "org-page-themes"))

(defvar storax-org-java-dir
  (concat (file-name-directory load-file-name) "java/"))

(defvar storax-plantuml-jar
  (concat storax-org-java-dir "plantuml.jar"))

(defvar storax-ditaa-jar
  (concat storax-org-java-dir "ditaa.jar"))

(defconst storax-org-packages
  '(org orgbox ox-rst org-page helm-org-rifle deft ox-jira org-alert))

(defun storax-org/init-orgbox ()
  (use-package orgbox))

(defun storax-org/init-ox-rst ()
  (use-package ox-rst :after ox))

(defun storax-org/init-org-alert ()
  (use-package org-alert
    :init
    (progn
      (setq alert-default-style 'libnotify))
    :config
    (progn
      (org-alert-enable))))

(defun storax-org/setup-org ()
  (setq org-modules
        '(org-bibtex
          org-docview
          org-gnus
          org-habit
          org-id
          org-notmuch
          org-toc
          org-info)

        org-directory "~/Documents/org/"
        org-default-notes-file "~/Documents/org/refile.org"
        org-agenda-files '("~/Documents/org/refile.org"
                           "~/Documents/org/deliveries.org"
                           "~/Documents/org/todo.org")
        org-agenda-diary-file "~/Documents/org/diary.org"

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; MISCELLANEOUS
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-return-follows-link t
        org-read-date-prefer-future 'time
        org-cycle-include-plain-lists t
        org-yank-adjusted-subtrees t
        org-global-properties  '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                 ("STYLE_ALL" . "habit"))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; VISUAL
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-indented t
        org-cycle-separator-lines 0
        ;; Don't enable this because it breaks access to emacs from Android phone
        ;; org-startup-with-inline-images nil
        ;; Inline images in HTML instead of producting links to the image
        org-html-inline-images t
        org-blank-before-new-entry  '((heading)
                                      (plain-list-item . auto))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; TODO-WORKFLOW
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING"))
        org-todo-keyword-faces
        '(("TODO" :foreground "OrangeRed" :weight bold)
          ("NEXT" :foreground "DeepSkyBlue" :weight bold)
          ("DONE" :foreground "LawnGreen" :weight bold)
          ("WAITING" :foreground "Orange" :weight bold)
          ("HOLD" :foreground "Gold" :weight bold)
          ("CANCELLED" :foreground "DarkGray" :weight bold)
          ("MEETING" :foreground "MediumSeaGreen" :weight bold))
        org-use-fast-todo-selection t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-enforce-todo-dependencies t
        org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; TAGS
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Tags with fast selection keys
        org-tag-alist
        '((:startgroup)
          ("@work" . ?e)
          ("@home" . ?H)
          (:endgroup)
          (:startgroup)
          ("TRIAGE" . ?t)
          ("IMPL" . ?i)
          ("CODEREVIEW" . ?r)
          (:endgroup)
          ("WAITING" . ?w)
          ("HOLD" . ?h)
          ("PERSONAL" . ?P)
          ("WORK" . ?W)
          ("NOTE" . ?n)
          ("CANCELLED" . ?c))

        ;; Don't exit after first key
        org-fast-tag-selection-single-key nil

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; CAPTURE
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-capture-templates
        `(("t" "todo" entry (file "refile.org")
           "* TODO %?\n" :clock-in t :clock-resume t)
          ("r" "respond" entry (file "refile.org")
           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n"
           :clock-in t :clock-resume t :immediate-finish t)
          ("n" "note" entry (file "notes.org")
           "* %? :NOTE:\n" :clock-in t :clock-resume t)
          ("m" "Mails")
          ("md" "Deliveries")
          ("mdd" "Delivery" entry (file "deliveries.org")
           "* TODO %:subject%?\nSCHEDULED: %^t\n%:link")
          ("mda" "Amazon" entry (file "deliveries.org")
           "* TODO %:subject\nSCHEDULED: %(storax/get-amazon-delivery-date)\n%:link")
          ("j" "Journal" entry (file+datetree "diary.org")
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("h" "Habit" entry (file "refile.org")
           (file ,(concat storax-org-caputre-dir "habit.org"))))
        org-reverse-note-order nil

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; REFILE
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
        org-refile-targets  '((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-target-verify-function 'storax/org-verify-refile-target

        org-archive-mark-done nil
        org-archive-location "%s_archive::* Archived Tasks"

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; AGENDA
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-agenda-dim-blocked-tasks nil
        org-agenda-compact-blocks t
        org-agenda-auto-exclude-function 'storax/org-auto-exclude-function
        ;; For tag searches ignore tasks with scheduled and deadline dates
        org-agenda-tags-todo-honor-ignore-options t
        org-agenda-custom-commands
        '(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          ("o" "Agenda"
           ((agenda "" nil)
            (tags "Triage"
                  ((org-agenda-overriding-header "Triage")
                   (org-tags-match-list-sublevels nil)))
            (tags "IMPL"
                  ((org-agenda-overriding-header "Implementations")
                   (org-tags-match-list-sublevels nil)))
            (tags "CODEREVIEW"
                  ((org-agenda-overriding-header "Code Reviews")
                   (org-tags-match-list-sublevels nil)))
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags "-REFILE/"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'storax/org-skip-non-archivable-tasks)
                   (org-tags-match-list-sublevels nil))))
           nil))

        org-agenda-clock-consistency-checks
        '(:max-duration "4:00"
                        :min-duration 0
                        :max-gap 0
                        :gap-ok-around ("4:00"))
        ;; Agenda clock report parameters
        org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)
        ;; Agenda log mode items to display (closed and state changes by default)
        org-agenda-log-mode-items '(closed state)
        org-stuck-projects '("" nil nil "")
        org-deadline-warning-days 30

        ;; Keep tasks with dates on the global todo lists
        org-agenda-todo-ignore-with-date nil
        ;; Keep tasks with deadlines on the global todo lists
        org-agenda-todo-ignore-deadlines nil
        ;; Keep tasks with scheduled dates on the global todo lists
        org-agenda-todo-ignore-scheduled nil
        ;; Keep tasks with timestamps on the global todo lists
        org-agenda-todo-ignore-timestamp nil
        ;; Remove completed deadline tasks from the agenda view
        org-agenda-skip-deadline-if-done t
        ;; Remove completed scheduled tasks from the agenda view
        org-agenda-skip-scheduled-if-done t
        ;; Remove completed items from search results
        org-agenda-skip-timestamp-if-done t
        org-agenda-include-diary nil
        org-agenda-insert-diary-extract-time t
        ;; Include agenda archive files when searching for things
        org-agenda-text-search-extra-files '(agenda-archives)
        ;; Show all future entries for repeating tasks
        org-agenda-repeating-timestamp-show-all t
        ;; Show all agenda dates - even if they are empty
        org-agenda-show-all-dates t
        ;; Sorting order for tasks on the agenda
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        ;; Start the weekly agenda on Monday
        org-agenda-start-on-weekday 1
        ;; Enable display of the time grid so we can see the marker for the current time
        org-agenda-time-grid '((daily today remove-match)
                               (0900 1100 1300 1500 1700)
                               "......"
                               "----------------")
        ;; Display tags farther right
        org-agenda-tags-column -102
        org-agenda-cmp-user-defined 'storax/org-agenda-sort
        ;; position the habit graph on the agenda to the right of the default
        org-habit-graph-column 50

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Clocking
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-log-done 'time
        org-log-into-drawer t
        ;; Show lot of clocking history so it's easy to pick items
        org-clock-history-length 23
        ;; Resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; Change tasks to NEXT when clocking in
        org-clock-in-switch-to-state 'storax/org-clock-in-to-next
        ;; Save clock data and state changes and notes in the LOGBOOK drawer
        org-clock-into-drawer t
        ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;; Clock out when moving task to a done state
        org-clock-out-when-done t
        ;; Save the running clock and all clock history when exiting Emacs, load it on startup
        org-clock-persist t
        ;; Do not prompt to resume an active clock
        org-clock-persist-query-resume nil
        ;; Enable auto clock resolution for finding open clocks
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        ;; Include current clocking task in clock reports
        org-clock-report-include-clocking-task t
        org-time-stamp-rounding-minutes '(1 1)
        org-clock-sound storax-org-warning-sound
        org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; COLUMS
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;; Set default column view headings: Task Effort Clock_Summary
        org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Babel
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-confirm-babel-evaluate t
        org-ditaa-jar-path storax-ditaa-jar
        org-plantuml-jar-path storax-plantuml-jar
        org-src-window-setup 'current-window
        org-babel-default-header-args '((:session . "none")
                                        (:results . "replace")
                                        (:exports . "code")
                                        (:cache . "no")
                                        (:noweb . "no")
                                        (:hlines . "no")
                                        (:tangle . "no"))

        org-babel-default-header-args:python '((:results . "output verbatim raw replace")
                                               (:exports . "both")
                                               (:wrap . "EXAMPLE"))
        org-babel-default-header-args:ruby '((:results . "output verbatim raw replace")
                                             (:exports . "both")
                                             (:wrap . "EXAMPLE"))
        org-babel-default-header-args:sh '((:results . "output verbatim raw replace")
                                           (:exports . "both")
                                           (:wrap . "EXAMPLE"))
        org-babel-default-header-args:emacs-lisp '((:results . "output verbatim raw replace")
                                                   (:exports . "both")
                                                   (:wrap . "EXAMPLE"))
        org-babel-default-header-args:clojure '((:results . "output verbatim raw replace")
                                                (:exports . "both")
                                                (:wrap . "EXAMPLE"))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Export
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        org-export-babel-evaluate nil
        ;; Do not use sub or superscripts - I currently don't need this functionality in my documents
        org-export-with-sub-superscripts nil
        ;; Do not generate internal css formatting for HTML exports
        org-export-htmlize-output-type (quote css)
        ;; Export with LaTeX fragments
        org-export-with-LaTeX-fragments t
        ;; Increase default number of headings to export
        org-export-headline-levels 6
        org-export-with-tags nil
        org-latex-listings t
        org-html-xml-declaration
        '(("html" . "")
          ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
          ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
        org-html-head (storax/org-rtd-template-html-header))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; PUBLISH
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; empty

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (ditaa . t)
     (python . t)
     (ruby . t)
     (gnuplot . t)
     (clojure . t)
     (shell . t)
     (org . t)
     (plantuml . t)
     (latex . t)
     (sql . t)))

  (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate))

(defun storax-org/init-org ()
    (use-package org
      :defer t
      :init
      (spacemacs/declare-prefix "o" "org" "org-mode")
      (spacemacs/declare-prefix "oa" "agenda" "org agenda")
      (spacemacs/declare-prefix "or" "report" "org report")
      (spacemacs/declare-prefix "op" "publish" "org publish")
      (spacemacs/declare-prefix "ov" "visual" "org toggle visual")
      (progn
        (spacemacs/set-leader-keys "oaa" 'org-agenda)
        (spacemacs/set-leader-keys "oac" 'org-cycle-agenda-files)
        (spacemacs/set-leader-keys "ob" 'org-switchb)
        (spacemacs/set-leader-keys "oc" 'org-capture)
        (spacemacs/set-leader-keys "oC" 'calendar)
        (spacemacs/set-leader-keys "ol" 'org-store-link)
        (spacemacs/set-leader-keys "on" 'storax/org-toggle-next-task-display)
        (spacemacs/set-leader-keys "op SPC" 'org-publish)
        (spacemacs/set-leader-keys "opa" 'org-publish-all)
        (spacemacs/set-leader-keys "opf" 'org-publish-current-file)
        (spacemacs/set-leader-keys "opp" 'storax/org-save-then-publish)
        (spacemacs/set-leader-keys "opP" 'org-publish-project)
        (spacemacs/set-leader-keys "org" 'org-clock-goto)
        (spacemacs/set-leader-keys "ori" 'org-clock-in)
        (spacemacs/set-leader-keys "ord" 'org-clock-display)
        (spacemacs/set-leader-keys "ork" 'storax/org-punch-out)
        (spacemacs/set-leader-keys "oro" 'org-clock-out)
        (spacemacs/set-leader-keys "orp" 'storax/org-punch-in)
        (spacemacs/set-leader-keys "orr" 'org-resolve-clocks)
        (spacemacs/set-leader-keys "ot" 'org-toc-show)
        (spacemacs/set-leader-keys "ot" 'org-toc-show)
        (spacemacs/set-leader-keys "ova" 'show-all)
        (spacemacs/set-leader-keys "ovh" 'storax/org-hide-other))
      (spacemacs/set-leader-keys "ovi" 'org-toggle-inline-images)
      (spacemacs/set-leader-keys "ovl" 'org-toggle-link-display)
      :config
      (progn
        (storax-org/setup-org)
        (define-key org-mode-map (kbd "C-,") 'er/contract-region)
        (add-hook 'org-babel-after-execute-hook 'org-babel-python-strip-session-chars)
        (add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer 'append)
        (add-hook 'org-clock-out-hook 'storax/org-clock-out-maybe 'append)
        (add-hook 'org-babel-after-execute-hook 'storax/org-display-inline-images 'append)
        ;; Rebuild the reminders everytime the agenda is displayed
        (add-hook 'org-finalize-agenda-hook 'storax/org-agenda-to-appt 'append)
        (add-hook 'after-init-hook 'storax/org-agenda-to-appt)
        (add-hook 'after-init-hook (lambda () (load-file (concat (file-name-directory load-file-name) "appt.el"))))
        (appt-activate t)
        (run-at-time "24:01" nil 'storax/org-agenda-to-appt)
        (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))
        (run-at-time "00:59" 3600 'org-save-all-org-buffers)
        (org-babel-lob-ingest storax-org-lob-file)))
    (use-package ox-publish
      :config
      (progn
        (storax/add-publish-project "org" "~/Documents/org/" "~/Documenst/org-publish/"))))

(defun storax-org/init-org-page ()
  (use-package org-page
    :init
    (progn
      (spacemacs/declare-prefix "opb" "blog" "org-page")
      (spacemacs/set-leader-keys "opbp" 'op/do-publication)
      (spacemacs/set-leader-keys "opbn" 'op/new-post)
      (spacemacs/set-leader-keys "opb TAB" 'op/insert-options-template)
      (spacemacs/set-leader-keys "opb SPC" 'op/do-publication-and-preview-site))
    :config
    (progn
      (setq op/theme-root-directory storax-org-pages-theme-dir
            op/theme 'storax
            op/highlight-render 'htmlize
            op/repository-directory (expand-file-name "~/projects/storax.github.io/")
            op/repository-org-branch "source"
            op/repository-html-branch "master"
            op/site-domain "https://storax.github.io/"
            op/site-main-title "Storax"
            op/site-sub-title "Soon to be a major emacs mode."
            op/personal-github-link "https://github.com/storax"
            op/personal-disqus-shortname "storaxgithubio"
            op/category-config-alist
            '(("blog" ;; this is the default configuration
               :show-meta t
               :show-comment t
               :uri-generator op/generate-uri
               :uri-template "/blog/%y/%m/%d/%t/"
               :sort-by :date     ;; how to sort the posts
               :category-index t) ;; generate category index or not
              ("index"
               :show-meta nil
               :show-comment nil
               :uri-generator op/generate-uri
               :uri-template "/"
               :sort-by :date
               :category-index nil)
              ("about"
               :show-meta nil
               :show-comment nil
               :uri-generator op/generate-uri
               :uri-template "/about/"
               :sort-by :date
               :category-index nil))))))

(defun storax-org/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :init
    (progn
      (spacemacs/declare-prefix "o SPC" "rifle" "helm-org-rifle")
      (spacemacs/set-leader-keys "o SPC SPC" 'storax/org-rifle-agenda)
      (spacemacs/set-leader-keys "o SPC a" 'helm-org-rifle)
      (spacemacs/set-leader-keys "o SPC b" 'helm-org-rifle-current-buffer))))

(defun storax-org/post-init-deft ()
  (use-package deft
    :init
    (progn
      (setq deft-directory "~/Documents/org/"
            deft-extensions '("org")
            deft-org-mode-title-prefix t
            deft-use-filter-string-for-filename t))))

;;; packages.el ends here
