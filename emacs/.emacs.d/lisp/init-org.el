;; Org-Mode Settings ;;

;; Set up key binding shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Set up org mode
(setq org-startup-indented t)
(setq org-startup-folded t)
(setq org-directory "~/Sync/org/")
(setq org-agenda-files '("~/Sync/org/gtd.org"
                         "~/Sync/org/gtd.org_archive"
                         "~/Sync/org/someday.org"
                         "~/Sync/org/reminders.org"
                         "~/Sync/org/read.org"
                         "~/Sync/org/references/articles.org"))
(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-agenda-inhibit-startup t) ; Inhibit startup options to speed up agenda

;; Set up refile targets
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-outline-path-complete-in-steps nil) ; Refile in a single go
(setq org-refile-use-outline-path t)          ; Show full paths for refiling
(setq org-refile-allow-creating-parent-nodes 'confirm) ; New parents on refile

;; Define keywords for projects and tasks
(setq org-todo-keywords '((sequence "TODO(t)"
                                    "NEXT(n)"
                                    "WAITING(w)"
                                    "PROJECT(p)"
                                    "MAYBE(m)"
                                    "|"
                                    "DONE(d)"
                                    "CANCELLED(c)")))

;; Define tags available
(setq org-tag-alist
      '(("ongoing" . ?o)
        ("drill" . ?d)
        ("flag" . ?f)
        ("random" . ?r)
        ("nobrain" . ?n)
        ("childless" . ?l)
        ("readend" . ?e)
        ("task" . ?t)))

;; Load Markdown exporter
;; source: https://stackoverflow.com/a/22990257/6873133
(eval-after-load "org" '(require 'ox-md nil t))

;; Separate fill-column value for org-mode
;; source: https://emacs.stackexchange.com/a/29063/
(add-hook 'org-mode-hook (lambda () (setq fill-column nil)))

;; Setup org-capture templates
(setq org-capture-templates (quote (
    ;; Capture article summaries
    ("a"              ; key
     "Article"        ; name
     entry            ; type
     (file+headline "~/Sync/org/phd.org" "To Sort") ; target
     (file "~/Sync/org/templates/article.orgcaptempl") ; template
     :prepend t       ; properties
     :empty-lines 1   ; properties
     :created t       ; properties
    )
    ;; Capture notes and reference material
    ("n"
     "Note"
     entry
     (file+olp "~/Sync/org/inbox.org" "Tasks")
     (file "~/Sync/org/templates/note.orgcaptempl")
    )
    ;; Capture reading materials
    ("d"
     "To Read"
     entry
     (file+olp "~/Sync/org/read.org" "Read Queue")
     (file "~/Sync/org/templates/read.orgcaptempl")
    )
    ;; Capture incoming tasks
    ("t"
     "Task"
     entry
     (file+olp "~/Sync/org/inbox.org" "Tasks")
     (file "~/Sync/org/templates/task.orgcaptempl")
    )
    ;; Journaling
    ("j"
     "Journal"
     entry
     (file "~/Sync/org/journal.org")
     (file "~/Sync/org/templates/journal.orgcaptempl")
    )
    ;; Journaling
    ("r"
     "Weekly Review"
     entry
     (file "~/Sync/org/weekly.org")
     (file "~/Sync/org/templates/weekly.orgcaptempl")
    )
    ;; Research and project ideas
    ("i"
     "Research and Project Ideas"
     entry
     (file "~/Sync/org/ideas.org")
     (file "~/Sync/org/templates/research.orgcaptempl")
    )
)))

;; Enable native fontification in code blocks
(setq org-src-fontify-natively t)

;; Change column width for habit graph
(setq org-habit-graph-column 63)

;; Include clock
(setq org-clock-report-include-clocking-task t)

;; Hook to change visual view of agenda
;; source: https://superuser.com/a/531670/
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))

;; Additional org functions for checklist handling
;; https://orgmode.org/worg/org-contrib/org-checklist.html
(use-package org
  :ensure org-plus-contrib)
(require 'org-checklist)

;; Setup optional org-modules
(setq org-modules (quote (org-habit org-drill)))
(eval-after-load 'org
                 '(org-load-modules-maybe t))

;; Have org-drill look through current directory for files
(setq org-drill-scope (quote directory))

;; Set learn fraction, higher == larger time interval
;; Default == 0.5
(setq org-drill-learn-fraction 0.3)

;; Define stuck projects
(setq org-stuck-projects
      '("+LEVEL=2/-DONE" ;; Tags/TODO/property matcher string
        ("TODO" "NEXT" "NEXTACTION") ;; List of TODO keywords of non-stuck projects
        ("childless") ;; List of tags for non-stuck projects
        "")) ;; Arbitrary regulary expresion for non-stuck projects

;; Add keybindings for org-drill
(defun etl/tag-as-drill ()
  "Add `drill` tag to current org entry."
  (interactive)
  (org-set-tags-to (cons "drill" (org-get-tags))))
(defun etl/org-drill-hook ()
  "Miscellaneous keychords for org-drill mode"
  (visual-line-mode)
  (local-set-key (kbd "C-c d d") 'org-drill)
  (local-set-key (kbd "C-c d e") 'org-drill-tree)
  (local-set-key (kbd "C-c d r") 'org-drill-resume)
  (local-set-key (kbd "C-c d t") 'etl/tag-as-drill))
(defun etl/org-mode-hook ()
  "Miscellaneous keychords for org-mode"
  (visual-line-mode)
  (local-set-key (kbd "C-c b v") 'org-brain-visualize)
  (local-set-key (kbd "C-c b i") 'org-id-get-create))

;; Setup org-mode useful hooks
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'etl/org-drill-hook)
(add-hook 'org-mode-hook 'etl/org-mode-hook)

;; Place tags close to the right-hand side of the window
;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-12/msg00410.html
(defun etl/place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))
(add-hook 'org-finalize-agenda-hook 'etl/place-agenda-tags)

;; Modify agenda to be facilitate getting things done
;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun etl/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))
;; TODO WIP
;; Modified from https://stackoverflow.com/a/10091330/6873133
(defun etl/org-agenda-skip-tag (tag &optional others)
  "Skip all entries that correspond to TAG.

If OTHERS is true, skip all entries that do not correspond to TAG."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if others
        (if (not (member tag (org-get-tags-at current-headline)))
            next-headline
          nil)
      (if (member tag (org-get-tags-at current-headine))
          next-headline
        nil))))
(defun etl/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (etl/org-skip-subtree-if-priority ?A)
                          (etl/org-skip-subtree-if-habit)
                          (org-agenda-skip-entry-if 'regexp "[[:digit:]]\{4\} - .*")
                          (org-agenda-skip-entry-if 'todo '("WAITING" "MAYBE"))
                          (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "All normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))
        ("W" "Weekly Review"
         ((agenda "" ((org-agenda-span 7))) ; Review upcoming deadlines
          (stuck "") ; Review stuck tasks that aren't maybe
          (todo "PROJECT") ; Review all projects being TODO items
          (todo "MAYBE") ; Review someday/maybe items
          (todo "WAITING") ; Review waiting items
          ))))

;; Org-mode exporters
(require 'ox-taskjuggler) ;; Taskjuggler exporter
(require 'ox-freemind) ;; Freemind mindmapping


(provide 'init-org)
