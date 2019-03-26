;; Org-Mode Settings ;;

;; Set up key binding shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Set up org mode
(setq org-startup-indented t)
(setq org-startup-folded t)
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files '("~/Dropbox/org/gtd.org"
                         "~/Dropbox/org/gtd.org_archive"
                         "~/Dropbox/org/someday.org"
                         "~/Dropbox/org/reminders.org"
                         "~/Dropbox/org/read.org"
                         "~/Dropbox/org/references/articles.org"))
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
                                    "|"
                                    "DONE(d)"
                                    "CANCELLED(c)")))

;; Define tags available
(setq org-tag-alist
      '(("ongoing" . ?o)
        ("drill" . ?d)
        ("flag" . ?f)
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
     (file+headline "~/Dropbox/org/phd.org" "To Sort") ; target
     (file "~/Dropbox/org/templates/article.orgcaptempl") ; template
     :prepend t       ; properties
     :empty-lines 1   ; properties
     :created t       ; properties
    )
    ;; Capture notes and reference material
    ("n"
     "Note"
     entry
     (file+olp "~/Dropbox/org/inbox.org" "Tasks")
     (file "~/Dropbox/org/templates/note.orgcaptempl")
    )
    ;; Capture reading materials
    ("d"
     "To Read"
     entry
     (file+olp "~/Dropbox/org/read.org" "Read Queue")
     (file "~/Dropbox/org/templates/read.orgcaptempl")
    )
    ;; Capture incoming tasks
    ("t"
     "Task"
     entry
     (file+olp "~/Dropbox/org/inbox.org" "Tasks")
     (file "~/Dropbox/org/templates/task.orgcaptempl")
    )
    ;; Journaling
    ("j"
     "Journal"
     entry
     (file "~/Dropbox/org/journal.org")
     (file "~/Dropbox/org/templates/journal.orgcaptempl")
    )
    ;; Journaling
    ("r"
     "Weekly Review"
     entry
     (file "~/Dropbox/org/weekly.org")
     (file "~/Dropbox/org/templates/weekly.orgcaptempl")
    )
    ;; Research and project ideas
    ("i"
     "Research and Project Ideas"
     entry
     (file "~/Dropbox/org/ideas.org")
     (file "~/Dropbox/org/templates/research.orgcaptempl")
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

;; Setup org-mode useful hooks
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Place tags close to the right-hand side of the window
(defun etl/place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))
(add-hook 'org-finalize-agenda-hook 'etl/place-agenda-tags)

;; Org-mode exporters
(require 'ox-taskjuggler) ;; Taskjuggler exporter
(require 'ox-freemind) ;; Freemind mindmapping


(provide 'init-org)
