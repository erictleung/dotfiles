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
      '(("@office" . ?o)
        ("@home" . ?h)
        ("@computer" . ?c)
        ("@project" . ?p)
        ("@lunchtime" . ?l)
        ("@transit" . ?t)))

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
     (file+olp "~/Dropbox/org/read.org" "Read")
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
)))

;; Setup optional org-modules
(setq org-modules '(org-habit))
(eval-after-load 'org
                 '(org-load-modules-maybe t))

;; Enable native fontification in code blocks
(setq org-src-fontify-natively t)

;; Additional org functions for checklist handling
;; https://orgmode.org/worg/org-contrib/org-checklist.html
(use-package org-plus-contrib
  :ensure t)
(require 'org-checklist)

;; Org-mode exporters
(require 'ox-taskjuggler) ;; Taskjuggler exporter
(require 'ox-freemind) ;; Freemind mindmapping


(provide 'init-org)
