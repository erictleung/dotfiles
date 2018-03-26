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
                         "~/Dropbox/org/someday.org"
                         "~/Dropbox/org/habits.org"
                         "~/Dropbox/org/read.org"))
(add-to-list 'org-agenda-files (expand-file-name "~/Dropbox/org/references"))
(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Set up refile targets
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-outline-path-complete-in-steps nil) ; Refile in a single go
(setq org-refile-use-outline-path t)          ; Show full paths for refiling
(setq org-refile-allow-creating-parent-nodes 'confirm) ; New parents on refile

;; Define keywords for projects and tasks
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Load Markdown exporter
;; source: https://stackoverflow.com/a/22990257/6873133
(eval-after-load "org" '(require 'ox-md nil t))

;; Add org-ref notes
(setq org-ref-notes-directory "~/Dropbox/org/references/notes"
      org-ref-bibliography-notes "~/Dropbox/org/references/articles.org"
      org-ref-default-bibliography '("~/Dropbox/org/references/references.bib")
      org-ref-pdf-directory "~/Dropbox/zotero/")

;; Setup org-capture templates
(setq org-capture-templates (quote (
    ;; Capture article summaries
    ("a"
     "Article"
     entry
     (file+headline "~/Dropbox/org/phd.org" "Article")
     "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?"
     :prepend t
     :empty-lines 1
     :created t
    )
    ;; Capture notes and reference material
    ("n"
     "Note"
     entry
     (file+olp "~/Dropbox/org/inbox.org" "Tasks")
     "* %?\n:CREATED: %U"
    )
    ;; Capture incoming tasks
    ("t"
     "Task"
     entry
     (file+olp "~/Dropbox/org/inbox.org" "Tasks")
     "* TODO %?\n:CREATED: %U"
    )
    ;; Journaling
    ("j"
     "Journal"
     entry
     (file "~/Dropbox/org/journal.org")
     "* %T\n** Grateful?\n- %?\n** Making today great?\n- \n** Daily affirmations. I am...\n- \n** Three amazing things\n- \n** How to make today better?\n- \n** Learned\n- \n** Comments\n- "
    )
)))

;; Setup optional org-modules
(setq org-modules '(org-habit))
(eval-after-load 'org
                 '(org-load-modules-maybe t))

(provide 'init-org)
