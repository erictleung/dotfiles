;; Packages I use
(setq required-packages
      '(ido
        ace-jump-mode
        org-ref
        interleave
        helm-bibtex))

;; M-x install-missing-packages to install above packages on new computer
;; source: https://dthompson.us/syncing-required-packages-in-emacs.html
(defun install-missing-packages ()
  "Install all required packages that haven’t been installed."
  (interactive)
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
        required-packages)
  (message "Installed all missing packages!"))

;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Add built-in package manager
(require 'package)

;; Add in popular packages
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (markdown-mode helm-bibtexkey interleave org-ref ## evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Add line numbers
(global-linum-mode t)
(setq linum-format "%d ")

;; Word wrap long lines
(global-visual-line-mode t)

;; Set basic backup settings
;; Source: https://stackoverflow.com/a/20824625/6873133
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(setq vc-make-backup-files t)

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

(add-hook 'text-mode-hook 'auto-fill-mode)

;; Org-Mode Settings ;;

;; set up key binding shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; set up org mode
(setq org-startup-indented t)
(setq org-startup-folded t)
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files '("~/Dropbox/org/gtd.org"
                         "~/Dropbox/org/someday.org"))
(add-to-list 'org-agenda-files (expand-file-name "~/Dropbox/org/references"))
(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; set up refile targets
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-outline-path-complete-in-steps nil) ; Refile in a single go
(setq org-refile-use-outline-path t)          ; Show full paths for refiling
(setq org-refile-allow-creating-parent-nodes 'confirm) ; New parents on refile

;; define keywords for projects and tasks
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Word wrap lines
(setq-default word-wrap t)
(setq-default fill-column 79)

;; Load Markdown exporter
;; source: https://stackoverflow.com/a/22990257/6873133
(eval-after-load "org" '(require 'ox-md nil t))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; set up ido mode
(require `ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(ido-mode 1)

;; remove unnecessary toolbars, scrollbars, etc
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; set up ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC" ) 'ace-jump-mode)

;; Use spaces instead of tabs
;; source: http://emacsblog.org/2007/09/30/quick-tip-spaces-instead-of-tabs/
(setq-default indent-tabs-mode nil)

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
     "* %T\n** Grateful?\n*** %?\n** Making today great?\n** Daily affirmations. I am...\n** Three amazing things\n** How to make today better?"
    )
)))
