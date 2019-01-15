(setq debug-on-error t)

;; Add extra lisp files to path of files to call from
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa" user-emacs-directory))

;; Inhibit startup screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Add built-in package manager
(require 'package)

;; Add in popular package repositories
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

;; Install use-package for easy package management
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (misterioso)))
 '(doc-view-continuous t)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/references/articles.org" "~/Dropbox/org/gtd.org" "~/Dropbox/org/gtd.org_archive" "~/Dropbox/org/someday.org" "~/Dropbox/org/reminders.org" "~/Dropbox/org/read.org")))
 '(org-clock-report-include-clocking-task t)
 '(org-habit-graph-column 63)
 '(package-selected-packages
   (quote
    (xclip writegood-mode use-package try scala-mode ox-pandoc org-ref org-plus-contrib org-edna markdown-mode magit interleave htmlize helm-bibtexkey ess elfeed-org elfeed-goodies ace-window)))
 '(tool-bar-mode nil)
 '(whitespace-style
   (quote
    (face trailing spaces space-after-tab space-before-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 102 :width normal))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

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

;; Word wrap lines
(setq-default word-wrap t)
(setq-default fill-column 79)

;; Remove unnecessary toolbars, scrollbars, etc
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Use spaces instead of tabs
;; source: http://emacsblog.org/2007/09/30/quick-tip-spaces-instead-of-tabs/
(setq-default indent-tabs-mode nil)

;; Toggle truncation of lines
;; https://stackoverflow.com/a/49692205/
(global-set-key (kbd "C-x w") 'toggle-truncate-lines)

;; Load configs for specific modes -------------------------------------

(require 'init-pkgs)

(require 'init-org)

(require 'init-osx-keys)

(require 'init-ref)

(require 'init-babel)

(require 'init-elfeed)
