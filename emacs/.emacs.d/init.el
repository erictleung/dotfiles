;;; init.el --- My main configuration file for Emacs

;;; Commentary:
;; This is where my configuration starts with some basic configurations.
;; Near the end, I source other files for my configuration.

;;; Code:

;; Produce trace when error occurs
(setq debug-on-error t)

;; Add extra lisp files to path of files to call from
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa" user-emacs-directory))

; Add custom configurations to separate file and load accordingly
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Inhibit startup screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Add built-in package manager
(require 'package)

;; Add in popular package repositories
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://mirrors.163.com/elpa/gnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

;; Install use-package for easy package management
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scroll slower
(setq scroll-conservatively 100)

;; Stop bell from playing
(setq ring-bell-function 'ignore)

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
  "Make a special 'per session' backup at the first save of each Emacs session."
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

;; Show and highlight matching parentheses
(show-paren-mode 1)

;; Load configs for specific modes -------------------------------------

(require 'init-custom)
(require 'init-pkgs)
(require 'init-complete)
(require 'init-org)
(require 'init-ref)
(require 'init-babel)
(require 'init-elfeed)
(require 'init-ess)
(require 'init-python)
(require 'init-osx-keys)
(require 'ob2dot)

;;; init.el ends here
