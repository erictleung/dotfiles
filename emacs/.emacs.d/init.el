;; Add extra lisp files to path of files to call from
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa" user-emacs-directory))

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
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (ox-pandoc xclip htmlize markdown-mode helm-bibtexkey interleave org-ref ## evil-visual-mark-mode)))
 '(tool-bar-mode nil))
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

;; Word wrap lines
(setq-default word-wrap t)
(setq-default fill-column 79)

;; Set up ido mode
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(ido-mode 1)

;; Remove unnecessary toolbars, scrollbars, etc
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Set up ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC" ) 'ace-jump-mode)

;; Use spaces instead of tabs
;; source: http://emacsblog.org/2007/09/30/quick-tip-spaces-instead-of-tabs/
(setq-default indent-tabs-mode nil)

;; Use Emacs Speaks Statistics (ESS)
;; (require 'ess-site)

;; Enable CUA mode
;; C-x for cut
;; C-c for copy
;; C-v for paste
(cua-mode 1)

;; Load configs for specific modes -------------------------------------

(require 'init-org)

(require 'init-osx-keys)

(require 'init-ref)

(require 'init-babel)
