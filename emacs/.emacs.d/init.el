;;; init.el --- My main configuration file for Emacs

;;; Commentary:
;; This is where my configuration starts, with some basic configurations to get
;; started.

;;; Code:

(require 'package)

(setq package-enable-at-startup nil)

;; "Speaking as a package maintainer, please do not use MELPA Stable--use
;; regular MELPA" (2020)
;; https://www.reddit.com/r/emacs/comments/etikbz
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

;; Add custom configurations to separate file and load accordingly
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
