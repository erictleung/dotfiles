;; Miscellaneous small package configurations

;; Setup interactively do things
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point 'guess)
  (ido-mode 1))

;; Quick cursor jump mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-." . ace-jump-mode))

;; Convert buffer text and decorations to HTML
(use-package htmlize
  :ensure t)

;; Another org-mode exporter via pandoc
(use-package ox-pandoc
  :defer t
  :init
  (with-eval-after-load 'org '(require 'ox-pandoc)))

;; Copy-paste yank to clipboard
(use-package xclip
  :ensure t)

;; Interfafce to version control system Git
(use-package magit
  :ensure t)

;; Search and manage bibliographies in Emacs
(use-package helm-bibtex
  :ensure t)

;; Minor mode to interleave notes and textbooks
(use-package interleave
  :ensure t)

;; Org-mode bibliography reference management
(use-package org-ref
  :ensure t)

;; Try out package briefly before committing to them
(use-package try
  :ensure t)

(provide 'init-pkgs)
