;; Miscellaneous small package configurations

;; Better window management and navigation
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

;; Quick cursor jump mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-." . ace-jump-mode))

;; Setup interactively do things
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point 'guess)
  (ido-mode 1))

;; Search and manage bibliographies in Emacs
(use-package helm-bibtex
  :ensure t)

;; Convert buffer text and decorations to HTML
(use-package htmlize
  :ensure t)

;; Minor mode to interleave notes and textbooks
(use-package interleave
  :ensure t)

;; Interfafce to version control system Git
(use-package magit
  :ensure t)

;; Create major mode for editing Markdown-formatted text
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.txt\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Org-mode bibliography reference management
(use-package org-ref
  :ensure t)

;; Another org-mode exporter via pandoc
(use-package ox-pandoc
  :defer t
  :init
  (with-eval-after-load 'org '(require 'ox-pandoc)))

;; Try out package briefly before committing to them
(use-package try
  :ensure t)

;; Copy-paste yank to clipboard
(use-package xclip
  :ensure t)

;; Improve writing with tips from
;; http://matt.might.net/articles/shell-scripts-for-passive-voice-weasel-words-duplicates/
(use-package writegood-mode
  :ensure t)
(global-set-key "\C-cg" 'writegood-mode)
(global-set-key "\C-c\C-gg" 'writegood-grade-level)
(global-set-key "\C-c\C-ge" 'writegood-reading-ease)

;; Install PDF tools
(use-package pdf-tools
  :ensure t
  :pin manual ;; Manual update so no need to run install command
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

;; Disable linum-mode in pdf-view-mode
(add-hook 'pdf-view-mode-hook (lambda () (linum-mode 0)))

;; ESS for R
(use-package ess
  :ensure t
  :defer 5
  :init (require 'ess-site))

(provide 'init-pkgs)
