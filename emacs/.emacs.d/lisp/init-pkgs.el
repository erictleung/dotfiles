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

;; Convert buffer text and decorations to HTML
(use-package htmlize
  :ensure t)

;; Interface to version control system Git
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

;; flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

;; Help display key bindings
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; Browse internet with w3m
;; Help and examples:
;; - http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
(use-package w3m
  :ensure t
  :config
  (setq w3m-use-cookies t
        w3m-cookie-accept-bad-cookies t
        w3m-fill-column 0
        w3m-home-page "https://duckduckgo.com")
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (autoload 'w3m-region "w3m" "Render region in current buffer and replace with result." t)
  ;; UTF-8 everything
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8))

;; Use org-mode for concept mapping
(use-package org-brain
  :ensure t
  :init
  (setq org-brain-path "~/Sync/org/brain")
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-files "~/.emacs.d/.org-id-locations")
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-file-entries-use-title nil)
  (setq org-brain-title-max-length 21))
(defun etl/org-brain-hook ()
  "Miscellaneous keychords for org-brain mode"
  (visual-line-mode)
  (local-set-key (kbd "C-c b u") 'org-brain-update-id-locations)
  (local-set-key (kbd "C-c b s") 'org-brain-switch-brain))
(add-hook 'org-brain-visualize-mode-hook 'etl/org-brain-hook)

;; Create multiple major modes for different langauges
;; Inspired by
;; - https://github.com/SteveLane/dot-emacs/blob/master/packages-polymode.el
;; - http://johnstantongeddes.org/open%20science/2014/03/26/Rmd-polymode.html
(use-package polymode
  :ensure markdown-mode
  :ensure poly-R
  :ensure poly-noweb
  :config
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  )
(use-package poly-markdown
  :ensure polymode
  :defer t
  :config
  ;; Wrap lines at column limit, but don't put hard returns in
  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))
  ;; Flyspell on
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1))))
(use-package poly-R
  :ensure polymode
  :ensure poly-markdown
  :ensure poly-noweb
  :defer t)

;; Help define words
(use-package define-word
  :ensure t
  :config
  (global-set-key (kbd "C-c d") 'define-word-at-point)
  (global-set-key (kbd "C-c D") 'define-word))

;; Quickly select semantically meaningful regions
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(provide 'init-pkgs)
