;; Org-Reference Configuration ;;
;; Sources
;; - https://github.com/jkitchin/org-ref/blob/master/org-ref.org
;; - http://kitchingroup.cheme.cmu.edu/blog/2014/05/13/Using-org-ref-for-citations-and-references/
;; - http://kitchingroup.cheme.cmu.edu/blog/2014/05/15/Using-org-ref-to-keep-your-bibtex-files-in-order/

;; Org-mode bibliography reference management
(use-package org-ref
  :ensure t)

;; Minor mode to interleave notes and textbooks
(use-package interleave
  :ensure t)

;; Search and manage bibliographies in Emacs
(use-package helm-bibtex
  :ensure t)

;; setup bibliography workflow for notetaking
;; https://www.reddit.com/r/emacs/comments/4gudyw/d2l16uj/
(let ((default-directory "~/Sync/org/references/"))
  (setq org-ref-notes-directory (expand-file-name "notes")
        org-ref-bibliography-notes (expand-file-name "articles.org")
        org-ref-default-bibliography (expand-file-name "articles.bib")
        org-ref-pdf-directory "~/Sync/zotero/"))

;; setup management of bibliographies
(let ((default-directory "~/Sync/org/references/"))
  (setq helm-bibtex-bibliography (expand-file-name "articles.bib")
        helm-bibtex-library-path "~/Sync/zotero/"
        helm-bibtex-notes-path (expand-file-name "articles.org")))

;; setup bibliography path
(setq bibtex-completion-bibliography
      '("~/Sync/org/references/articles.bib"))

;; setup where PDFs can be found
(setq bibtex-completion-library-path
      '("~/Sync/zotero"))

;; setup auto-formatting of citation
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 3
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 15)

;; Add keybindings for org-ref
(defun etl/org-ref-hook ()
  (visual-line-mode)
  (local-set-key (kbd "C-c r c") 'org-ref-clean-bibtex-entry)
  (local-set-key (kbd "C-c r l") 'crossref-lookup)
  (local-set-key (kbd "C-c r o") 'org-ref-open-bibtex-notes))
(defun etl/interleave ()
  (visual-line-mode)
  (local-set-key (kbd "C-c i m") 'interleave-mode))

;; Setup org-ref useful hooks
(add-hook 'bibtex-mode-hook 'etl/org-ref-hook)
(add-hook 'org-mode-hook 'etl/interleave)

(provide 'init-ref)
