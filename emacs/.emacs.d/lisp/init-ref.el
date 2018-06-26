;; Org-Reference Configuration ;;

(require 'org-ref)
(require 'interleave)
(require 'helm-bibtex)

;; setup bibliography workflow for notetaking
;; https://www.reddit.com/r/emacs/comments/4gudyw/d2l16uj/
(let ((default-directory "~/Dropbox/org/references/"))
  (setq org-ref-notes-directory (expand-file-name "notes")
        org-ref-bibliography-notes (expand-file-name "articles.org")
        org-ref-default-bibliography (expand-file-name "articles.bib")
        org-ref-pdf-directory "~/Dropbox/zotero/"))

;; setup management of bibliographies
(let ((default-directory "~/Dropbox/org/references/"))
  (setq helm-bibtex-bibliography (expand-file-name "articles.bib")
        helm-bibtex-library-path "~/Dropbox/zotero/"
        helm-bibtex-notes-path (expand-file-name "articles.org")))

;; setup bibliography path
(setq bibtex-completion-bibliography
      '("~/Dropbox/org/references/articles.bib"))

;; setup where PDFs can be found
(setq bibtex-completion-library-path
      '("~/Dropbox/zotero"))

(provide 'init-ref)
