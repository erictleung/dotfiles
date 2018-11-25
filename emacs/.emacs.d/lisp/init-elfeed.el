;; Create gloabl binding for elfeed
(global-set-key (kbd "C-x w") 'elfeed)

;; Shortcut functions to certain feeds
;; Need to create these bookmarks manually using C-x r m whenever in the
;; filtered result.
;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
(defun etl/elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))
(defun etl/elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))
(defun etl/elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))
(defun etl/elfeed-show-dev ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-dev"))
(defun etl/elfeed-show-academic ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-academic"))
(defun etl/elfeed-show-microbiome ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-microbiome"))

;; Load database from disk before updating
(defun etl/elfeed-load-db-and-open ()
  "Load the elfeed db from disk before updating."
  (interactive)
  (elfeed)
  (elfeed-db-load)
  (elfeed-search-update--force)
  (elfeed-update))

;; Write to disk when quitting
(defun etl/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;; Use org file to organize RSS feeds
;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory "~/Dropbox/org/elfeed/")
  :bind (:map elfeed-search-mode-map
              ("A" . etl/elfeed-show-all)
              ("E" . etl/elfeed-show-emacs)
              ("D" . etl/elfeed-show-daily)
              ("V" . etl/elfeed-show-dev)
              ("C" . etl/elfeed-show-academic)
              ("M" . etl/elfeed-show-microbiome)
              ("q" . etl/elfeed-save-db-and-bury)))
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/org/elfeed/feed.org")))

(provide 'init-elfeed)
