;; Babel Language Configurations

;; active Babel languages
(org-babel-do-load-languages
  'org-babel-load-languages
  '((awk . t)
    (css . t)
    (ditaa . t)
    (dot . t)
    (emacs-lisp . t)
    (gnuplot . t)
    (js . t)
    (latex . t)
    (python . t)
    (R . t)
    (ruby . t)
    (sed . t)
    (shell . t)
    (sql . t)))

;; Remove requirement of confirmation for evaluating
(setq org-confirm-babel-evaluate nil)

(provide 'init-babel)
