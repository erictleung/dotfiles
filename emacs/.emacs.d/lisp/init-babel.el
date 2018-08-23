;; Babel Language Configurations ;;

;; active Babel languages
(org-babel-do-load-languages
  'org-babel-load-languages
  '((R . t)
    (awk .t )
    (dot .t )
    (js .t )
    (sed .t )
    (shell .t )
    (ruby . t)
    (css . t)
    (python . t)))

;; Remove requirement of confirmation for evaluating
(setq org-confirm-babel-evaluate nil)

(provide 'init-babel)
