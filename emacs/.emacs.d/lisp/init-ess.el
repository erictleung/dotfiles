;; ESS for R
(use-package ess
  :ensure t
  :defer 5
  :init
  (require 'ess-site)
  :config
  (setq ess-style 'RStudio))

(provide 'init-ess)
