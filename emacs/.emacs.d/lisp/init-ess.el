;; ESS for R
(use-package ess
  :ensure t
  :defer 5
  :init
  (require 'ess-site)
  :config
  (setq ess-style 'RStudio))

;; Smarter underscore when using ESS
(use-package ess-smart-underscore
  :ensure t)

(provide 'init-ess)
