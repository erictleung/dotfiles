;; ESS for R
(use-package ess
  :ensure t
  :defer t
  :config
  (setq ess-style 'RStudio))

;; Smarter underscore when using ESS
(use-package ess-smart-underscore
  :ensure t)

(provide 'init-ess)
