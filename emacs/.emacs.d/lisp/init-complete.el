;; General auto-complete
(use-package company
  :ensure t
  :init
  :config
  (global-company-mode t))

;; Completions for academic phrases
(use-package academic-phrases
  :ensure t)

(provide 'init-complete)
