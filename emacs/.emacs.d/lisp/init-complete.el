;; General auto-complete
(use-package company
  :ensure t
  :init
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.5)
  (setq company-selection-wrap-around t)
  (global-company-mode t))

;; More quick help
(use-package company-quickhelp
  :ensure t
  :defer t
  :disabled t
  :commands company-quickhelp-mode
  :init
  (progn
    (setq company-quickhelp-idle-delay 0.2)
    (add-hook 'after-init-hook 'company-quickhelp-mode)))

;; Completions for academic phrases
(use-package academic-phrases
  :ensure t)

(provide 'init-complete)
