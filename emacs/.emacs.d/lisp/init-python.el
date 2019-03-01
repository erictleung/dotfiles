;; Python environment settings
;; Notes on using use-package
;; https://github.com/howardabrams/dot-files/blob/master/emacs-python.org
;; RealPython https://realpython.com/emacs-the-best-python-editor/

;; General environment
(use-package elpy
  :ensure t
  :commands
  elpy-enable
  :init
  (with-eval-after-load 'python (elpy-enable))
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  )

;; Auto format Python files using PEP8
(use-package py-autopep8
  :ensure t
  :init
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; Use IPython Notebooks/Jupyter in Emacs
;; Docs: http://millejoh.github.io/emacs-ipython-notebook/
(use-package ein
  :ensure t)

(provide 'init-python)
