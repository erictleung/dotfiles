;; Custom personal functions

;; Run top within emacs
;; source: https://emacs.stackexchange.com/a/28088/
(defun etl/htop ()
  (interactive)
  (if (get-buffer "*top*")
    (switch-to-buffer "*top*")
    (ansi-term "/bin/bash" "top")
    (comint-send-string "*top*" "top\n")))

(provide 'init-custom)
