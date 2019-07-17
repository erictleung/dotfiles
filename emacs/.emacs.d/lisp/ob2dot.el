;;; ob2dot.el --- 

;; Copyright 2018 cnngimenez
;;
;; Author: cnngimenez
;; Version: $Id: ob2dot.el,v 0.0 2018/07/16 18:31:31 cnngimenez Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'ob2dot)

;;; Code:

(provide 'ob2dot)
(eval-when-compile
  (require 'cl))

(require 'org-brain)

(defconst obdot-temp-dir "~/misc/"
  "Temporal directory where the PNG file is created.
End it with \"/\"."
  ) ;; defconst


(defun obdot--nodedata (node)
  "Recognize if the node is a file or a heading.
Returns a list with two elements, the node name and a color representing 
the type."
  (if (stringp node)
      (list node "green")
    (list (nth 1 node) "blue")
    ) ;; if
  ) ;; defun

(defun obdot-children (entry)
  "Create the dot edges and nodes for the children of NODE."
  (let ((children (org-brain-children entry))
	(output '())
	)

    (dolist (child children)
      (let ((childdata (obdot--nodedata child))
	    )
	(setq output
	      (append output
		      (list "\"" (car childdata) "\" [color="
			    (cadr childdata) "];\n")))
	(setq output
      	      (append output
      		      (list "\"" entry "\" -> \""
			    (car childdata) "\";\n")))
	) ;; let
      ) ;; dolist

    (eval (append '(s-concat) output))
    ) ;; let
  ) ;; defun

(defun obdot-parents (entry)
  "Create the dot edges and nodes for the parents of NODE."
  (let ((parents (org-brain-parents entry))
	(output '())
	)
    (dolist (parent parents)
      (let ((parentdata (obdot--nodedata parent))
	    )

	(setq output
	      (append output
		      (list "\"" (car parentdata) "\" [color="
			    (cadr parentdata) "];\n")))
	(setq output
	      (append output
		      (list "\"" (car parentdata) "\" -> \"" entry "\"")))
	) ;; let
      ) ;; dolist

    (eval (append '(s-concat) output))
    ) ;; let	
  ) ;; defun

(defun obdot-depth-first (entry)
  "Create dots through a depth first search."
  (let ((children (org-brain-children entry))
	(output '())
	)
    (dolist (child children)
      (let ((childdata (obdot--nodedata child))
	    )
	(setq output	      
	      (append output
		      (list "\"" (car childdata) "\" [color=" (cadr childdata)"];\n")
		      (list "\"" entry "\" -> \"" (car childdata) "\";\n")))

	(setq output
	      (append output
		      (list (obdot-depth-first (car childdata)))))
	) ;; let	
      ) ;; dolist

    (eval (append '(s-concat) output))
    ) ;; let
  ) ;; defun


(defun obdot-subtree-dot (entry)
  "Create the subtree dot file of the org-brain ENTRY provided."
  (s-concat "digraph {\n"
	    (obdot-depth-first entry)
	    "}"
	    )
  ) ;; defun

(defun obdot-subtree-diagram (entry)
  "Create a diagram for all the subtree starting from ENTRY."
  (interactive
   (list
    (org-brain-choose-entry
     "Entry: "
     (append (org-brain-files t) (org-brain-headline-entries))
     nil nil)))
  (with-temp-buffer
    (insert (obdot-subtree-dot entry))
    (shell-command-on-region
     (point-min) (point-max)
     (concat "dot -Tpng > " obdot-temp-dir "obdot.png")
    ))

  (find-file (concat obdot-temp-dir "obdot.png"))
  ) ;; defun



(defun obdot-relative-dot (entry)
  "Create the dot string representing the digraph for the ENTRY."
  (s-concat "digraph {\n"
	    (obdot-parents entry)
	    (obdot-children entry)
!	    "}"
	    )
  ) ;; defun

(defun obdot-relative-diagram (entry)
  "Create a diagram for the ENTRY children."
  (interactive
   (list
    (org-brain-choose-entry
     "Entry: "
     (append (org-brain-files t) (org-brain-headline-entries))
     nil nil)))
  
  (with-temp-buffer
    (insert (obdot-relative-dot entry))
    (shell-command-on-region
     (point-min) (point-max)
     (concat "dot -Tpng > " obdot-temp-dir "obdot.png")
    ))

  (find-file (concat obdot-temp-dir "obdot.png"))
  ) ;; defun
  

;;; ob2dot.el ends here
