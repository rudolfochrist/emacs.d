(require-package 'hydra)
(require 'windmove)

;;; copied from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
      (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
      (enlarge-window arg)))


(global-set-key
 (kbd "C-M-=")
 (defhydra hydra-globals (:hint nil)
   "
Some global mappings.
_q_ : I just changed my mind. Quit.

     Text Scaling                Window Resizing
-------------------------  ----------------------------
_g_: increase text scale              _k_
_f_: decrease text scale           _h_   _l_
_0_: default text size                _j_
                           _=_: Balance windows
"
   ("q" nil)
   ("g" text-scale-increase)
   ("f" text-scale-decrease)
   ("0" (text-scale-adjust 0) :color blue)
   ("h" (hydra-move-splitter-left 5))
   ("j" (hydra-move-splitter-down 5))
   ("k" (hydra-move-splitter-up 5))
   ("l" (hydra-move-splitter-right 5))
   ("=" balance-windows :color blue)))

(provide 'init-hydra)
