(require-package 'hydra)

(global-set-key
 (kbd "C-M-=")
 (defhydra hydra-globals (:hint nil)
   "
Some global mappings.
_q_ : I just changed my mind. Quit.

     Text Scaling
-----------------------
_g_: increase text scale
_l_: decrease text scale
_0_: default text size"
   ("q" nil)
   ("g" text-scale-increase)
   ("l" text-scale-decrease)
   ("0" (text-scale-adjust 0) :color blue)))

(provide 'init-hydra)
