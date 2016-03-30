(require-package 'multiple-cursors)

(global-set-key
 (kbd "C-<")
 (defhydra multiple-cursors-hydra (:hint nil)
   "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit

[_i_] Insert numbers \(optional number prefix\)
[_I_] Insert letter \(optinoal number prefix\)
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
("i" mc/insert-numbers)
("I" mc/insert-letters)
  ("q" nil)))

(provide 'init-multiple-cursors)
