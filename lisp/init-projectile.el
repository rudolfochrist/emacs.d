(require-package 'projectile)

(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired
      projectile-completion-system 'ivy)

(global-set-key
 (kbd "C-c C-p")
 (defhydra hydra-project (:color blue :hint nil :idle 0.4)
   "
                                                                    ╭────────────┐
    Files             Search          Buffer             Do         │ Projectile │
  ╭─────────────────────────────────────────────────────────────────┴────────────╯
    [_f_] file          [_a_] ag          [_b_] switch         [_g_] magit
    [_l_] file dwim     [_A_] grep        [_v_] show all       [_p_] commander
    [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info
    [_d_] dir           [_S_] replace     [_K_] kill all
    [_o_] other         [_t_] find tag
    [_u_] test file     [_T_] make tags
    [_h_] root
"
   ("<tab>" hydra-master/body "back")
   ("<ESC>" nil "quit")
   ("a"   projectile-ag)
   ("A"   projectile-grep)
   ("b"   projectile-switch-to-buffer)
   ("B"   projectile-switch-to-buffer-other-window)
   ("c"   projectile-run-async-shell-command-in-root)
   ("C"   projectile-run-command-in-root)
   ("d"   projectile-find-dir)
   ("D"   projectile-find-dir-other-window)
   ("f"   projectile-find-file)
   ("F"   projectile-find-file-other-window)
   ("g"   projectile-vc)
   ("h"   projectile-dired)
   ("i"   projectile-project-info)
   ("kc"  projectile-invalidate-cache)
   ("kd"  projectile-remove-known-project)
   ("kk"  projectile-cache-current-file)
   ("K"   projectile-kill-buffers)
   ("ks"  projectile-cleanup-known-projects)
   ("l"   projectile-find-file-dwim)
   ("L"   projectile-find-file-dwim-other-window)
   ("m"   projectile-compile-project)
   ("o"   projectile-find-other-file)
   ("O"   projectile-find-other-file-other-window)
   ("p"   projectile-commander)
   ("r"   projectile-recentf)
   ("s"   projectile-multi-occur)
   ("S"   projectile-replace)
   ("t"   projectile-find-tag)
   ("T"   projectile-regenerate-tags)
   ("u"   projectile-find-test-file)
   ("U"   projectile-test-project)
   ("v"   projectile-display-buffer)
   ("V"   projectile-ibuffer)))

(provide 'init-projectile)
