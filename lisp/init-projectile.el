(require-package 'projectile)
(require-package 'ibuffer-projectile)
(require 'recentf)

;;; settings
(setq projectile-completion-system 'ivy)

(recentf-mode 1)
(projectile-global-mode)

;;; hydras
(global-set-key (kbd "C-x C-g") #'projectile-find-file)
(global-set-key (kbd "C-x C-M-g") #'projectile-switch-to-buffer)

(global-set-key
 (kbd "C-x p")
 (defhydra hydra-quick-projectile (:color teal)
   "
PROJECTILE: %(projectile-project-root)
"
   ("f" projectile-find-file "find file")
   ("b" projectile-switch-to-buffer "buffers")
   ("r" projectile-recentf "recent files")
   ("S" projectile-save-project-buffers "save all buffers")
   ("a" projectile-ag "ag")
   ("F" hydra-projectile/body "Show full hydra" :color blue)
   ("q" nil "quit" :color blue)))

(defhydra hydra-projectile (:color teal
                                   :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _K_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                          _S_: Save all buffers  ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("S"   projectile-save-project-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

;;; init ibuffer
(add-hook 'ibuffer-hook (lambda ()
                          (ibuffer-projectile-set-filter-groups)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic))))

(provide 'init-projectile)
