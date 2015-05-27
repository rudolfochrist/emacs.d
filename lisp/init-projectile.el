(require-package 'projectile)

(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired
      projectile-completion-system 'ivy)

(provide 'init-projectile)
