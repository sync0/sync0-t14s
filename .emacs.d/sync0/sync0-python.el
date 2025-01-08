(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :custom
  ;;   (pyvenv-default-virtual-env-name "env")
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "]"))))

(use-package auto-virtualenv
  :after pyenv
  :straight (auto-virtualenv :type git :host github :repo "marcwebbie/auto-virtualenv") 
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  ;; If using projectile
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv))

(use-package python
  :straight nil
  :custom 
  (python-shell-interpreter "python3")
  :config
  ;; (add-hook 'python-mode-hook 'jedi:setup)
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(provide 'sync0-python)
