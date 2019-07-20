;;; init-extras.el --- some nice packages I'm not currently using

(use-package aggressive-indent-mode)

(use-package auto-yasnippet
  :disabled
  :bind (("s-w" . aya-create)
         ("s-y" . aya-expand)))

(use-package expand-region
  :disabled
  :bind ("C-=" . er/expand-region))

(use-package key-chord)
(use-package jump-char
  :disabled
  :config
  (key-chord-define-global "fd" 'jump-char-forward)
  (key-chord-define-global "bd" 'jump-char-backward)
  (key-chord-mode t))

(use-package neotree
  :disabled
  :bind ("C-c n" . neotree-toggle)
  :config (setq neo-smart-open t))

(use-package windmove
  :disabled
  :config (windmove-default-keybindings 'shift))

(use-package restclient)

(use-package try)
