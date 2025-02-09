;;; init-prog.el --- language specific init

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (progn
    (setq
     js-indent-level 2
     js2-basic-offset 2
     js2-include-node-externs t
     js2-strict-inconsistent-return-warning nil
     js2-strict-trailing-comma-warning nil)))

(custom-set-variables
 '(js2-bounce-indent-p t)
 '(js2-indent-switch-body t))


(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))


(use-package go-mode)


(use-package restclient
  :mode ("\\.api\\'" . restclient-mode))


;; for eglot to handle pyenv and virtualenv
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(require 'eglot)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-go-on-save-hooks)

;; (setq eglot-server-programs
;;       '((python-mode . ("pyright"))))

(defun eglot-go-on-save-hooks ()
  (add-hook 'before-save-hook 'eglot-organize-imports nil t)
  (add-hook 'before-save-hook 'eglot-format-buffer -10 t))

(defun eglot-organize-imports ()
  (call-interactively 'eglot-code-action-organize-imports))
