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


(use-package go-mode)


(use-package restclient
  :mode ("\\.api\\'" . restclient-mode))


(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-go-on-save-hooks)

(defun eglot-go-on-save-hooks ()
  (add-hook 'before-save-hook 'eglot-organize-imports nil t)
  (add-hook 'before-save-hook 'eglot-format-buffer -10 t))

(defun eglot-organize-imports ()
  (call-interactively 'eglot-code-action-organize-imports))
