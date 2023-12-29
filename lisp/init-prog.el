;;; init-prog.el --- language specific init

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
