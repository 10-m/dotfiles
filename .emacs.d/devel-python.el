;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; jedi
;; ---------------------------------------------------------
(require 'jedi)
(require 'epc)
(require 'python)

(setq jedi:complete-on-dot t)

(define-key jedi-mode-map (kbd "<C-tab>") 'jedi:complete)
(define-key python-mode-map "\C-cd" 'jedi:goto-definition)

;; ---------------------------------------------------------
;; flycheck
;; ---------------------------------------------------------
;; See devel.el

;; ---------------------------------------------------------
;; yapf
;; ---------------------------------------------------------
(require 'py-yapf)

(defun my-py-yapf-region (&optional begin end)
  (interactive "r")
  (shell-command-on-region begin end "yapf"
                             nil t))

;; Tidy buffer
(define-key python-mode-map (kbd "C-c I") 'py-yapf-buffer)

;; Tidy region
(define-key python-mode-map (kbd "C-c i") 'my-py-yapf-region)

;; ---------------------------------------------------------
;; Hook
;; ---------------------------------------------------------
(add-hook 'python-mode-hook
          '(lambda ()
             (setq indent-level 4)
             (setq python-indent 4)
             (setq python-default-offset 4)
             (setq tab-width 4)
             (jedi:setup)
             ))
