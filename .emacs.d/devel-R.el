;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; ESS
;; ---------------------------------------------------------
(require 'ess-site)
(add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))

;; ---------------------------------------------------------
;; Hook
;; ---------------------------------------------------------
(add-hook 'R-mode-hook
          '(lambda ()
             ;; tab length
             (setq ess-indent-level 4)
             ;; indent
             (setq ess-arg-function-offset-new-line (list ess-indent-level))
             ;; comment-region by # (default##)
             (make-variable-buffer-local 'comment-add)
             (setq comment-add 0)

             ;; autocomplete
             (setq ess-use-auto-complete t)
             ;; disalbe ido
             (setq ess-use-ido nil)

             ;; Not modify underscore '_'
             (ess-toggle-underscore nil)
             ))
