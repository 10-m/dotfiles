;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; ESS
;; ---------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)

;; ---------------------------------------------------------
;; Hook
;; ---------------------------------------------------------
(add-hook 'ess-mode-hook
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
