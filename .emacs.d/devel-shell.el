;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; Insert debug print
;; ---------------------------------------------------------
(defun my-insert-shell-debug ()
  (interactive)
  (insert-string
   "echo \"DBG pwd=$PWD, line=$LINENO, exit_code=$?, lang=$LANG\"")
  (indent-according-to-mode))

;; ---------------------------------------------------------
;; Hook
;; ---------------------------------------------------------
(add-hook 'sh-set-shell-hook
          '(lambda ()
             ;; デバッグコメント挿入
             (define-key (current-local-map) "\C-cp" 'my-insert-shell-debug)
             ))

