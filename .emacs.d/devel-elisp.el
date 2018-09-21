;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; デバッグ文挿入
;; ---------------------------------------------------------
(defun my-insert-elisp-debug ()
  (interactive)
;;  (insert-string "(message \"DEBUG %s:%s \" load-file-name (count-lines (point-min) (point-max)))")
  (insert-string "(message \"DEBUG \" )")
  (indent-according-to-mode))

;; ---------------------------------------------------------
;; Hook
;; ---------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (setq fill-column 78)
             (setq auto-fill-mode t)
             (setq comment-column fill-column)
             ;; タブ幅を 4 にする
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (my-tab 4)
             ;; タブをスペースにする
             (setq indent-tabs-mode nil)

             (define-key (current-local-map) "\'" (lambda ()
                                                    (interactive)
                                                    (insert "'")))

             ;; デバッグコメント挿入
             (define-key (current-local-map) "\C-cp" 'my-insert-elisp-debug)

             ;;
             (flycheck-mode 0)
             ))
