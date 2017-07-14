;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; js2-mode
;; ---------------------------------------------------------
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; タブを押すたびにインデントが変わるのを止める
(setq js2-bounce-indent-flag nil)

;; 括弧などを自動挿入しない。insert-bracesを使う
(setq js2-mirror-mode nil)

;; ---------------------------------------------------------
;; my-js-indent-region
;; ---------------------------------------------------------
(defun my-js-indent-region ()
  (interactive)
  (trim-region)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char 0)
      (while (< (point) (point-max))
        (progn
          (js2-indent-line)
          (forward-line))))))

;; ---------------------------------------------------------
;; js hook
;; ---------------------------------------------------------
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq fill-column 78)
             (setq js2-basic-offset 2)

             ;; flycheck
             (flycheck-add-next-checker 'javascript-jshint)
             (flycheck-mode)

             (define-key (current-local-map) "\C-ci" 'my-js-indent-region)
             ))

;; ---------------------------------------------------------
;; HTML hook
;; ---------------------------------------------------------
(add-hook 'sgml-mode-hook
          '(lambda ()
             (setq fill-column 78)
             (setq auto-fill-mode t)
             (setq comment-column fill-column)

             ;; タブ幅を 2 にする
             (setq sgml-indent-step 2)
             (setq tab-width 2)
             (my-tab 2)

             ;; タブをスペースにする
             (setq indent-tabs-mode nil)

             ;; < で <> を書く
             (define-key (current-local-map) "<" 'insert-angle)
             ;; C-c >で region を<>で囲む
             (define-key (current-local-map) "\C-c>" 'insert-angle-region)
             ;; C-c < でクローズタグ
             (define-key (current-local-map) "\C-c<" 'sgml-close-tag)
             ))
