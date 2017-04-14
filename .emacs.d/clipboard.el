;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; クリップボードと同期
;; ---------------------------------------------------------

(setq x-select-enable-clipboard t)

(defun my-cut-function (text &optional rest)
  (interactive)
  (let ((process-connection-type nil))
    (let ((proc (start-process "xsel" "*Messages*" "xsel" "-b" "-i")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun my-paste-function ()
  (interactive)
  (shell-command-to-string "xsel -b -o"))

(when (and (not window-system)
         (executable-find "xsel"))
  (setq interprogram-cut-function 'my-cut-function)
  (setq interprogram-paste-function 'my-paste-function))

;; ---------------------------------------------------------
;; 貼り付けの拡張
;; ---------------------------------------------------------
;; [使い方]
;; C-c y        kill-ring の一覧を表示
;; n  p         前後へ移動
;; y            貼り付け
;; q            終了
;; e            選択中の kill-ring を編集
;; U            間違って貼り付けた場合、取り消し
;; 普通の isearch で検索できる
(require 'browse-kill-ring)
(global-set-key "\C-cy" 'browse-kill-ring)

;; kill-ring を一行で表示
(setq browse-kill-ring-display-style 'one-line)

;; kill-ring の内容を表示する際の区切りを指定する
;;(setq browse-kill-ring-separator "\n-------")
;;(setq browse-kill-ring-separator-face 'separator)

;; 区切り文字のフェイスを指定する
(setq browse-kill-ring-separator-face 'region)

;; browse-kill-ring 終了時にバッファを kill する
(setq browse-kill-ring-quit-action 'kill-and-delete-window)

;; 必要に応じて browse-kill-ring のウィンドウの大きさを変更しない
(setq browse-kill-ring-resize-window nil)

;; 現在選択中の kill-ring のハイライトする
(setq browse-kill-ring-highlight-current-entry t)

;; 一覧で表示する文字数を指定する． nil ならすべて表示される．
(setq browse-kill-ring-maximum-display-length 100)
