;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; Occur
;; ---------------------------------------------------------
;; M-x occur            バッファに対する検索結果を一覧表示
;; e                    編集モードに移行
;; M-x multi-occur      複数バッファを検索
;; M-x multi-occur-in-matching-buffers 正規表現でバッファを指定

;; ---------------------------------------------------------
;; color-moccur
;; ---------------------------------------------------------
;; M-x occur-by-moccur  カレントバッファを検索
;; M-x moccur-grep      grep のようにファイルを検索 (正規表現)
;; M-x moccur-grep-find
;;                      grep+find のようにファイルを検索 (正規表現)
;; M-x search-buffers   すべてのバッファを全文検索．検索語はスペースで区切る
;; M-x grep-buffers     開いているファイルを対象に grep ．
;; バッファリストで M-x Buffer-menu-moccur
;;                      m でマークをつけたバッファのみを対象に検索
;; dired で M-x dired-do-moccur
;;                      m でマークをつけたファイルのみを対象に検索
;; moccur の結果で s     一致したバッファのみで再検索

;; ---------------------------------------------------------
;; moccur-edit
;; ---------------------------------------------------------
;; moccur をし，一覧を表示
;; 結果が表示されたところで， r (あるいは C-c C-i か C-x C-q でもいい) と
;; します．すると，バッファが編集できるようになります C-x C-s (あるいは
;; C-c C-c か C-c C-f でも可能) とすると，色がついている変更のみが適用
;; すべての変更を適用したくない時には， C-x k (あるいは C-c C-k か C-c k
;; か C-c C-u でも可能)
(require 'moccur-edit)

;; moccur-edit-finish-editと同時にファイルを保存する
(defadvice moccur-edit-change-file
  (after save-after-moccur-edit-buffer activate)
  (save-buffer))

;; ---------------------------------------------------------
;; wgrep
;; ---------------------------------------------------------
(require 'wgrep nil t)

;; r 編集開始
(setf wgrep-enable-key "r")

;; C-x C-xで保存
;; C-x k で保存せずに終了
(define-key wgrep-mode-map "\C-xk" 'wgrep-abort-changes)

;;; wgrep終了時にバッファを保存
(setq wgrep-auto-save-buffer t)
