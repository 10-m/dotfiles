;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; Insert debug print
;; ---------------------------------------------------------
;; #define DBG_PRINT(...)       {fprintf(stderr, __VA_ARGS__); fflush(stderr);}
;; #define DBG_PRINT(...)       {printf(__VA_ARGS__); fflush(stdout);}
;; #define DBG_PRINT(_ARGS_...) {fprintf(stderr, ## _ARGS_); fflush(stderr);}
;; #define DBG_PRINT(_ARGS_...) {printf(stderr, ## _ARGS_); fflush(stdout);}
;; などの定義必要
(defun my-insert-c-debug ()
  (interactive)
  (insert-string "DBG_PRINT(\"DBG %s %s:%d\\n\", __func__, __FILE__, __LINE__);")
  (indent-according-to-mode))

;; C のコメント挿入
(defun my-insert-c-comment (arg)
  (interactive "p")
  (let()
    (progn
      (insert "/*  */")
      (backward-char 3))))

;; ---------------------------------------------------------
;; font-lock
;; ---------------------------------------------------------
(font-lock-user-keywords
 'c-mode
 '(("!" . font-lock-warning-face)
   ("=" . font-lock-keyword-face)
   ("[0-9]+" . font-lock-constant-face)))

(font-lock-user-keywords
 'c++-mode
 '(("!" . font-lock-warning-face)
   ("=" . font-lock-keyword-face)
   ("[0-9]+" . font-lock-constant-face)))

;; ---------------------------------------------------------
;; c-mode hook
;; ---------------------------------------------------------
(defun my-c-mode-hook ()
  (setq fill-column 78)
  (setq auto-fill-mode t)
  (setq comment-column 32)

  ;; gtags を有効にする
  (gtags-mode 1)

  ;; タブ幅を 4 にする
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (my-tab 4)

  ;; タブをスペースにする
  (setq indent-tabs-mode nil)

  ;; c-offset による字下げ。 C-c C-o で対話的にオフセットを指定できる機能を利用
  ;; してオフセットのシンボルを調べることができる。
  ;; 関数定義を開始する中括弧
  (c-set-offset 'defun-open 0)

  ;; トップレベルの関数定義の最初の行
  (c-set-offset 'defun-block-intro c-basic-offset)

  ;; if、while、for、 do、elseの直後の最初の行
  (c-set-offset 'substatement c-basic-offset)

  ;; substatementのブロックを開始する中括弧
  (c-set-offset 'substatement-open 0)

  ;; case または label のインデント
  (c-set-offset 'case-label c-basic-offset)

  ;; メンバ初期化リストのインデント
  (c-set-offset 'brace-list-intro c-basic-offset)
  (c-set-offset 'brace-list-entry 0)

  ;; Backspcae で空白全削除
  ;; (c-toggle-hungry-state 1)
  ;; 行中の TAB 入力を許可
  (setq c-tab-always-indent nil)

  ;; C-c * で /*  */ を挿入
  (define-key (current-local-map) "\C-c*" 'my-insert-c-comment)

  ;; デバッグコメント挿入
  (define-key (current-local-map) "\C-cp" 'my-insert-c-debug)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; ---------------------------------------------------------
;; GDB
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/higepon/20090505/p1

;; M-x gdb              GDB 起動
;; shell cmd            シェルコマンド cmd を実行
;; file  実行ファイル   実行ファイル再読み込み

;; 表示、変数
;; p 変数名             変数の表示
;; p 変数 = 値          値の代入
;; C-c C-l              プログラムの停止しているソースを表示

;; ブレークポイント
;; b 関数名/ 行番号     ブレークポイント設定
;; C-x space            現在のコード位置にブレークポイント設定
;; i b                  ブレークポイントの表示
;; d number             number のブレークポイント削除

;; 実行
;; C-x C-a C-u          現在のカーソル行まで実行
;; n OR next 1          行実行
;; s OR step            ステップ実行
;; c OR continue        次のブレークポイントまで実行
;; f                    現在のスタックフレームを出るまで

