;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; ウィンドウの先頭(M-p)、中央(M-r)、最後(M-n)に移動
;; M-r はデフォルトで定義
;; ---------------------------------------------------------
(global-set-key (kbd "M-p") (lambda () (interactive) (move-to-window-line 0)))
(global-set-key (kbd "M-n") (lambda () (interactive) (move-to-window-line -1)))

;; ---------------------------------------------------------
;; 相対的なカーソル位置を動かさないスクロール
;; ---------------------------------------------------------
(setq scroll-preserve-screen-position t)

;; ---------------------------------------------------------
;; カーソル位置を中央にしつつ、一行移動
;; ---------------------------------------------------------
(global-set-key (kbd "C-M-p") (lambda () (interactive) (previous-line 1) (recenter)))
(global-set-key (kbd "C-M-n") (lambda () (interactive) (next-line) (recenter)))

;; ---------------------------------------------------------
;; M-f で単語の先頭に移動する
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/kitokitoki/20091128/p2
;; Mark から開始したときは、オリジナルの forward-word を使用
;; それ以外は単語の先頭に移動する
(defvar my-forward-word-flag nil) ;; Mark から開始したかどうか
(defun my-forward-word (arg)
  (interactive "p")

  ;; M-f 以外だったら、フラグ を nil
  (if (not (eq last-command 'my-forward-word))
      (setq my-forward-word-flag nil))

  (if (or my-forward-word-flag (eq last-command 'set-mark-command))
      (progn
    ;; Mark が起点のときは、フラグを立てて通常の forward-word
    (setq my-forward-word-flag t)
    (forward-word arg))
    (if (looking-at ".$")
    ;; 行末時は、単語の先頭へ移動
    (re-search-forward "\\W\\b")
      (if (looking-at "\\cj")
      (progn
        ;; 日本語の途中のときは、通常の forward-word。日本語の文節移動
        (forward-word arg)
        (if (looking-at "\\(。\\|、\\|．\\|，\\)")
        ;; 。、．，で止まったときは、その後ろへ移動
        (re-search-forward "\[。、．，\]+")))
    ;; 行末以外または日本語の途中以外のときは、行末または単語の先頭に移動
    (re-search-forward "\\(.$\\|\\W\\b\\)")))))
(global-set-key "\M-f" 'my-forward-word)

;; ---------------------------------------------------------
;; 他のウィンドウへ移動。他のウィンドウがなかったら、分割
;; ---------------------------------------------------------
(defun my-other-window ()
  (interactive)
  (when (one-window-p)
    ;; (split-window-horizontally)
    (split-window))
  (other-window 1))

;; ---------------------------------------------------------
;; 移動全般
;; ---------------------------------------------------------
;; bufferの先頭で previous-lineを実行時の警告を抑制
(defun previous-line (arg)
  (interactive "p")
  (if (called-interactively-p "interactive")
      (condition-case nil
      (line-move (- arg))
    ((beginning-of-buffer end-of-buffer)))
    (line-move (- arg)))
  nil)

;; スクロールは一行
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell 用

;; ---------------------------------------------------------
;; 物理行の移動を可能にする
;; ---------------------------------------------------------
(load "physical-line")
(physical-line-on)

;; dired-mode では使わない
(setq physical-line-ignoring-mode-list '(dired-mode))
