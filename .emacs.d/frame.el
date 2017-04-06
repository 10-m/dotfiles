;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; フレームに対するキー操作
;; ---------------------------------------------------------
;; [F2] で 2 分割した画面を入れ替える
(defun swap-screen ()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor ()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)

;; [F3]で横分割と縦分割を切り替える
(defun window-toggle-division ()
  "ウィンドウ 2 分割時に、縦分割<->横分割"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "ウィンドウが 2 分割されていません。"))
  (let (before-height (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)

    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))

    (switch-to-buffer-other-window other-buf)
    (other-window -1)))
(global-set-key [f3] 'window-toggle-division)

;; ---------------------------------------------------------
;; ウィンドウ間を簡単に移動
;; ---------------------------------------------------------
;; [使い方]
;; 下のウィンドウへ行きたいと思ったら， Shift+ 下カーソル
;; 右のウィンドウなら， Shift+ 右カーソル
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; ---------------------------------------------------------
;; 分割情報を保存
;; ---------------------------------------------------------
;; [使い方]
;; C-t 0-9:指定した番号のウィンドウへ移動， C-t 0 なら， 0 番へ移動．
;;  ただし，ウィンドウを予め作成しておく必要がある．
;; C-t C-t:直前のウィンドウへ移動
;; C-t g:番号を指定して移動
;; C-t C-f:ファイルを開いて，新規ウィンドウへ移動
;; C-t b:バッファを切り替えて，新規ウィンドウへ移動
;; C-t n;次のタブ
;; C-t p;前のタブ

;; Color
(defface elscreen-tab-background-face
  '((((type x w32 mac) (class color))
     :background "Gray")
    (((class color))
     (:background "Gray" :underline nil)))
  "*Face to fontify background of tab line."
  :group 'elscreen)

(defface elscreen-tab-current-screen-face
  '((((class color))
     (:background "Black" :foreground "Gray" :underline nil)))
  "*Face for current screen tab."
  :group 'elscreen)

(defface elscreen-tab-other-screen-face
  '((((type x w32 mac) (class color))
     :background "Gray" :foreground "Black")
    (((class color))
     (:background "Gray" :foreground "Black" :underline nil)))
  "*Face for tabs other than current screen one."
  :group 'elscreen)

(elscreen-start)

;; prefix key
(elscreen-set-prefix-key "\C-t")

;; C-t C-x b  :ウィンドウの一覧をミニバッファに表示
;; C-t C-x C-b:ウィンドウの一覧をミニバッファに表示
(define-key elscreen-map "\C-xb" 'elscreen-display-screen-name-list)
(define-key elscreen-map "\C-x\C-b" 'elscreen-display-screen-name-list)

;; C-t C-pやC-t C-n:前後のウィンドウへ移動
;; C-t C-→やC-←   :前後のウィンドウへ移動
(define-key elscreen-map [left] 'elscreen-previous)
(define-key elscreen-map [right] 'elscreen-next)

;; C-t C-x C-f:ウィンドウの作成
(define-key elscreen-map "\C-x\C-f" 'elscreen-create)

;; C-t C-x C-w:ウィンドウの名前を設定する
(define-key elscreen-map "\C-x\C-w" 'elscreen-screen-nickname)

;; C-t C-x k:現在のウィンドウを kill
(define-key elscreen-map "\C-xk" 'elscreen-kill)

;; C-t C-t:直前のウィンドウに移動 (トグル)
(define-key elscreen-map "\C-t" 'elscreen-toggle)
(define-key dired-mode-map "\C-t\C-t" 'elscreen-toggle)

;; C-t space:次のウィンドウへ
(define-key elscreen-map [(control space)] [(control @)])
(define-key elscreen-map "\C-@" 'elscreen-next)
(define-key elscreen-map [?\C-\ ] 'elscreen-next)
(define-key elscreen-map "@" 'elscreen-next)
(define-key elscreen-map [?\ ] 'elscreen-next)

;; タブ幅
(setq elscreen-tab-width 6)

;; No Display [X] & [!]
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-create-screen nil)
(setq elscreen-tab-display-control nil)

;; ---------------------------------------------------------
;; ibuffer
;; ---------------------------------------------------------
;; [使い方]
;; バッファリストで C-x d や C-x C-f とすると，
;; その行のファイルがあるフォルダがデフォルトにもなる
(require 'ibuffer)

;; バッファの一覧をカレントバッファにする
;; (global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-x\C-b" 'ibuffer)

;; バッファをアルファベット順で表示する
(setq ibuffer-default-sorting-mode 'alphabetic)

;; 外観の変更
(setq ibuffer-formats
      '((mark modified read-only " " (name 30 30)
              " " (size 6 -1) " " (mode 16 16) " " filename)
        (mark " " (name 30 -1) " " filename)))

;; Gをタイプするとバッファリストを絞り込める
;; gで元に戻せる。というより再表示
(defun Buffer-menu-grep (str)
  (interactive "sregexp:")
  (goto-char (point-min))
  (let (lines)
    (forward-line 2)
    (setq lines (buffer-substring (point-min) (point)))
    (while (re-search-forward str nil t)
      (let ((bol (progn (beginning-of-line) (point)))
	    (eol (progn (forward-line) (point))))
	(setq lines (concat lines (buffer-substring bol eol)))))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert lines))))
(define-key ibuffer-mode-map "G" 'Buffer-menu-grep)

;; R 一致したバッファを削除する
(defun Buffer-menu-grep-delete (str)
  (interactive "sregexp:")
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (while (re-search-forward str nil t)
      (save-excursion
	(ibuffer-mark-for-delete nil)
	)
      (end-of-line))))
(define-key ibuffer-mode-map "R" 'Buffer-menu-grep-delete)

;; C-t が elscreen の prefix キーと競合するのを防止
(define-key ibuffer-mode-map (kbd "C-t") nil)
