;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; 表示全般
;; ---------------------------------------------------------
;; 背景は黒
(custom-set-faces
 '(default ((t (:background "#111111" :foreground "#EEEEEE"))))
 ;;カーソル
 '(cursor (
           (((class color) (background dark )) (:background "#2e8b57"))
           (((class color) (background light)) (:background "#999999"))
           (t ())
           )))
;; フレーム透過設定
(add-to-list 'default-frame-alist '(alpha . (0.90 0.90)))

;; マウスカーソルを消す設定
(mouse-avoidance-mode 'banish)

;; 括弧のハイライト表示
(show-paren-mode t)
(when (not window-system)
  (set-face-foreground 'show-paren-match "white")
  (set-face-background 'show-paren-match "red"))

;; mode line
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "black")
(set-face-foreground 'mode-line-inactive "black")
(set-face-background 'mode-line-inactive "white")

;; タイトルバーにファイル名を表示
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; 行・桁番号を表示
(line-number-mode t)
(column-number-mode t)

;; スクロールバーの位置
(when (window-system)
  (scroll-bar-mode t)
  (scroll-bar-mode 'right))

(require 'tool-bar)

;; ツールバーを表示しない
(tool-bar-mode 0)

;; カーソルを点滅させない
;; コンソールではターミナルソフトに設定が必要
(blink-cursor-mode 0)

;; 編集行をハイライト
(setq hl-line-face 'underline)
(global-hl-line-mode t)

;; ターミナルの時、メニューバーを表示しない
;; M-`かF10でミニバッファに呼び出せる
(if (not window-system)
    (menu-bar-mode -1))

;; 起動時画面を表示しない
(setq inhibit-startup-message t)

;; 画面のフラッシュ
(if (not window-system)
    (setq visible-bell nil)
  (setq visible-bell t))

;; x-faceの起動画面を表示しない
(setq x-face-inhibit-loadup-splash t)

;; ヒントをミニバッファで表示するように変更
(setq tooltip-use-echo-area t)

;; 現在の関数名をモードラインに表示
(require 'which-func)
(which-func-mode t)
(which-function-mode t)

;; マウスの右クリックでメニューを出す
(defun bingalls-edit-menu (event)
  (interactive "e")
  (popup-menu menu-bar-edit-menu))
(global-set-key [mouse-3] 'bingalls-edit-menu)

;; face 名の調査
;; M-x list-faces-display で一覧表示でも調査できる
(defun my-get-face-at-point ()
  (interactive)
  (message (prin1-to-string (get-char-property (point) 'face))))

;; 設定ファイルを見やすく
 (require 'generic-x)

;; 行間指定
(setq-default line-spacing 0.2)

;; 縦分割防止
(setq split-width-threshold nil)

;; ---------------------------------------------------------
;; font-lock-user-keywords
;; http://d.hatena.ne.jp/buzztaiki/20111209/1323444755
;; 設定するときは font-lock-add-keywords と同じように使うだけ。
;; リセットするときは (font-lock-user-keywords 'c-mode) として
;; 呼んであげれば元に戻ってくれます
;; ---------------------------------------------------------
(defun font-lock-user-keywords (mode &optional keywords)
  "Add user highlighting to KEYWORDS to MODE.
See `font-lock-add-keywords' and `font-lock-defaults'."
  (unless mode
    (error "mode should be non-nil "))
  (font-lock-remove-keywords mode (get mode 'font-lock-user-keywords))
  (font-lock-add-keywords mode keywords)
  (put mode 'font-lock-user-keywords keywords))

;; ---------------------------------------------------------
;; transient-mark の色
;; ---------------------------------------------------------
(set-face-background 'region "blue")
(set-face-foreground 'region "white")

;; ---------------------------------------------------------
;; popwin.el
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/m2ym/20110120/1295524932
;; (require 'popwin)
;; (setq display-buffer-function 'popwin:display-buffer)

;; ---------------------------------------------------------
;; 折り返し表示
;; ---------------------------------------------------------
;; http://en.yummy.stripper.jp/?eid=1317446

(setq truncate-lines t)                 ;; 通常時
(setq truncate-partial-width-windows t) ;; 縦分割時

;; 折り返しする/しないをトグルで切り替える
(defun my-toggle-truncate-lines ()
    "折り返し表示をトグル"
    (interactive)
    (if truncate-lines
        (setq truncate-lines nil)
        (setq truncate-lines t))
    (recenter))

;; ---------------------------------------------------------
;; 行番号の表示
;; ---------------------------------------------------------
;; http://stud4.tuwien.ac.at/~e0225855/linum/linum.el
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d ")

;; ---------------------------------------------------------
;; ハイライト表示
;; ---------------------------------------------------------
;; M-x highlight-phrase
;; Phrase to hilight: foo RET          ; 色を付けたいフレーズを指定
;; Highlight using face: hi-yellow RET ; 色を選ぶ
;;
;; M-x highlight-lines-matching-regexp
;; 正規表現にマッチした行を丸ごと色付け
;;
;; M-x unhighlight-regexp
;; ハイライト表示の取り消し
(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")

;; ---------------------------------------------------------
;; iimage-mode
;; ---------------------------------------------------------
;; M-x iimage-mode  画像をインライン表示
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)

;; ---------------------------------------------------------
;; font-lockの設定
;; ---------------------------------------------------------
(load "font-lock")
(global-font-lock-mode t)
(setq font-lock-support-mode 'jit-lock-mode)
(set-face-attribute 'font-lock-constant-face nil
                    :foreground "white"
                    :background "black"
                    :weight 'bold
                    :underline nil)

;; ---------------------------------------------------------
;; 全角空白、タブ、改行表示モード
;; 切り替えは M-x whitespace-mode
;; ---------------------------------------------------------
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face
                         spaces
                         space-mark
                         tabs
                         tab-mark
                         ))
(setq whitespace-space-regexp "\\( +\\|\u3000+\\)")
(setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])))
(set-face-attribute 'whitespace-tab nil
                    :foreground "blue"
                    :background "black"
                    :underline t)

;; ---------------------------------------------------------
;; Time
;; ---------------------------------------------------------
(display-time)

(setq display-time-string-forms
      '((format "%s:%s" 24-hours minutes)
        load
        (if mail " Mail" "")))

;; 24 hour format
(setq display-time-24hr-format t)
