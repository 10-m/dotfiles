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

;; 時間をモード行に表示
(display-time)

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
;; japanese-holiday.el
;; ---------------------------------------------------------
;; calendar を日本の祝祭日に合わせて使う。
;; 20060829 現在、最新版は、下。
;; http://www.meadowy.org/meadow/netinstall/browser/branches/3.00/pkginfo/japanese-holidays/japanese-holidays.el?rev=799
(add-hook 'calendar-load-hook
          (lambda ()
            (require 'japanese-holidays)
            (setq calendar-holidays
                  (append japanese-holidays local-holidays other-holidays))))
(setq mark-holidays-in-calendar t)

;; “きょう”をマークするには以下の設定を追加します。
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; 日曜日を赤字にする場合、以下の設定を追加します。
(setq calendar-weekend-marker 'diary)
(add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
(add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)

;; 日曜日だけじゃなく、土曜にも色をつける。
(setq calendar-weekend '(0 6))
;; (make-face 'sunday-face)
;; (set-face-foreground 'sunday-face "Tomato")
;; (make-face 'saturday-face)
;; (set-face-foreground 'saturday-face "LightBlue")
;; (setq calendar-sunday-marker 'sunday-face)
;; (setq calendar-saturday-marker 'saturday-face)
;; (setq diary-entry-marker (quote bold-italic))
;;(setq diary-file "~/howm/diary")

;; ---------------------------------------------------------
;; font-lockの設定
;; ---------------------------------------------------------
(load "font-lock")
(global-font-lock-mode t)
(setq font-lock-support-mode 'jit-lock-mode)

;; ---------------------------------------------------------
;; 全角空白、タブ、改行表示モード
;; 切り替えは M-x whitespace-mode
;; ---------------------------------------------------------
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face           ; faceで可視化  
                         trailing       ; 行末
                         tabs           ; タブ
                         ;;empty        ; 先頭/末尾の空行
                         spaces
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-space-regexp "\\( +\\|\u3000+\\)")
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
        (space-mark ?\u3000 [?\u25a1])))

;; ---------------------------------------------------------
;; EOFを表示
;; ---------------------------------------------------------
(defun my-mark-eof ()
  (let ((existing-overlays (overlays-in (point-max) (point-max)))
        (eof-mark (make-overlay (point-max) (point-max) nil t t))
        (eof-text "[EOF]"))
    ;; 旧EOFマークを削除
    (dolist (next-overlay existing-overlays)
      (if (overlay-get next-overlay 'eof-overlay)
          (delete-overlay next-overlay)))
    ;; 新規EOF マークの表示
    (unless (= (point) (point-max))
      (put-text-property 0 (length eof-text)
                         'face '(foreground-color . "slate gray") eof-text)
      (overlay-put eof-mark 'eof-overlay t)
      (overlay-put eof-mark 'after-string eof-text))))

(defun my-hook-eof ()
  ;; 下記2行が必要かどうか要検討
  (add-hook 'pre-command-hook 'my-mark-eof nil t)
  (add-hook 'post-command-hook 'my-mark-eof nil t))

(add-hook 'find-file-hooks 'my-hook-eof)
