;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; 直前のコマンドを繰返し実行する
;; ---------------------------------------------------------
(defun my-kill-region (&optional flag)
  (interactive
   (list  current-prefix-arg))
  (let ((beg (region-beginning)) (end (region-end)))
    (if flag
        (delete-region beg end)
      (kill-region beg end))))

;; C-u C-w で kill-ringに保存せずにリージョン削除
(global-set-key (kbd "C-w") 'my-kill-region)

;; ---------------------------------------------------------
;; 直前のコマンドを繰返し実行する
;; ---------------------------------------------------------
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=repeat
;; 例えば, C-k とした後で, C-x z とすると, 直前のコマンド C-k が実行されます.
;; その後, z を押すと, 連続して C-k が実行されます.
;; F5 に割り当ててしまう。
(global-set-key [f5] 'repeat)

;; ---------------------------------------------------------
;; C-gを押したときに現在の入力をヒストリーに記録
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/rubikitch/20091216/minibuffer
;; 同じコマンドを起動してM-pで前の入力を呼び戻せる
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))

;; ---------------------------------------------------------
;; あらゆるモードで有効なキーバインドの設定
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/rubikitch/20101126/keymap
;; http://www.pqrs.org/tekezo/emacs/doc/keyjack-mode/index.html
(setq my-keyjack-mode-map (make-sparse-keymap))

(mapcar (lambda (x)
      ;; (elscreen-set-prefix-key "\C-t")
      (define-key my-keyjack-mode-map (car x) (cdr x))
      (global-set-key (car x) (cdr x)))
    '(
      ;; M-o で他のウィンドウに移動
      ("\M-o" . my-other-window)

      ;; M-g で goto-line
      ("\M-g" . goto-line)

      ;; C-c ; で anything
      ("\C-c;" . anything-migemo)
      ))

(easy-mmode-define-minor-mode my-keyjack-mode "Grab keys"
                  t " Keyjack" my-keyjack-mode-map)

(add-hook 'minibuffer-setup-hook (lambda () (my-keyjack-mode -1)))

;; ---------------------------------------------------------
;; 編集全般
;; ---------------------------------------------------------
;; カーソルが行頭にあるときに、 C-k を 1 回打つだけでその行全体が削除
;; (setq kill-whole-line t)

;; ファイルの最後で自動で行を作らない
(setq next-line-add-newlines nil)

;; Emacsを終了するか問い合わせるようにする。
(setq confirm-kill-emacs 'y-or-n-p)

;; ---------------------------------------------------------
;; 大文字と小文字の変換コマンド
;; ---------------------------------------------------------
;; M-l
;;     ポイントに続く単語を小文字に変換します(downcase-word)．
;; M-u
;;     ポイントに続く単語を大文字に変換します(upcase-word)．
;; M-c
;;     ポイントに続く単語を大文字で始めます(capitalize-word)．
;; C-x C-l
;;     リージョンを小文字にします(downcase-region)．
;; C-x C-u
;;     リージョンを大文字にします(upcase-region)．

;; ---------------------------------------------------------
;; リージョン中の行を反転させる
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/amt/20091127/Reverse
;; M-x reverse-region

;; ---------------------------------------------------------
;; ediff
;; ---------------------------------------------------------
;; M-x ediff-buffers or ediff-files 差分表示
;;   n,p で前後移動 a,b でバッファマージ
;; M-x ediff-directories            ディレクトリ差分表示
;;   ==で一致しているファイルを表示
;; Return                           ファイルに移動
;; ^                                親ディレクトリに移動
;; D                                子ディレクトリに移動
;; M-x ediff-regions-linewise
;; http://www.gside.org/blowg/e/user/tma/entry/200711152051/
;;   リージョンを比較。C-SPACE と M-w で比較対象となる部分を選択。
;;   選択後は C-M-c を押す。そのあと、ediff-buffers

;; ediff ウィンドウを消す
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; 色
(add-hook 'ediff-prepare-buffer-hook
      '(lambda ()
         (set-face-foreground 'ediff-odd-diff-A "darkblue")
         (set-face-background 'ediff-odd-diff-A "white")
         (set-face-foreground 'ediff-odd-diff-B "black")
         (set-face-background 'ediff-odd-diff-B "white")
         ))

;; ---------------------------------------------------------
;; タブ
;; ---------------------------------------------------------
;; インデントに tab を使わない
(setq-default indent-tabs-mode nil)

;; タブ処理
(defun my-tab (width &optional global)
  "Set 'tab-width' and 'tab-stop-list' to first argument.
When second argument is 't', global variable is modified"
  (interactive "nTab Width:\nP")
  (if global
      (progn (setq default-tab-width width)
         (setq tab-stop-list nil))
    (set (make-local-variable 'tab-width) width)
    (set (make-local-variable 'tab-stop-list) nil))
  (let ((n width)
    (max 1024))
    (while (<= n max)
      (setq tab-stop-list (nconc tab-stop-list (list n))
        n (+ n width))))
  t)

;; タブ幅を 4 にする
(my-tab 4 t)

;; ---------------------------------------------------------
;; キー入力
;; ---------------------------------------------------------
;; yes or no を入力しない
(fset 'yes-or-no-p 'y-or-n-p)

;; Home End キー設定
(global-set-key [end]  'end-of-buffer)
(global-set-key [home] 'beginning-of-buffer)

;; C-h で BackSpace
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [f1] 'help-for-help)

;; M-hでbackward-kill-word。M-hのオリジナルは mark-paragraph
(global-set-key "\M-h" 'backward-kill-word)

;; インデント文字を飛ばした行頭に戻る。ただし、ポイントから行頭までの間にインデ
;; ント文字しかない場合は、行頭に戻る。(Windowsライク)
;; http://d.hatena.ne.jp/kitokitoki/20100131/p4
(defun my-beggining-of-line (current-point)
  (interactive "d")
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key "\C-a" 'my-beggining-of-line)

;; 行末に飛ぶ。連続で実行したときは、行末の空白とタブを削除
(defun my-end-of-line ()
  (interactive)
  (if (eq last-command this-command)
      (delete-horizontal-space)
    (end-of-line)))
(global-set-key "\C-e" 'my-end-of-line)

;; インデント時に、自動的に前の行と同じインデント
(setq indent-line-function 'indent-relative-maybe)

;; eval-expresion の使用解除を毎回聞いてこないようにする
(put 'eval-expression 'disabled nil)

;; ---------------------------------------------------------
;; text-adjust
;; ---------------------------------------------------------
;; [インストール]
;; http://taiyaki.org/elisp/text-adjust/
;; http://taiyaki.org/elisp/mell/ から tar.gz アーカイブをダウンロード
;;
;; [使い方]
;; text-adjust-codecheck    半角カナ, 規格外文字を「〓」に置き換える.
;; text-adjust-hankaku      全角英数文字を半角にする.
;; text-adjust-kutouten     句読点を「, 」「. 」に置き換える.
;; text-adjust-space        全角文字と半角文字の間に空白を入れる.
;; text-adjust              上記をすべて実行する.
;; text-adjust-fill         句読点優先で, fill-region をする.
(load "text-adjust")

(defun my-fill-region ()
  (interactive)
  (fill-region (point) (mark))
  (text-adjust-space-region (point) (mark))
  (fill-region (point) (mark)))
(setq fill-column 78)
(global-set-key "\C-cl" 'my-fill-region)

;; text-adjust-save minor モードを定義。このモードになるとファイル保存の前にバッ
;; ファを text-adjust する。
(defvar text-adjust-save-mode nil)
(if (not (assq 'text-adjust-save-mode minor-mode-alist))
    (setq minor-mode-alist
      (cons '(text-adjust-save-mode " text-adjust")
        minor-mode-alist)))

(defun text-adjust-save-mode (&optional arg)
  "text-adjust-save minor-mode"
  (interactive)
  (if text-adjust-save-mode
      (progn
    (remove-hook 'before-save-hook 'text-adjust-space-buffer)
    (setq text-adjust-save-mode nil))
    (add-hook 'before-save-hook 'text-adjust-space-buffer)
    (setq text-adjust-save-mode t)))

(provide 'text-adjust-save)

;; ---------------------------------------------------------
;; 独自改行
;; ---------------------------------------------------------
;; C-m  自動インデント
;; C-j  通常改行
(defun my-indent-relative ()
  (interactive)
  (let (indent)
    (save-excursion
      (previous-line 1)
      (beginning-of-line)
      (if (looking-at "^[ \t]*")
      (setq indent
        (buffer-substring
         (point) (re-search-forward "^[ \t]*")))
    (setq indent "")))
    (beginning-of-line)
    (insert indent)))
(defun my-text-adjust-space ()
  (interactive)
  (save-excursion (mark-paragraph)
          (text-adjust-space-region (region-beginning) (region-end))))
(defun my-newline-and-indent ()
  (interactive)
  (delete-horizontal-space t)
                    ;  (my-text-adjust-space)
  (newline)
  (my-indent-relative))
(global-set-key "\C-m" 'my-newline-and-indent)
(global-set-key "\C-j" 'newline)

;; M-矢印でフレームサイズ変更
(define-key global-map [M-right] 'enlarge-window-horizontally)
(define-key global-map [M-left] 'shrink-window-horizontally)
(define-key global-map [M-up] 'enlarge-window)
(define-key global-map [M-down] 'shrink-window)

;; ---------------------------------------------------------
;; ターミナル上でペースト
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/Tetsujin/20090623/1245690877
(require 'term-paste-mode)

;; ---------------------------------------------------------
;; ホイールマウス設定
;; ---------------------------------------------------------
(global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   1)))

;; Shift
(global-set-key [S-mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key [S-mouse-5] '(lambda () (interactive) (scroll-up   5)))

;; Control
(global-set-key [C-mouse-5] '(lambda () (interactive) (scroll-up   (/ (window-height) 2))))
(global-set-key [C-mouse-4] '(lambda () (interactive) (scroll-down (/ (window-height) 2))))

;; ---------------------------------------------------------
;; 矩形処理
;; ---------------------------------------------------------
;; C-x r t (string-rectangle )
;; 引用したい行を範囲選択し、 C-x r t とすると、
;; ミニバッファに引用記号を聞いてくる

;; ---------------------------------------------------------
;; 自動改行
;; ---------------------------------------------------------
;; fill-column          折り返す文字数
;; ESC XX C-x f         折り返す文字数を XX にする
;; M-q                  現在の段落を詰め込み
;; M-g (fill-region)    リージョン内の段落を詰め込み
;; M-x auto-fill-mode   自動詰め込み
(setq fill-column 78)
(setq auto-fill-mode t)

;; 文字を追加するたびに，自動的に再整形
;; (refill-mode 1)

;; ---------------------------------------------------------
;; 日付
;; ---------------------------------------------------------
(defun my-get-date (form)
  (insert (format-time-string form)))

(defun my-insert-date ()
  (interactive)
  (my-get-date "%Y-%m-%d"))
(global-set-key "\C-ctd" 'my-insert-date)

(defun my-insert-time ()
  (interactive)
  (my-get-date "%H:%M"))
(global-set-key "\C-ctt" 'my-insert-time)

(defun my-insert-full-time ()
  (interactive)
  (my-get-date "%Y-%m-%d %H:%M"))
(global-set-key "\C-ctf" 'my-insert-full-time)

;; ---------------------------------------------------------
;; キーボードマクロ
;; ---------------------------------------------------------
;; C-x (
;;  キーボードマクロの定義を開始する（ start-kbd-macro ）
;; C-x )
;;  キーボードマクロの定義を終了する（ end-kbd-macro ）
;; C-x e
;; もっとも最近のキーボードマクロを実行する（ call-last-kbd-macro ）
;; C-u 5 C-x e
;;  5 回マクロを繰り返す

;; ---------------------------------------------------------
;; 連番
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/rubikitch/20081223/seq
;; (require 'cl)した状態でloopマクロを使う
;; M-: (loop for i from 1 to 3 do (insert (format "(%d)\n" i)))

;; キーボードマクロを使う
;; http://d.hatena.ne.jp/rubikitch/20090722/kmacro
;; まず、連番の初期値 1 を設定する。 C-x C-k C-c を押すと「 Macro counter
;; value 」と聞いてくるので、 1 を入力する。その後は C-x C-k C-i で現在の値を挿
;; 入できる。なので、 C-x ( C-x C-k C-i . SPC RET C-x ) でマクロを定義。あとは
;; <f4>を連打することでどんどん連番を挿入できる。

;; ---------------------------------------------------------
;; テーブルモード
;; ---------------------------------------------------------
;; [元ネタ]
;; http://www.emacswiki.org/emacs/TableMode
;; http://murakan.cocolog-nifty.com/blog/2009/01/emacs-tips-27c2.html
;; http://wassr.jp/user/dummy/statuses/B50uQibW2u
;;
;; [使い方]
;; M-x table-insert
;;  テーブルを挿入する。ミニバッファにカラム数なとが問われる
;;  Number of columns (default 3): (横方向のセル数)
;;  Number of rows (default 3):    (縦方向のセル数)
;;  Cell width(s) (default 5):     (セルの横幅)
;;  Cell height(s) (default 1):    (セルの高さ)
;;
;; M-x table-unrecognize-table
;;  カーソル位置のテーブルの編集モードを抜ける
;;
;; M-x table-recognize-table
;;  カーソル位置のテーブルの編集モードに入る
;;
;; C->, C-c C-c > (table-widen-cell)
;;  カーソル位置のセルを横拡張
;; C-<, C-c C-c < (table-shorten-cell)
;;  カーソル位置のセルを横収縮
;; C-:, C-c C-c : (table-justify)
;;  セル中の文字列の位置を操作 (left/center/right)
;;
;; 縦と横方向の分割と結合
;; M-x table-split-cell
;;  セルの分割 (vertically, horizontally)
;; C--, C-c C-c - (table-split-cell-vertically)
;;  セルの横分割
;; C-|, C-c C-c | (table-split-cell-horizontally)
;;  セルの縦分割
;; C-*, C-c C-c * (table-span-cell)
;;  セルの結合。(left/right/below/above)
;;
;; C-^ (table-generate-source)
;;  HTML, LaTeX, CALS (DocBook DTD) に変換
;;
;; M-x table-capture
;;  リージョンをテーブル化
;; M-x table-releas
;;  カーソル位置にあるテーブルモードのテーブルの罫線を削除

;; ---------------------------------------------------------
;; M-x re-builder
;; ---------------------------------------------------------
;; C-c C-i 正規表現文法切り替え。正規表現(string) or 文字リテラル(read)
;; C-c C-r, C-c C-s ジャンプ
;; C-c C-w 正規表現をコピー。C-yで貼り付け
;; C-c C-q 終了
