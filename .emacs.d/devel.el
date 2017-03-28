;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; 共通設定
;; ---------------------------------------------------------
;;  M-x compileでコンパイル時に*compile*を自動でスクロール
(setq compilation-scroll-output t)

;; ---------------------------------------------------------
;; 共通キーバインド
;; ---------------------------------------------------------
;; M-f or C-right forward-word      次の単語へ
;; M-b or C-left backward-word      前の単語へ
;; M-{ or C-up backward-paragraph   前の段落に移動
;; M-} or C-down forward-paragraph  次の段落に移動
;; C-M-a begginin-of-defun          関数の先頭へ移動
;; C-M-e end-of-defun               関数の最後に移動
;; C-M-b backward-sexp              括弧の先頭へ移動
;; C-M-f forward-of-defun           括弧の最後に移動
;; C-x C-x                          マーク位置に飛ぶ
;; M-x indent-region                インデントをそろえる
;; M-x align-regex                  整列。主に代入文で使う
;; M-x align                        整列。主に変数定義や箇条書きで使う

;; ---------------------------------------------------------
;; Hs minor mode
;; ---------------------------------------------------------
;; 関数折り畳み
;; M-x M-x hs-minor-modeか 各モードのhookで(hs-minor-mode 1) を実行
;; C-c @ C-c       hs-toggle-hiding
;; C-c @ C-h       hs-hide-block
;; C-c @ C-l       hs-hide-level
;; C-c @ C-s       hs-show-block
;; C-c @ ESC       Prefix Command
;; C-c @ C-M-h     hs-hide-all
;; C-c @ C-M-s     hs-show-all

;; ---------------------------------------------------------
;; コメントアウト
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/tomoya/20091015/1255593575
;; 1.  transient-mark-mode がオンでリージョンが有効のときに M-; すると、コメン
;;     トアウト、もしくは解除のコマンドになる
;; 2. transient-mark-mode がオンでリージョンが有効のときに C-u 数値 M-; すると、
;;    コメント文字列を数値分にする (下に補足説明あり)
;; 3. 何もない行で M-; した場合、コメント文字列を挿入する
;; 4. 何か書かれている行で M-; した場合、行末にコメント文字列を挿入する
;; 5. コメント行で M-; した場合、コメント文までジャンプする
;; 6. コメント行で、引数を与えて M-; した場合 (C-u M-; という感じ) 、コメント行
;;    であれば削除する。

;; C-c /                            リージョンをコメントアウト
;; (global-set-key "\C-c/" 'comment-dwim)
(global-set-key "\C-c/" 'comment-region)

;; C-c \                            リージョンをアンコメントアウト
(global-set-key "\C-c\\" 'uncomment-region)

;; ---------------------------------------------------------
;; expand-region
;; ---------------------------------------------------------
(global-set-key (kbd "C-M-@") 'er/expand-region)

;; ---------------------------------------------------------
;; brackets,el
;; ---------------------------------------------------------
;; [インストール]
;; http://www.mcl.chem.tohoku.ac.jp/~nakai/emacs/site-lisp/brackets.el
;;
;; オリジナルにinsert-single-quotationを追加した
(load "brackets")

;; ---------------------------------------------------------
;; trim-region
;; ---------------------------------------------------------
(defun trim-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char 0)
      (replace-regexp "[ \t]+$" ""))))

;; ---------------------------------------------------------
;; my-indent-region
;; ---------------------------------------------------------
(defun my-indent-region (start end)
  (interactive "r")
  (untabify start end)
  (trim-region start end)
  (indent-region start end))

;; ---------------------------------------------------------
;; highlight-indentation
;; ---------------------------------------------------------
;; http://blog.iss.ms/2012/03/17/095152
;; (require 'highlight-indentation)
;; (set-face-background 'highlight-indentation-face "darkblue")
;;
;; M-x highlight-indentation-mode
;;  カラムごとにインデントがハイライト
;; M-x highlight-indentation-current-column-mode
;;  現在の行に関連するインデントのみハイライト

;; ---------------------------------------------------------
;; テキストモード
;; ---------------------------------------------------------
;; SKK 対応
;; 元ネタ http://www.bookshelf.jp/soft/meadow_16.html#SEC148
;; SKK 変換候補出力時の折り返しを綺麗にする
;; (setq-default fill-nobreak-predicate 'refill-skk-ignore)
;; (defun refill-skk-ignore ()
;;   (interactive)
;;   (if
;;       (or
;;        skk-henkan-mode
;;        skk-current-rule-tree)
;;       t
;;     nil))
(add-hook 'text-mode-hook
  '(lambda ()
     (font-lock-mode 1)
     (setq fill-column 78)
     (setq auto-fill-mode t)
     (auto-fill-mode t)
     ;; 文字を追加するたびに，自動的に再整形 -> Auto-fillでいいか？
     ;; (refill-mode 1)
     ;; C-c i で my-fill-region
     (define-key (current-local-map) "\C-ci" 'my-fill-region)
     (define-key (current-local-map) "\C-m" 'my-newline-and-indent)))

;; ---------------------------------------------------------
;; imenu
;; ---------------------------------------------------------
;; 関数一覧を表示
;; See also anything-gtags's settings in .anything.el
(require 'imenu)

;; (when (require 'cedet)
;;   (semantic-load-enable-code-helpers))
;; (when (and (require 'semantic)
;;            (require 'semantic-ia)
;;            (require 'semantic-imenu)
;;            (require 'semantic-load))
;;   (semantic-load-enable-code-helpers))

;; ---------------------------------------------------------
;; etags
;; ---------------------------------------------------------
;; 初回に限り， TAB や SPC で補完しようとすると， TAGS ファイルの場所を
;; 聞いてきますので，作った TAGS ファイルを指定します．これで， TAB や
;; SPC での補完を使いながら関数を指定でき，一気に関数定義へ飛ぶことがで
;; きます．
;; etags *.el *.c *.h   タグファイルの作成
;; M-. (find-tag)       タグジャンプ(次の検索結果)
;; M-* (pop-tag-mark)   タグジャンプを戻る
;; C-u M-.              別の同名の関数へジャンプ
;; C-u - M-.            C-u M-.の前へ戻る
;; M-x tags-search      TAGS ファイルに登録されているファイルを全文検索
;; M-x tags-query-replace TAGS ファイルに登録されているファイルを置換
;; M-x visit-tags-table ファイルの切り替え
;; M-x list-tags        関数の一覧を表示
;; M-x tags-apropos     正規表現に一致した関数のみを表示
;; M-x tags-reset-tags-tables タグファイルの情報をリセット
;; M-TAB                関数の補完

;; タグファイルの自動生成
(defadvice find-tag (before c-tag-file activate)
  "Automatically create tags file."
  (let ((tag-file (concat default-directory "TAGS")))
    (unless (file-exists-p tag-file)
      (shell-command "etags *.[ch] *.el .*.el -o TAGS 2>/dev/null"))
    (visit-tags-table tag-file)))

;; ---------------------------------------------------------
;; gtags
;; ---------------------------------------------------------
;; [インストール]
;; http://www.gnu.org/software/global/global.html
;; から、global-XX.tar.gzをダウンロード。
;; ./configure, make, make instal
;; gtags.elをパスを通っているところにコピー
;;
;; [使い方]
;; gtags -v
;; 再帰的に検索して，タグファイルを作成
;; M-x gtags-mode
;; C-cgt - gtags-find-tag
;;  関数の定義元へ移動。ミニバッファで補完が効くので補完入力にも使える
;; C-cgr - gtags-find-rtag
;;  関数を参照元の一覧を表示． RET で参照元へジャンプできる
;; C-cgs - gtags-find-symbol
;;  変数の定義元と参照元の一覧を表示． RET で該当箇所へジャンプできる．
;; C-cgf - gtags-parse-file
;;  カレントバッファのメソッド一覧を見る
;; M-x gtags-find-pattern
;;   マッチした行にジャンプ
;; M-x gtags-find-file
;;  マッチしたファイル名にジャンプ。
;; C-cu - gtags-pop-stack
;; 前のバッファへ戻る
;; M-x gtags-find-pattern
;;  関連ファイルからの検索．
;; M-x gtags-find-tag-from-here
;;  カーソル位置の関数定義へ移動．
;; Specify the root directory of project.
;;  M-x gtags-visit-rootdir

;; See also anything-gtags's settings in .anything.el
(require 'gtags)

(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\C-cgt" 'gtags-find-tag)
         (local-set-key "\C-cgr" 'gtags-find-rtag)
         (local-set-key "\C-cgs" 'gtags-find-symbol)
         (local-set-key "\C-cgu" 'gtags-pop-stack)
         (local-set-key "\C-cgf" 'gtags-parse-file)
         ))

(defun my-global-select-pop-windows (&optional other-window)
   (interactive)
   (let ((prev-buffer (current-buffer)))
     (when (one-window-p)
         (split-window))
     (other-window 1)
     (switch-to-buffer prev-buffer)
     (gtags-select-tag)
     (if other-window
         (other-window -1))
   ))

;; GTAGS SELECT バッファで o を押すと他のウィンドウにに結果を表示。
;; v の場合はカーソルが GTAGS SELECT バッファのまま
(add-hook 'gtags-select-mode-hook
      '(lambda ()
         (define-key (current-local-map) (kbd "C-cgt") 'gtags-find-tag)
         (define-key (current-local-map) (kbd "C-cgr") 'gtags-find-rtag)
         (define-key (current-local-map) (kbd "C-cgs") 'gtags-find-symbol)
         (define-key (current-local-map) (kbd "C-cgu") 'gtags-pop-stack)
         (define-key (current-local-map) (kbd "C-f") 'forward-char)
         (define-key (current-local-map) (kbd "C-b") 'backward-char)
         (define-key (current-local-map) "o" 'my-global-select-pop-windows)
         (define-key (current-local-map) "v" '(lambda ()
                                                  (interactive)
                                                  (my-global-select-pop-windows t)))
         ;; C-t が elscreen の prefix キーと競合するのを防止
         (define-key (current-local-map) (kbd "C-t") nil)
         ))

;; Meadow + Cygwin 環境用の gtags-visit-rootdir
;; ディレクトリ補完なし
(defun my-gtags-visit-rootdir ()
  (interactive)
  (let (cyg_dir drive path)
    (cond
     (running-UNIX
      (gtags-visit-rootdir))
     (running-Meadow
      (setq drive (substring default-directory 0 1))
      (setq path (substring default-directory 2))
      (setq cyg_dir (format "/cygdrive/%s%s" drive path))
      (setq cyg_dir (read-from-minibuffer "GTAGSROOT: " cyg_dir))
      (setenv "GTAGSROOT" cyg_dir)))))

;; ---------------------------------------------------------
;; バージョン管理
;; ---------------------------------------------------------
;; M-x magit-status
;;  ? コマンド一覧
;;  s staging (Add)
;;  c commmit-mode
;;    c commit
;;      C-c C-c done commit
;;  l log-mode
;;    b log all branch
;;      b c checkout new branch
;;  b branch-mode
;;    c checkout new branch
;;  m merge-mode
;;    m merge
;;  Shift-p push-mode
;;    u push
;;  f fetch-mode
;;    a fetch all
;;  shift-f pull-mode
;;    u pull

;; ---------------------------------------------------------
;; vc-annotate
;; ---------------------------------------------------------
;; M-x vc-annotateでバージョン履歴を色で表示
;; 赤が直近、黄色がそれより前、青が昔変更したもの文字化けしたときは、
;; (prefer-coding-system 'XXXX) を適切に設定してやればよいが、元に戻さないと他
;; のファイルとか開けない
