;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; help-for-help
;; ---------------------------------------------------------
;; http://www.bookshelf.jp/soft/meadow_11.html
;; キーバインドを調べる
;;   M-x help-for-help b(あるいは[f1] b)
;; あるキーに割り当てられているコマンドを知る
;;   M-x help-for-help c(あるいは[f1] c)
;; コマンドがどのキーに割り当てられているかを知る
;;   M-x help-for-help w(あるいは[f1] w) とした後で，コマンド名を入力
;; チュートリアル
;;   M-x help-for-help t(あるいは[f1] t)
;; ヘルプの表示
;;   M-x help-for-help i(あるいは[f1] i)
;; コマンドの説明を読む
;;   M-x help-for-help f(あるいは[f1] f) の後で，コマンド名を入力
;; コマンド名を断片的にしか覚えていない時
;;   M-x help-for-help a(あるいは [f1] a) の後で，コマンド名の一部を入力
;; 変数の説明と値を知る
;;   M-x help-for-help v(あるいは[f1] v) の後で，変数名を入力
;; 現在のモードの説明を見る
;;   M-x help-for-help m(あるいは[f1] m)
;; 最近入力したキーを表示する
;;   M-x help-for-help l(あるいは[f1] l)
(global-set-key [f1] 'help-for-help)

;; ---------------------------------------------------------
;; Info
;; ---------------------------------------------------------
;; M-x info infoを開始
;; d        Go to the Info directory node.
;; RET      Follow a node reference near point, like <mouse-2>.
;; u        Move "up" from this node.
;; n        Move to the "next" node of this node.
;; p        Move to the "previous" node of this node.
;; SPC      1ページ進む
;; DEL      1ページ戻る
;; q        Quit Info: reselect previously selected buffer.
;; 先頭にローカルinfoを追加
;; (setq Info-default-directory-list
;;       (append Info-default-directory-list (list (expand-file-name "~/info"))))
;; 最後にローカルinfoを追加
;; (setq Info-default-directory-list
;;       (const (list (expand-file-name "~/info") Info-default-directory-list ))

;; ---------------------------------------------------------
;; manを読む (woman)
;; ---------------------------------------------------------
;; M-x woman
;; というか M-x manでいいか...
;; face
(custom-set-faces
 '(woman-bold ((t (:foreground "yellow")))))

(setq woman-cache-filename "~/tmp/emacs/.wmncach.el")

;;フレームを分割しない
(setq woman-use-own-frame nil)

(setq woman-manpath '("/usr/share/man/ja"
                      "/usr/man"
                      "/usr/share/man"
                      "/usr/local/man"
                      "/usr/ssl/man"
                      "/usr/X11R6/man"
                      "~/.linuxbrew/share/man"
                      ))

;; imenu のメニュー作成用の正規表現を日本語にもマッチするように変更
(setq woman-imenu-generic-expression
      '((nil
         "^\\(   \\)?\\([ぁ-んァ-ヴー一-龠ａ-ｚＡ-Ｚ０-９a-zA-Z0-9]+\\)" 2)))

;; ---------------------------------------------------------
;; スペルチェック
;; ---------------------------------------------------------
;; M-x ispell-buffer    バッファ全体をスペルチェック
;; M-x ispell-region    リージョン内をスペルチェック
;; M-$ (ispell-word)    カーソル位置をスペルチェック
;; q                    終了
;; SPC                  何もしないで次に進む
;; i                    プライベート辞書に追加

;; C-c d cで補完 [Dictionary Complete]
;; 補完対象の文字列が短かすぎると補完に失敗することがあるので注意
(global-set-key "\C-cdc" 'ispell-complete-word)

;; ispell の代わりに aspell を使う
(setq ispell-program-name "aspell")

;; Word辞書のパス
;; ~(チルダ)を使うと上手く行かない場合があったので、絶対パスにしておく
(setq ispell-alternate-dictionary "/usr/share/dict/words")

;; 日本語ファイル中の英単語スペルチェックを可能にする
;; http://www.an.econ.kobe-u.ac.jp/~namba/meadow/setup_08.html
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]")))

;; 個人用辞書
;; .aspell.confを使用するため、設定しない
;; (setq ispell-personal-dictionary "~/dict/.ispell-dict-personal")

;; ---------------------------------------------------------
;; 自動スペルチェック
;; ---------------------------------------------------------
;; M-x flyspell-mode 今カーソルがある行に対してスペルチェックを行い, 間
;;                   違っているスペルは色をつけ
;; M-x flyspell-buffer  バッファ全体をスペルチェック
;; M-x flyspell-region  リージョン内のスペルチェック

;; ---------------------------------------------------------
;; SDIC
;; ---------------------------------------------------------
;; [元ネタ]
;; http://www.namazu.org/~tsuchiya/sdic/
;;
;; [インストール]
;; 1.  sdic-2.1.3.tar.gz をダウンロードして、展開して下さい。
;; 2. README と同じディレクトリに「GENE辞書」のデータ
;; gene95.lzh, gene95.tar.gz または gene95.tar.bz2 を置いて下さい。
;; 3. README と同じディレクトリに「EDICT辞書」のデータ edict.gz または
;; edict.bz2 を置いて下さい。
;; 4. ./configure ( ./configure --with-emacs=meadow)
;; ./configure --with-dictdir=~/dict --with-lispdir=~/lib/site-lisp --with-eijirou=~/dict/eijirou --with-waeijirou=~/dict/waeijirou
;; ./configure --with-emacs=$(which emacs) --with-lispdir=~/local/share/emacs/site-lisp --with-eijirou=~/dict/eijirou --with-waeijirou=~/dict/waeijirou --with-dictdir=~/dict
;; インストール先のディレクトリは、configure にオプションを与えることに
;; よって変更できます。
;; 5. make
;; 6. make install (cp -a lisp ~/local/site-lisp/sdicでいいかも)
;; 7. make install-info
;; 8. make dict
;; 9. make install-dict
;; 10. sample.emacs に個人設定のサンプルが入っていますので、
;; それを参考にして各自の .emacs を編集してください。
;; make eedict.dic
;; cp eedict.dic /usr/local/share/dict/
;;
;; http://honmat.cocolog-nifty.com/txt/2008/09/fedora-emacs-sd.html
;; gene.sdicの生成に失敗しているときは、Makefileの no を nkfに変えて、
;; make gene.sdic

;; sdic-mode 用の設定
(load "sdic")
(autoload 'sdic-describe-word
  "sdic" "英単語の意味を調べる" t nil)
;; C-c d wで辞書検索 [Dictionary Words]
(global-set-key "\C-cdw" 'sdic-describe-word)

;; 英和検索で使用する辞書
(setq sdic-eiwa-dictionary-list
      '(
        (sdicf-client
         "~/dict/gene.sdic"
         (sdicf-client
          "~/dict/eedict.sdic")
         )))
;; 和英検索で使用する辞書
(setq sdic-waei-dictionary-list
      '(
        (sdicf-client
         "~/dict/jedict.sdic")
        ))

;; 文字色
(setq sdic-face-color "brightmagenta")
(add-hook 'sdic-mode-hook
          '(lambda ()
             (set-face-foreground 'sdic-face sdic-face-color)))

;; 辞書ファイル形式
(setq sdic-default-coding-system 'euc-japan-unix)

;; ;; ---------------------------------------------------------
;; ;; lookup
;; ;; ---------------------------------------------------------
;; ;; [元ネタ]
;; ;; http://openlab.jp/edict/lookup/  (elisp)
;; ;; http://openlab.ring.gr.jp/edict/fpw/ (辞書)
;; ;; http://www.bookshelf.jp/soft/meadow_53.html#SEC783
;; ;;
;; ;; WEB      : Webster's Revised Unabridged Dictionary (1913)
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/web1913/web1913-fpw1.1.1.zip
;; ;; WORDNET  : WordNet(r) 1.6
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/wordnet/wordnet-1.6-fpw1.1.2.zip
;; ;; ASCDATES : パソコン用語辞典 (1998 年版アスキー DATES 手帳巻末)
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/ASCIIdates98/ASCIIdates98-1.0.zip
;; ;; FUMEIKAI : 不明解略語辞典
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/Fumeikai/Fumeikai-1.0.zip
;; ;; EDICT    : EDICT 和英辞典 (簡易英和検索対応)
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/edict/edict-fpw1.2.1.zip
;; ;; DEVIL    : 惡魔の辭典 (Devil's Dictionary)
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/devil/devil-fpw1.0.2.zip
;; ;; RYAKU    : 略語辞典辞典 (K's Bookshelf)
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/Ryaku/Ryaku-1.0.1.zip
;; ;; VERA     : V.E.R.A. -- Virtual Entity of Relevant Acronyms
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/vera/vera-1.7-fpw1.0.2.zip
;; ;; FOLDOC   : Free On-line Dictionary of Computing
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/foldoc/foldoc-fpw1.0.1.zip
;; ;; WDIC     : 通信用語の基礎知識
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/wdic/wdic-fpw1.4.zip
;; ;; JARGON   : ハッカー用語辞典
;; ;; http://openlab.ring.gr.jp/edict/fpw/dist/jargon/jargon-fpw2.0.zip
;; ;;
;; ;; [インストール]
;; ;; install.el というファイルがあるので，これを Meadow で開き，M-x
;; ;; eval-buffer。または、シェルから
;; ;; cd lookup
;; ;; ./configure --with-emacs=meadow
;; ;;
;; ;; eblockインストール
;; ;; http://openlab.jp/edict/eblook/

;; ;; ロゴを表示しない
;; (setq lookup-enable-splash nil)

;; ; オートロードの設定
;; (autoload 'lookup "lookup" nil t)
;; (autoload 'lookup-region "lookup" nil t)
;; (autoload 'lookup-pattern "lookup" nil t)

;; ;; C-d d dでlookup起動 [Dictionary Dictionary]
;; ;; (define-key ctl-x-map "l" 'lookup)
;; ;; (define-key ctl-x-map "y" 'lookup-region)
;; ;; (define-key ctl-x-map "\C-y" 'lookup-pattern)
;; (global-set-key "\C-cdd" 'lookup-pattern)

;; ;; 辞書関連
;; (setq ndspell-words-dictionary (expand-file-name "~/dict/words"))
;; (setq lookup-search-agents
;;       '((ndeb "~/dict/fpw/WEB/")
;;         (ndeb "~/dict/fpw/WORDNET/")
;;         (ndeb "~/dict/fpw/ASCDATES/")
;;         (ndeb "~/dict/fpw/FUMEIKAI/")
;;         (ndeb "~/dict/fpw/EDICT/")
;;         (ndeb "~/dict/fpw/DEVIL/")
;;         (ndeb "~/dict/fpw/RYAKU/")
;;         (ndeb "~/dict/fpw/VERA/")
;;         (ndeb "~/dict/fpw/FOLDOC/")
;;         (ndeb "~/dict/fpw/WDIC/")
;;         (ndeb "~/dict/fpw/JARGON/")))

;; ;; 文字コードの設定
;; (cond
;;  (running-Meadow
;;   (setq ndeb-process-coding-system 'sjis-dos)
;;   (setq lookup-process-coding-system 'sjis-dos))
;;  (running-UNIX
;;   (setq ndeb-process-coding-system 'euc-jp)
;;   (setq lookup-process-coding-system 'euc-jp)))
