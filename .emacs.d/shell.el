;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; 非シェルモードキーバインド
;; ---------------------------------------------------------
;; M-! ミニバッファでシェル実行。
;; M-| ミニバッファでシェル実行。リージョンの内容が渡される
;; 結果は、ミニバッファか *Shell Command Output* バッファ
;; に出力される。結果をコピーしたい場合は、*Shell Command Output*
;; からコピー

;; ---------------------------------------------------------
;; シェルモード全般
;; ---------------------------------------------------------
;; エスケープシーケンスを処理する
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; なるべく多くのテキストが見えるようにする
(setq comint-scroll-show-maximum-output t)

;; シェルバッファ上限を 5000行にする
(setq comint-buffer-maximum-size 5000)

;; 連続する同一の入力を履歴に格納しない
(setq comint-input-ignoredups nil)

;; パスワードの入力を隠す
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; 環境変数
(setenv "_TERM_SCREEN" "false")

;; ---------------------------------------------------------
;; シェルモードキーバインド
;; ---------------------------------------------------------
;; よく使うもの
;; M-x shell シェル実行
;; C-c C-r 最後のコマンドの出力の1行目へジャンプ
;; C-c C-o 最後のコマンドの出力を削除（キルリングへ）
;; C-c C-p ひとつ前の出力グループの先頭へジャンプ
;; C-c C-n ひとつ後の出力グループの先頭へジャンプ
;; C-c C-l バッファのシェルコマンド履歴を別のウィンドウに表示する
;; （comint-dynamic-list-input-ring)
;; M-p コマンド履歴一つ前
;; M-n コマンド履歴一つ後
;; M-r 履歴後方検索
;; M-s 履歴前方検索

;; C-c C-d
;; end-of-fileを入力として送ります．通常はシェルあるいはそのサブジョブ
;; を終らせます(shell-send-eof)．
;; C-c C-u
;; 入力として送ろうとしているテキストをすべて削除します
;; (kill-shell-input)．
;; C-c C-w
;; ポイントの前の単語を削除します(backward-kill-word)．
;; C-c C-c
;; シェルまたはそのサブジョブにインタラプトをかけます
;; (interruput-shell-subjob)．
;; C-c C-z
;; シェルまたはそのサブジョブをストップします(stop-shell-subjob)．
;; C-c C-\
;; シェルまたはそのサブジョブにquitシグナルを送ります
;; (quit-shell-subjob)．
;; C-c C-y
;; 直前のシェルへの入力をコピーし，バッファのポイントの前に挿入します
;; (copy-last-shell-input)．最後の改行は挿入されず，RETが入力されるまで
;; コピーされた入力を送ることはしません．
;; C-c C-a
;; 行の先頭に行く。ただし、プロンプトがある場合にはプロンプトの直後に行
;; く（comint-bol）。同じ行でこのコマンドを2回繰り返すと、2回目ではプロ
;; セスマークへ戻る。プロセスマークとは、サブシェルへまだ送っていない入
;; 力の開始位置のこと。（通常、これは同じ場所であり、プロセスマークはそ
;; の行のプロンプトの終りにある。ただし、C-c SPCのあとでは、プロセスマー
;; クはまえの行にあるかもしれない。）
;; C-c C-f
;; シェルコマンド1つ分だけ先へ進めるが、現在行の末尾より先へはいかない
;; （shell-forward-command）。変数shell-command-regexpには、シェルコマ
;; ンドの終りの探し方（正規表現）を指定する。
;; C-c C-b
;; シェルコマンド1つ分だけ手前へ戻るが、現在行の先頭よりまえへはいかな
;; い（shell-backward-command）。
;; M-x dirs
;; シェルにカレントディレクトリを問い合わせ、 Emacs側のものをシェルに合
;; わせる。

;; ---------------------------------------------------------
;; Cygwin の bash を使う場合
;; ---------------------------------------------------------
(when (eq system-type 'cygwin)
  (setenv "LANG" "ja_JP.SJIS")
  (setq explicit-shell-file-name "bash")
  (setq shell-file-name "sh")
  (setq shell-command-switch "-c")
  ;; argument-editing の設定
  (require 'mw32script)
  (mw32script-init)

  (add-hook 'shell-mode-hook
            (lambda ()
              (set-buffer-process-coding-system 'japanese-shift-jis-dos
                                                'japanese-shift-jis-unix)))

  ;; ^M をとる
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

  ;; shell-modeでの補完 (for drive letter)
  (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")
  )

;; shell-command のコマンド入力で補完
;(require 'shell-command)
;(shell-command-completion-mode)

;; ---------------------------------------------------------
;; Shell Mode Hook, Advice
;; ---------------------------------------------------------
(add-hook 'shell-mode-hook
          (lambda ()
            ;; C-aでプロンプトの先頭に移動
            (define-key (current-local-map) "\C-a" 'comint-bol-or-process-mark)

            ;; M-rでコマンド履歴インクリメンタルサーチ
            (define-key shell-mode-map
              [?\M-r] 'comint-previous-matching-input-from-input)

            ;; font-lock をオフ
            (font-lock-mode 0)

            ;; linum モードをオフ
            (linum-mode 0)

            ;; コマンド確定時に色を変えない
            ;; (set-face-foreground 'comint-highlight-input "pink")
            ;; (set-face-foreground 'comint-highlight-prompt "green")
            ))

;; font-lock と linum をオフ (不完全)
(defadvice shell (after my-shell-advice-after)
  ;; font-lock をオフ
  (font-lock-mode 0)

  ;; linum モードをオフ
  (linum-mode 0))
(ad-enable-advice 'shell 'after 'my-shell-advice-after)
(ad-activate 'shell)

;; ---------------------------------------------------------
;; シェルトグル
;; ---------------------------------------------------------
;; shell-toggleは使わずに独自実装。以下はメモとして残しておく
;; M-x shell-toggle
;; shell バッファで M-x shell-toggleで戻る
;; buffer -> shell
;; M-x shell-toggle-cdで shell バッファに切り替え後，
;; 現在編集中のファイルがあるディレクトリに移動
;; (autoload 'shell-toggle "shell-toggle"
;;   "Toggles between the *shell* buffer and whatever buffer you are editing."
;;   t)
;; (autoload 'shell-toggle-cd "shell-toggle"
;;   "Pops up a shell-buffer and insert a \"cd \" command." t)

;; shellトグル独自実装
(defun my-shell-cd (&optional files)
  (interactive)
  (let ((this (selected-window))
        (other (next-window))
        (dir (expand-file-name default-directory))
        cmd)

    ;; シェルモードで、my-shell-cd を実行したときは、ウィンドウを削除
    (if (string= mode-name "Shell")
        (delete-window)

      ;; シェルモード以外

      ;; ウィンドウ分割
      ;; (if (eq this other)
      ;;     (progn
      ;;       (setq my-shell-split-window-flag t)
      ;;       (split-window-vertically))
      ;;   (setq my-shell-split-window-flag nil))

      ;; (unless (eq 0 (nth 1 (window-edges)))
      ;;     (other-window 1))
      (setq my-shell-this-pwd (pwd))
      (shell)
      (setq my-shell-other-pwd (pwd))

      ;; プロンプトの先頭行に移動
      (goto-char (point-max))
      (comint-bol-or-process-mark)

      ;; コマンドラインに残っている文字列を消す
      (delete-region (point) (point-max))

      (when (not (string= my-shell-this-pwd my-shell-other-pwd))
        ;; 頭に1文字空白を付けるのは、cd コマンドをヒストリに残さないため
        ;; ただし、シェルの設定も必要
        (setq cmd (concat " cd " "\'" dir "\'"))
        (insert cmd)
        (comint-send-input))
      (setq cmd "")
      (while files
        (setq cmd (concat cmd " " (car files)))
        (setq files (cdr files)))
      (insert cmd)
      (comint-bol-or-process-mark))
      ))

;; C-c s シェルバッファに移った後、作業中のディレクトリに移動
(global-set-key "\C-cs"  'my-shell-cd)

;; ---------------------------------------------------------
;; 関連づけしたコマンドをシェルプロンプトに展開
;; ---------------------------------------------------------
;; 起動時にシェルバッファを作っておく
(save-excursion
  (let ((current (buffer-name))
        (buffer (get-buffer-create "*shell*")))
    (set-buffer buffer)
    (shell)
    (switch-to-buffer current)))

;; ファイル名と適用するコマンド
(setq my-exec-script-alist
      '(("\\.sh\\'"                "sh")
        ("\\.\\(html\\|htm\\)\\'"  "firefox")
        ("\\.\\(pl\\|pm\\)\\'"     "perl -w")
        ("\\.t\\'"                 "prove -v --timer -w -I '.'")
        ("\\.rb\\'"                "ruby")
        ("\\.py\\'"                "python3")
        ("\\.mk\\|Makefile\\'"     "make -f")
        ("\\.js\\'"                "cscript")
        ("\\.dot\\'"               "dot -Tgif"  " -o tmp.gif && (display tmp.gif &)")
        ("\\.pic\\'"               "pic2png.sh" " tmp.png && (display tmp.png &)")
        ("\\.mp\\'"                "mp2png.sh")
        ("\\.c\\'"                 "gcc -g"     " -o a.out && ./a.out")
        ("\\.cpp\\'"               "g++ -g"     " -o a.out && ./a.out")
        ("\\.R\\'"                 "Rscript --slave --vanilla")))

;; シェルバッファにファイルと関連付けしたコマンドを展開
;; ただし、シェルバッファがあらかじめ存在しないと、動かない？
(defun my-exec-script ()
  (interactive)
  (let ((alist my-exec-script-alist)
        (file (buffer-file-name))
        elt cmds cmd suffix regexp)
    (if (not file)
        (setq file (dired-get-filename)))
    (if file
        (progn
          ;; ファイルをシェルバッファに展開
          (my-shell-cd (list (file-name-nondirectory file)))

          (when (string= (buffer-name) "*shell*")
            ;; ファイルの先頭にコマンドを展開 (shなど)
            (while alist
              (setq elt (car alist)
                    regexp (car elt)
                    alist (cdr alist))
              (if (string-match regexp file)
                  (setq cmds (cdr elt)
                        alist nil)))

            ;; ファイルの後ろに文字列を展開 (-o a.outなど)
            (when cmds
              (setq cmd (car cmds))
              (setq suffix (car (cdr cmds)))
              (insert cmd)
              (end-of-line)
              (if suffix
                  (insert suffix)))))
      ;; ファイル以外のときは、my-shell-cd
      (my-shell-cd))))
;; C-c e スクリプト実行
(global-set-key "\C-ce"  'my-exec-script)

;; C-c ! 編集中のファイル名をシェルバッファに展開
;; dired モードのときのキーバインドの意味が異なる (diredモード参照)
(defun my-file-do-shell ()
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (my-shell-cd (list (file-name-nondirectory file)))
      (my-shell-cd))))
(global-set-key "\C-c!"  'my-file-do-shell)

;; For Windows
;; http://www.emacswiki.org/cgi-bin/wiki/download/cygwin-mount.el
(when (eq system-type 'cygwin)
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  (setq shell-toggle-cygwin-shell t))

;; ---------------------------------------------------------
;; シェルの複数起動
;; ---------------------------------------------------------
;; 複数のサブシェルを使うには、M-x shell を数引数付きで実行します(例え
;; ば C-u M-x shell のように)．実行すると，バンファ名を求められ，そのバッ
;; ファにサブシェルを作成(あるいは，再使用)します．shell の複数起動だけ
;; であれば，M-x shell で shell を起動後，M-x rename-buffer で *shell*
;; 以外に変えておくと，M-x shell とすると新しい shell を起動できます．
;; あるいは，M-x rename-uniquelyで名前を変えてから， shell を起動しても
;; 複数起動できます．

;; ---------------------------------------------------------
;; 環境変数の動的設定
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/syohex/20110418/1303138207
;; ~/bin/create_shell_elisp.pl > ~/tmp/shellenv.el
;; (load-file (expand-file-name "~/tmp/shellenv.el"))
