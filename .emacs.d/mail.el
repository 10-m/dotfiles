;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; Mew
;; ---------------------------------------------------------
;; 猫の絵を表示しない
(setq mew-demo-picture nil)
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; 他の elisp との連携
(setq read-mail-command 'mew)
(autoload 'mew-user-agent-compose "mew" nil t)
(setq mail-user-agent 'mew-user-agent)
(define-mail-user-agent
  'mew-user-agent
  'mew-user-agent-compose
  'mew-draft-send-message
  'mew-draft-kill
  'mew-send-hook)

;; パスワードをキャッシュ
(setq mew-use-cached-passwd t)
(setq mew-passwd-timer-unit 60)
(setq mew-passwd-lifetime 14)

;; biff
(setq mew-use-biff t)
(setq mew-use-biff-bell t)
(setq mew-pop-biff-interval 20)

;; 起動時にメールを取得
;; (setq mew-auto-get t)
(setq mew-auto-get nil)

;; スレッドの親子関係を表示
(setq mew-use-fancy-thread t)

;; スレッド間に区切り
(setq mew-use-thread-separator t)

;; メールサイズの制限 (パイト)
(setq mew-pop-size 10000000)

;; MIME 解析するメッセージの大きさの上限 (バイト)
(setq mew-file-max-size 10000000)

;; 未読マークをつける．
(setq mew-use-unread-mark t)

;; シグネチャは常に最後に
;; (setq mew-signature-insert-last t)

;; Date を JST にする
(setenv "TZ" "JST-9")

;; 不明な添付ファイルはバイナリとする
(setq mew-content-type "Application/Octet-Stream")


(add-hook 'mew-init-hook
  '(lambda ()
     ;; mime 追加
     (add-to-list 'mew-mime-content-type
             '("application/zip" "\\.zip$" mew-b64
               mew-prog-octet-stream mew-icon-text))
               ))

(add-hook 'mew-draft-mode-hook
  '(lambda ()
     ;; シグネチャを自動で入れる
     ;; (mew-draft-insert-signature)
     ;; 自動改行
     (refill-mode -1)
     (auto-fill-mode 1)
     (setq fill-column 60)

     ;; 自動スペルチェック
     (flyspell-mode)))

(add-hook 'mew-draft-mode-newdraft-hook
  '(lambda ()
     ;; 定型文書の挿入
     (let ((p (point)))
       (goto-char (point-max))
       (insert-file "~/local/emacs/preface")
       (goto-char (point-max))
       (newline)
       (newline)
       (insert-file "~/local/emacs/signature")
       (goto-char p))))

(add-hook 'mew-summary-mode-hook
  '(lambda ()
     (global-linum-mode t)
     ;; g        メール受信 (オリジナルはi)
     (define-key (current-local-map) "g" 'mew-summary-retrieve)
     ;; i        メールフォルダに移動 (オリジナルはg)
     (define-key (current-local-map) "i" 'mew-summary-goto-folder)))

;; ---------------------------------------------------------
;; 日本語検索
;; ---------------------------------------------------------
(setenv "PERL5LIB"
        (mapconcat 'identity (list
                              "~/.emacs/mew/"
                              (replace-regexp-in-string
                               "^\\(.\\):" "/cygdrive/\\1" (expand-file-name "~/.emacs/mew/"))
                              (getenv "PERL5LIB")) ":"))

(setq mew-prog-grep (expand-file-name "~/.emacs/mew/mg.pl"))
(setq mew-prog-grep-opts '("-j" "jis" "-l" "-e" "-x" "&mime"))
(setq mew-prog-vgrep mew-prog-grep)
(setq mew-prog-vgrep-opts mew-prog-grep-opts)

;; ---------------------------------------------------------
;; 送信確認
;; ---------------------------------------------------------
(defun my-mew-send-check ()
  (let ((subject (mew-header-get-value mew-subj:))
        (to (mew-header-get-value mew-to:))
        (cc (mew-header-get-value mew-cc:))
        (sendit))
    (setq subject (or subject "-"))
    (setq to (or to "-"))
    (setq cc (or cc "-"))
    (setq sendit (y-or-n-p
               (format "Subject: %s\nTo: %s\nCc: %s\nSend ? " subject to cc)))
    (if (not sendit)
        (keyboard-quit))))
(add-hook 'mew-send-hook 'my-mew-send-check)

;; ---------------------------------------------------------
;; Mew キーバインド
;; ---------------------------------------------------------
;; (Summary モード)
;; SPC      メール読む
;; n        次のメールへ
;; p        前のメールへ
;; g        メール受信 (オリジナルはi)
;; i        メールフォルダに移動 (オリジナルはg)
;; a        メール返信
;; A        メールを引用して返信
;; f        メール転送
;; w        メール作成
;; E        メールの再編集
;; j        マークしたメールの結合。分割メールはPマークがついている
;; C-c C-c  メール送信
;; d        削除マーク
;; o        移動マーク
;; *        一時マーク
;; u        マーク取り消し
;; U        マークを指定してマーク取り消し
;; M-u      未読マーク
;; m d      一時マーク一括削除
;; m o      一時マーク一括移動
;; m a      マークされていないメールにマーク
;; N        マークされた次のメールに移動
;; P        マークされた前のメールに移動
;; ?        メール検索。検索結果にマークが付く
;; /        メール検索。検索結果を一時バッファに表示
;; C-c C-a  アドレスブック編集
;; Z        アドレスブックを直接編集したときなど、設定反映
;; .        添付ファイルをメッセージ内で表示する
;; ,        添付ファイルをメッセージ内で表示しない
;; Q        Mew を終了
;; C-u z    メールフォルダのメンテナンス
;; C        mew-config-alist 切り替え
;; t t      スレッド表示と元の表示をトグル
;; t n      次のスレッドへ
;; t p      前のスレッドへ
;; S        ソート
;; z v      省略された To: や Cc: を表示

;; (Draft モード)
;; C-c C-y  メール引用
;; C-c C-a  ファイル添付。ドラック&ドロッグでも添付できる
;; T        添付ファイルのタイプを指定
;; d        添付ファイルの削除

;; (Addrbook モード)
;; C-c C-c  設定保存
;; C-c C-q  設定破棄
