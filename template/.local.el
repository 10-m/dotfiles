;; -*- coding: utf-8-unix -*-

(defun my-local-env-first ()
  ;; 素のlsを使う(t)、ls-lispを使う(nil)
  (setq ls-lisp-use-insert-directory-program t)

  ;; ls オプション
  ;; 変更時刻順
  ;; (setq dired-listing-switches "-lAFXGhnt")
  ;; アクセス時刻順
  (setq dired-listing-switches "-AFXlGhntur")

  (cond
   (running-Meadow
    (setq ls-lisp-use-insert-directory-program nil)
    (setq dired-listing-switches "-lAXGhnt")
    (setq load-path (cons "~/local/site-lisp/tramp" load-path))))

  (setq load-path (cons "/usr/local/share/emacs/site-lisp/mew" load-path))

  ;; メール関連
  (setq mew-config-alist
        '(
          ("default"
           ;; 受信フォルダ
           ("inbox-folder" . "+inbox")

           ;; メールアドレスの @ より前（ユーザ名）を指定する。
           ("user"        . "aaa")

           ;; メールアドレスの @ 以降を指定する。
           ("mail-domain" . "bbb")

           ;; POPを利用する場合。APOP の場合は設定する必要はない。
           ("pop-auth"     . pass)
           ("mailbox-type" . pop)

           ;; POPサーバのアカウントを指定する。
           ("pop-user"    . "aaa")

           ;; 利用するPOPサーバを指定する。
           ("pop-server"  . "bbb.co.jp")

           ;; 利用するSMTPサーバ（メールサーバ）を指定する。
           ("smtp-server" . "ccc.co.jp")

           ;; メールを14日間POPサーバに残す
           ("pop-delete"   . 14)

           ;; 常にBCC
           ("dcc"   . "aaa@bbb.co.jp")
           )
          ("local"
           ("inbox-folder" . "+local")
           ("pop-delete"   . nil)
           ("mailbox-type" . mbox)
           ("mbox-command" . "incm")
           ("mbox-command-arg" . (format "-d %s" (getenv "MAIL")))
           )
          ))
  )

(defun my-local-env-end ()
  (interactive)

  ;; 以下のコメントを外した場合、dired 新規バッファを作成しない
  ;; (put 'dired-find-alternate-file 'disabled nil)

  ;;   (setq browse-url-generic-program "firefox")
  ;;   (setq browse-url-browser-function
  ;;         '(("^mailto:" . browse-url-mail)
  ;;           ("." . browse-url-generic)))

  ;; Memo
  (elscreen-screen-nickname "Memo")
  (setq org-default-notes-file (concat org-directory "daily.org"))
  (find-file org-default-notes-file)
  (add-to-list 'org-agenda-files org-default-notes-file)

  ;; Mail
  (elscreen-create)
  (elscreen-screen-nickname "Mail")
  (mew)

  ;; Home
  (elscreen-create)
  (elscreen-screen-nickname "Home")
  (dired "~/")

  ;; Work
  (elscreen-create)
  (elscreen-screen-nickname "Work")

  ;; Elisp
  (elscreen-create)
  (elscreen-screen-nickname "Elisp")
  (dired "~/.emacs.d/")

  ;; Lang
  (elscreen-create)
  (elscreen-screen-nickname "Lang")
  (dired "~/lang/")

  (cond
   (running-UNIX
    ;; Download
    (elscreen-create)
    (elscreen-screen-nickname "DL")
    (dired "~/download/")
    (split-window-vertically)
    (other-window 1)
    (dired "~/toolz/")
    ;; EXPAND
    (elscreen-create)
    (elscreen-screen-nickname "EXPAND")
    (dired "~/download/")
    ;; SHARE
    (elscreen-create)
    (elscreen-screen-nickname "SHARE")
    (dired "~/download/")
    ;; E-WORK
    (elscreen-create)
    (elscreen-screen-nickname "E-WORK")
    (dired "~/download/expand/work")
    )

   (running-Meadow
    ;; Download
    (elscreen-create)
    (elscreen-screen-nickname "DL")
    (dired "c:/Documents and Settings/k00658/デスクトップ/")
    (other-window 1)
    (dired "c:/toolz/")

    ;; Cygwin
    (elscreen-create)
    (elscreen-screen-nickname "Cygwin")
    (dired "c:/cygwin/")

    ;; Doc(Server)
    (elscreen-create)
    (elscreen-screen-nickname "Doc(Server)")
    (dired "y:/20_Runcom/31_BS_Release/")

    ;; Doc(Local)
    (elscreen-create)
    (elscreen-screen-nickname "Doc(Local)")
    (dired "d:/WiMAX/")

    ;; Doc(JRCAD)
    (elscreen-create)
    (elscreen-screen-nickname "Doc(JRCAD)")
    ))
  ;; 最初に戻る
  (elscreen-jump-0))
