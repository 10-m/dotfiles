;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; サイズ，拡張子で並び換え
;; ---------------------------------------------------------
;; s を何回か入力すると，拡張子やサイズによる並び換え

;; ---------------------------------------------------------
;; Tramp
;; ---------------------------------------------------------
;; C-x C-f (or C-x d) /ssh:user@example.com:/path/to/file
;; C-x C-f (or C-x d) /ssh:user@example.com#port:/path/to/file
;; C-x C-f (or C-x d) /su::/path/to/file
;; C-x C-f (or C-x d) /sudo::/path/to/file
;;
;; (multi hop)
;; C-x C-f (or C-x d) /multi:ssh:user@example.com:su:roo@localhost:/path/to/file
;; C-x C-f (or C-x d) /multi:ssh:user@example.com:sudo:root@localhost:/path/to/file
(setq tramp-auto-save-directory "~/backup/emacs")
(setq tramp-default-method "ssh")
;; (setq-default tramp-shell-prompt-pattern "^[ #$]+")
;; (setq-default tramp-completion-without-shell-p t)
;; (setq-default tramp-debug-buffer t)

;; ---------------------------------------------------------
;; 再帰コピーと再帰削除
;; ---------------------------------------------------------
;; 再帰コピーを許可
(setq dired-recursive-copies 'always)

;; 再帰削除を禁止
(setq dired-recursive-deletes 'nil)

;; ---------------------------------------------------------
;; バックアップ
;; ---------------------------------------------------------
;; 一つのファイルまたはマークされたファイルのバックアップ
(defun my-dired-backup (&optional flag)
  (interactive (list  current-prefix-arg))
  (let* ((files (dired-get-marked-files))
     (date (format-time-string "%Y%m%d-%H%M%S"))
     (dir (expand-file-name "~/backup/emacs")))
    (if (not (file-directory-p dir))
    (make-directory dir))
    (mapc '(lambda (file)
         (let ((backup (format "%s_%s_bak.%s"
                   (file-name-sans-extension file)
                   date
                   (file-name-extension file))))
           (dired-copy-file file backup nil)
           (if (not flag)
           (dired-copy-file backup dir nil))))
      files)
    (revert-buffer)))

;; ---------------------------------------------------------
;; 表示
;; ---------------------------------------------------------
;; ディレクトリを先に表示
(setq ls-lisp-dirs-first t)

;; lsオプション
(setq dired-listing-switches "-AFlth --group-directories-first")

;; M-x grep-dired or M-x find-grep-dired オプション
(setq grep-find-command "find . -type f -print0 | xargs -0 -e grep -ns ")

;; M-x find-dired オプション
(setq find-ls-option '("-exec ls -dAFXlGhn '{}' \\;" . "-dAFXlGhn"))

;; 同一ファイル名のバッファ名を分かりやすく
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
(setq uniquify-min-dir-content 1)

;; ---------------------------------------------------------
;; dired hook
;; ---------------------------------------------------------
(add-hook 'dired-mode-hook
      '(lambda ()

         ;; dired-x を使用
         (require 'dired-x)

         ;; .local.el の ls-lisp-use-insert-directory-program で
         ;; 有効/無効の設定がされることに注意
         (require 'ls-lisp)

         ;; C-h ディレクトリ一階層上がる
         (define-key (current-local-map) [backspace] 'dired-up-directory)

         ;; C-i dired-diplay-file (C-oが漢字変換キーにしていて使えないため)
         (define-key (current-local-map) "\C-i" 'dired-diplay-file)

         ;; c 他の window に コピー
         (define-key (current-local-map) "c"
           (lambda (arg) (interactive "P")
         (let ((dired-dwim-target t)) (dired-do-copy arg))))

         ;; r 他の window に 移動
         (define-key (current-local-map) "r"
           (lambda (arg) (interactive "P")
         (let ((dired-dwim-target t)) (dired-do-rename arg))))

         ;; M-s 他の window に symlink
         (define-key (current-local-map) "\M-s"
           (lambda (arg) (interactive "P")
         (let ((dired-dwim-target t)) (dired-do-symlink arg))))

         ;; C-c w wdired-change-to-wdired-mode
         (define-key (current-local-map) "\C-cw" 'wdired-change-to-wdired-mode)

         (define-key (current-local-map) "q" 'my-dired-do-qiv)
         (define-key (current-local-map) "\C-cq" 'my-dired-do-qiv-chk)
         (define-key (current-local-map) "Q" 'my-dired-do-qbook)
         (define-key (current-local-map) "\M-q" 'my-chk-pic)
         (define-key (current-local-map) "\C-c!" 'my-dired-do-shell)

         ;; ポイントしているディレクトリ下のファイル一覧を同じdiredバッファ内に表示
         ;; C-x C-x でサブディレクトリを表す行に戻れる
         (define-key (current-local-map) "i" 'dired-maybe-insert-subdir)
         (define-key (current-local-map) "I" 'dired-kill-subdir)

         ;; C-oだとIMEが起動するので、v にdired-display-fileを割り当てる
         (define-key (current-local-map) "v" 'dired-display-file)

         ;; マークをゴミ箱へ
         (define-key (current-local-map) "\C-cr" 'my-dired-del-file)

         ;; バックアップ
         (define-key (current-local-map) [f11] 'my-dired-backup)

         ;; zで関連付け実行
         (define-key (current-local-map) "z" 'my-dired-do-exec)

         ;; マークされているときの色
         (set-face-foreground 'dired-marked "cyan")
         (set-face-foreground 'dired-flagged "red")

         (if (not window-system)
         (define-key (current-local-map) "\C-?"         'dired-up-directory))
         ))

;; ---------------------------------------------------------
;; 指定したファイルを非表示
;; ---------------------------------------------------------
;; dired-omit-mode ファイルの表示と非表示をトグル
;; M-o は other-window に割り当てた
(setq dired-omit-files "^\\.$\\|^\\.\\.$\\|\\.bak$|||~$")

;; ---------------------------------------------------------
;; wdired dired バッファを編集
;; ---------------------------------------------------------
;; M-x wdired-change-to-wdired-modeでwdiredモード
;; 編集が終わったら C-x C-s か C-c C-c
;; 変更を適用しない時は C-c C-k で，破棄
(require 'wdired)

;; ---------------------------------------------------------
;; 今週・先週変更したファイルに色をつける
;; ---------------------------------------------------------
(defface face-file-edited-today
  '(
    (((type tty)) (:foreground "cyan" :weight bold))
    (((type w32 pc)) (:foreground "cyan")))
  "dired today")
(defface face-file-edited-this-week
  '(
    (((type tty)) (:foreground "green" :weight bold))
    (((type w32 pc)) (:foreground "green")))
  "dired this week")
(defface face-file-edited-last-week
  '(
    (((type tty)) (:foreground "brightmagenta" :weight bold))
    (((type w32 pc)) (:foreground "pink")))
  "dired last week")
(defvar face-file-edited-today
  'face-file-edited-today)
(defvar face-file-edited-this-week
  'face-file-edited-this-week)
(defvar face-file-edited-last-week
  'face-file-edited-last-week)
(defun my-dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
   (concat "\\(" (format-time-string " %b %e" (current-time))
       "\\|"(format-time-string "%Y-%m-%d" (current-time))
       "\\|"(format-time-string " %m-%d" (current-time))
       "\\)"
       " [0-9]....") arg t))
(defun my-dired-date (time)
  "Fontlock search function for dired."
  (let ((now (current-time))
    (days (* -1 time))
    dateh datel daysec daysh daysl dir
    (offset 0))
    (setq daysec (* -1.0 days 60 60 24))
    (setq daysh (floor (/ daysec 65536.0)))
    (setq daysl (round (- daysec (* daysh 65536.0))))
    (setq dateh (- (nth 0 now) daysh))
    (setq datel (- (nth 1 now) (* offset 3600) daysl))
    (if (< datel 0)
    (progn
      (setq datel (+ datel 65536))
      (setq dateh (- dateh 1))))
    ;;(floor (/ offset 24))))))
    (if (< dateh 0)
    (setq dateh 0))
    ;;(insert (concat (int-to-string dateh) ":"))
    (list dateh datel)))
(defun my-dired-this-week-search (arg)
  "Fontlock search function for dired."
  (let ((youbi
     (string-to-int
      (format-time-string "%w" (current-time))))
    this-week-start this-week-end day ;;regexp
    (flg nil))
    (setq youbi (+ youbi 1))
    (setq regexp
      (concat "\\("))
    (while (not (= youbi 0))
      (setq regexp
        (concat
         regexp
         (if flg
         "\\|")
         (format-time-string " %b %e" (my-dired-date youbi))
         "\\|"
         (format-time-string "%Y-%m-%d" (my-dired-date youbi))
         "\\|"
         (format-time-string " %m-%d" (my-dired-date youbi))
         ))
      ;;(insert (concat (int-to-string youbi) "\n"))
      (setq flg t)
      (setq youbi (- youbi 1))))
  (setq regexp
    (concat regexp "\\)"))
  (search-forward-regexp
   (concat regexp " [0-9]....") arg t))
(defun my-dired-last-week-search (arg)
  "Fontlock search function for dired."
  (let ((youbi
     (string-to-int
      (format-time-string "%w" (current-time))))
    this-week-start this-week-end day ;;regexp
    lyoubi
    (flg nil))
    (setq youbi (+ youbi 0))
    (setq lyoubi (+ youbi 7))
    (setq regexp
      (concat "\\("))
    (while (not (= lyoubi youbi))
      (setq regexp
        (concat
         regexp
         (if flg
         "\\|")
         (format-time-string " %b %e" (my-dired-date lyoubi))
         "\\|"
         (format-time-string "%Y-%m-%d" (my-dired-date lyoubi))
         "\\|"
         (format-time-string " %m-%d" (my-dired-date lyoubi))
         ))
      ;;(insert (concat (int-to-string youbi) "\n"))
      (setq flg t)
      (setq lyoubi (- lyoubi 1))))
  (setq regexp
    (concat regexp "\\)"))
  (search-forward-regexp
   (concat regexp " [0-9]....") arg t))

(add-hook 'dired-mode-hook
      '(lambda ()
         (font-lock-add-keywords
          major-mode
          (list
           '(my-dired-today-search . face-file-edited-today)
           '(my-dired-this-week-search . face-file-edited-this-week)
           '(my-dired-last-week-search . face-file-edited-last-week)
           ))))

;; ---------------------------------------------------------
;; マーク
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/mooz/20090730/p1
;; * %      正規表現を使ってマーク
;; * *      実行可能なファイルをマーク
;; * /      ディレクトリをマーク
;; * @      シンボリックリンクをマーク

;; ---------------------------------------------------------
;; proced (M-x proced)
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/mooz/20090729/p1

;; マーク
;; d,m   マークを付ける
;; M     全てにマークを付ける
;; u     マークを外す
;; U     全てのマークを外す
;; C     子プロセスをマーク
;; P     親プロセスをマーク
;; x,k   マークの付いた (or カーソル下の) プロセスにシグナルを送る
;; t     通常 / ツリー表示を切り替え
;; g     表示を更新

;; ソート
;; s S  proced-sort-interactive
;; s c  proced-sort-pcpu
;; s m  proced-sort-pmem
;; s p  proced-sort-pid
;; s s  proced-sort-start
;; s t  proced-sort-time
;; s u  proced-sort-user

;; ---------------------------------------------------------
;; file extension
;; ---------------------------------------------------------
(setq dired-guess-shell-alist-user
      '(("\\.\\(pdf\\|PDF\\)\\'"   "xpdf")
        ("\\.\\(swf\\|SWF\\)\\'"   "firefox")
        ("\\.\\(html\\|HTML\\)\\'" "firefox")
        ("\\.\\(htm\\|HTM\\)\\'"   "firefox")
        ("\\.\\(tox\\|TOX\\)\\'"   "smplayer")
        ("\\.\\(mpg\\|MPG\\)\\'"   "smplayer")
        ("\\.\\(mpeg\\|MPEG\\)\\'" "smplayer")
        ("\\.\\(avi\\|AVI\\)\\'"   "smplayer")
        ("\\.\\(mp3\\|MP3\\)\\'"   "smplayer")
        ("\\.\\(mp4\\|MP4\\)\\'"   "smplayer")
        ("\\.\\(wmv\\|WMV\\)\\'"   "smplayer")
        ("\\.\\(flv\\|FLV\\)\\'"   "smplayer")
        ("\\.\\(rmvb\\|RMVB\\)\\'" "smplayer")
        ("\\.\\(mkv\\|MKV\\)\\'"   "smplayer")
        ("\\.\\(zip\\|ZIP\\)\\'"   "nautilus")
        ("\\.\\(rar\\|RAR\\)\\'"   "nautilus")
        ("\\.\\(z\\|Z\\)\\'"       "nautilus")
        ("\\.\\(7z\\|7Z\\)\\'"     "nautilus")
        ("\\.\\(lhz\\|LHZ\\)\\'"   "lha l")
        ("\\.\\(jpg\\|JPG\\)\\'"   "qiv")
        ("\\.\\(gif\\|GIF\\)\\'"   "qiv")
        ("\\.\\(png\\|PNG\\)\\'"   "qiv")
        ("\\.\\(bmp\\|BMP\\)\\'"   "qiv")))

(defun my-dired-view ()
  (interactive)
  (let ((alist dired-guess-shell-alist-user)
        (file (dired-get-filename))
        elt cmds)
    (while alist
      (setq elt (car alist)
            regexp (car elt)
            alist (cdr alist))
      (if (string-match regexp file)
          (setq cmds (cdr elt)
                alist nil)))
    (if (not (cdr cmds))
        (start-process "my-dired-do-exec" "*do-exec*" (car cmds) file))))

