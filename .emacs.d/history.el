;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; インクリメンタル検索
;; ---------------------------------------------------------
;; C-s (isearch-forward)
;; C-r (isearch-backward)
;; C-M-s (isearch-forward-regexp)
;; C-M-r (isearch-backward-regexp)

;; ---------------------------------------------------------
;; 置換
;; ---------------------------------------------------------
;; M-% (query-replace)
;; C-M-% (query-replace-regexp)

;; ---------------------------------------------------------
;; enable to pop `mark-ring' repeatedly like C-u C-SPC C-SPC ...
;; http://d.hatena.ne.jp/kbkbkbkb1/20111205/1322988550
;; ---------------------------------------------------------
(setq set-mark-command-repeat-pop t)

;; ---------------------------------------------------------
;; bookmark
;; ---------------------------------------------------------
;; [使い方]
;; C-x r m RET ファイルのポイント位置にブックマークを設定
;; C-x r l     すべてのブックマークを一覧表示
;; M-x set-variable bookmark-default-file で bookmark 切り替え
;;
;; (bookmark モード)
;; e 編集
;; r リネーム
;; k 削除マーク
;; x 削除実行
(require 'bookmark)

;; bookmarkの自動保存
(setq bookmark-save-flag 1)

;; bookmark ファイル
(setq bookmark-default-file "~/local/.emacs.bmk")

;; ブックマーク名を "!バッファ名:行数:関数名" にする。
;; オリジナルの bookmark-buffer-name 関数を上書き。
;; 後述するように、起動時に先頭が"!"となっているブックマークを削除することに注
;; 意。ブックマークを起動時に削除したくない場合は、ブックマーク登録時にミニバッ
;; ファで C-u を押して"!"を削除してから登録するか、 C-x r l で一覧表示して r を
;; 押して先頭の"!"を削除して登録しなおす。
(require 'which-func)
(setq my-bookmark-temp-counter 1000)
(defun bookmark-buffer-name ()
  (cond
   ((string-equal mode-name "Info") Info-current-node)
   ((and (boundp 'dired-directory) dired-directory)
    (format "%s" (buffer-name)))
   (t
    (decf my-bookmark-temp-counter)
    (format "!%03d:%s:%s:%s"
            my-bookmark-temp-counter
            (buffer-name)
            (line-number-at-pos (point)) (or (which-function) ""))
    )))

;; 起動時に先頭が"!"となっているブックマークを削除する。 bookmark-default-file
;; を変更しているときは、事前に bookmark-default-file を設定しておく必要あり。
(require 'cl)
(bookmark-maybe-load-default-file)
(setq bookmark-alist
      (remove-if
       (lambda (x) (string-match "^!" (car x)) ) bookmark-alist))
(bookmark-save)

;; ブックマーク名に関数名を入れたことにより、ブックマーク名が長くなりがちなので、
;; 表示を伸ばす
(setq bookmark-bmenu-file-column 64)

;; オリジナルの bookmark-set に以下の変更を加える。 bookmark.elc がある場合は要
;; バイトコンパイル
;; diff -c bookmark.el bookmark.el.20110526-1635~ 
;; *** bookmark.el	2011-05-30 14:51:42.435107600 +0900
;; --- bookmark.el.20110526-1635~	2009-08-09 21:37:28.001000000 +0900
;; ***************
;; *** 761,769 ****
;;     (setq bookmark-yank-point (point))
;;     (setq bookmark-current-buffer (current-buffer))
  
;; !   (let* ((default (bookmark-buffer-name))
;; ! ;;   (let* ((default (or bookmark-current-bookmark
;; ! ;;                       (bookmark-buffer-name)))
;;   	 (str
;;   	  (or name
;;                 (read-from-minibuffer
;; --- 761,768 ----
;;     (setq bookmark-yank-point (point))
;;     (setq bookmark-current-buffer (current-buffer))
  
;; !   (let* ((default (or bookmark-current-bookmark
;; !                       (bookmark-buffer-name)))
;;   	 (str
;;   	  (or name
;;                 (read-from-minibuffer
;; ***************
;; *** 771,780 ****
;;                  nil
;;                  (let ((now-map (copy-keymap minibuffer-local-map)))
;;                    (define-key now-map "\C-w" 'bookmark-yank-word)
;; !                  ;; (define-key now-map "\C-u" 'bookmark-insert-current-bookmark)
;; !                  (define-key now-map "\C-u" '(lambda ()
;; !                                                (interactive)
;; !                                                (insert default)))
;;                    now-map))))
;;   	 (annotation nil))
;;       (and (string-equal str "") (setq str default))
;; --- 770,776 ----
;;                  nil
;;                  (let ((now-map (copy-keymap minibuffer-local-map)))
;;                    (define-key now-map "\C-w" 'bookmark-yank-word)
;; !                  (define-key now-map "\C-u" 'bookmark-insert-current-bookmark)
;;                    now-map))))
;;   	 (annotation nil))
;;       (and (string-equal str "") (setq str default))

;; ---------------------------------------------------------
;; バックアップ
;; ---------------------------------------------------------
;; バッファを開いて最初のセーブ時にしかバックアップしないことに注意

;; バージョン管理下でもバックアプップ
(setq vc-make-backup-files t)

;; バックアップ先
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/backup/emacs/backup"))
            backup-directory-alist))

;; バージョンを付ける
(setq version-control t)

;; 古いものをいくつ残すか
(setq kept-old-versions 10)

;; 新しいものをいくつ残すか
(setq kept-new-versions 10)

;; 尋ねずに消去
(setq delete-old-versions t)

;; ---------------------------------------------------------
;; ファイルの自動保存
;; ---------------------------------------------------------
;; M-x recover-this-file 復元

;; ---------------------------------------------------------
;; 履歴の保存
;; ---------------------------------------------------------
(require 'savehist)
(setq savehist-file "~/tmp/emacs/savehist")
(setq history-length 1000)
(savehist-mode 1)

;; ---------------------------------------------------------
;; undohist
;; ---------------------------------------------------------
;; ファイル保存後もヒストリを保存
(require 'undohist)
(setq undohist-directory "~/tmp/emacs/undohist")
(setq undohist-ignored-files '("/tmp" "COMMIT_EDITMSG"))
(undohist-initialize)

;; ---------------------------------------------------------
;; save-place
;; ---------------------------------------------------------
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/tmp/emacs/save-places")

;; ---------------------------------------------------------
;; ファイル履歴
;; ---------------------------------------------------------
;; M-x recenf-open-files

;; 保存場所
(setq recentf-save-file "~/tmp/emacs/recentf")

(require 'recentf)
(require 'recentf-ext)

;; 存在しないファイルはcleanupしない
(setq recentf-auto-cleanup 'never)

;; 表示件数
(setq recentf-max-menu-items 300)

;; 保存件数
(setq recentf-max-saved-items 300)

(recentf-mode 1)

;; 自動保存
(setq recentf-auto-save-timer
      (run-with-idle-timer 600 t 'recentf-save-list))

;; ---------------------------------------------------------
;; recentfを拡張
;; ---------------------------------------------------------
;; ディレクトリも 対象とする
;; buffer に切り替えたときもソートする
(require 'recentf-ext)

;; ---------------------------------------------------------
;; Migemo
;; ---------------------------------------------------------
;; [インストール]
;; git clone https://github.com/koron/cmigemo
;; cd cmigemo
;; ./configure --prefix=$HOME/local
;; make gcc
;; make gcc-dict
;; make gcc-install
;; export LD_LIBRARY_PATH=$HOME/local/lib:$LD_LIBRARY_PATH
;;
;; 以前のやり方
;;; qkc をインストール
;; http://hp.vector.co.jp/authors/VA000501/qkcc100.zip
;; make qkc; パスの通ったところにコピー
;;
;; [使い方]
;; M-m で通常検索と入れ替え

;; 基本設定
(when (executable-find "cmigemo")
  (setq migemo-command (executable-find "cmigemo"))
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary (expand-file-name "~/local/share/migemo/utf-8/migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8)

  ;; キャッシュ機能を利用する
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  (setq migemo-pattern-alist-file (expand-file-name "~/tmp/emacs/migemo-pattern"))

  (load-library "migemo")

  ;; 起動時に初期化も行う
  (migemo-init)

  (add-hook 'isearch-mode-hook
            (lambda ()
              (define-key isearch-mode-map "\M-m" 'migemo-isearch-toggle-migemo)))
  )

;; 即時検索できない問題への対処
;; (migemo-pattern-alist-clear)

;; ---------------------------------------------------------
;; isearch
;; ---------------------------------------------------------
;; C-h で検索キー一字削除
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;; \C-d \C-w設定
(define-key isearch-mode-map "\C-d" 'isearch-yank-char)
(define-key isearch-mode-map "\C-w" 'isearch-yank-word)

;; 色
(set-face-foreground 'isearch "white")
(set-face-background 'isearch "red")
(set-face-foreground 'lazy-highlight "white")
(set-face-background 'lazy-highlight "blue")

(defun isearch-yank-char ()
  "Pull next character from buffer into search string with migemo."
  (interactive)
  (when (and (boundp 'migemo-isearch-enable-p) migemo-isearch-enable-p
             (not isearch-regexp) isearch-other-end)
    (setq isearch-string (buffer-substring-no-properties
                          isearch-other-end (point)))
    (setq isearch-message isearch-string))
  (isearch-yank-string
   (save-excursion
     (and (not isearch-forward) isearch-other-end
          (goto-char isearch-other-end))
     (buffer-substring-no-properties (point) (progn (forward-char 1) (point))))))

(defun isearch-yank-word ()
  "Pull next character from buffer into search string with migemo."
  (interactive)
  (when (and (boundp 'migemo-isearch-enable-p) migemo-isearch-enable-p
             (not isearch-regexp) isearch-other-end)
    (setq isearch-string (buffer-substring-no-properties
                          isearch-other-end (point)))
    (setq isearch-message isearch-string))
  (isearch-yank-string
   (save-excursion
     (and (not isearch-forward) isearch-other-end
          (goto-char isearch-other-end))
     (buffer-substring-no-properties (point) (progn (forward-word 1) (point))))))

;; ---------------------------------------------------------
;; auto-save-list
;; ---------------------------------------------------------
(setq auto-save-list-file-prefix "~/tmp/emacs/auto-save-list/saves-")

;; ---------------------------------------------------------
;; hs-minor-mode
;; ---------------------------------------------------------
;; http://d.hatena.ne.jp/kitokitoki/20090716
;; hs-minor-mode で起動
;; C-c @ C-c でブロック単位で表示/非表示

;; hs-minor-mode をデフォルトで有効にするモード
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'conf-mode-hook       'hs-minor-mode)
(add-hook 'apache-mode-hook     'hs-minor-mode)

;; コメントの表示と非常時をトグル
(setq my-hs-hide nil)
(defun my-toggle-hideshow-all ()
  "Toggle hideshow all."
  (interactive)
  (setq my-hs-hide (not my-hs-hide))
  (if my-hs-hide
     (hs-hide-all)
     (hs-show-all)))

;; ---------------------------------------------------------
;; undo-tree
;; ---------------------------------------------------------
;; M-_ redo
;; C-x u undo の履歴を可視化
;; [undo-tree-visualizer mode
;; 今xの場所にいる。
;; sはsaveされているところ
;; dを押してみるとその時の変更のdiffが見れる。
;; もう一度dを押すとdiff表示をoffに出来る。
;; 右のツリーに行きたい時はC-f、左はc-bでtreeのルートが切り替わる
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-_") 'undo-tree-redo)

;; ---------------------------------------------------------
;; clean old buffer
;; ---------------------------------------------------------
 (require 'midnight)
 
