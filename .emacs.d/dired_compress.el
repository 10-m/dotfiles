;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; 自動圧縮モード (pkunzipは使わない)
;; ---------------------------------------------------------
;; (require 'jka-compr)
(auto-compression-mode 1)
(setq archive-zip-use-pkzip nil)

;; ---------------------------------------------------------
;; ファイルの圧縮/展開
;; ---------------------------------------------------------
;; Z でファイルの圧縮と展開
(define-key dired-mode-map "Z" 'my-dired-do-decide-compress)

;; 圧縮ファイルと見なすファイル拡張子
(setq my-compress-extension-list
      (list
       "\\.zip$"
       "\\.tar$"
       "\\.tgz$"
       "\\.gz$"
       "\\.lzh$"
       "\\.bz2$"
       "\\.z$"))

;; 圧縮時のデフォルト拡張子
(setq my-compress-default-extension ".zip")

;; ファイルを圧縮するか展開するか判定する
(defun my-dired-do-decide-compress ()
  (interactive)
  (let ((files (dired-get-marked-files))
	(file)
	(extension-list my-compress-extension-list)
	(extention))

    (setq file (car files))

    ;; ファイルが複数選択されているときは圧縮する
    (if (< 1 (length files))
	(my-dired-do-compress files)

      ;; ファイルが一つだけ選択されていて拡張子が圧縮ファイルであるときは展開する
      (catch 'break
	(while extension-list
	  (setq extension (car extension-list)
		extension-list (cdr extension-list))
	  (when (string-match extension (downcase file))
	    (my-dired-do-uncompress file)
	    (throw 'break t))))

      ;; ファイルが一つだけ選択されていて拡張子が圧縮ファイルでないときは圧縮する
      (if (= 0 (length extension-list))
	  (my-dired-do-compress (list file))))
    ))

;; ファイル名を指定してファイルを圧縮
(defun my-dired-do-compress (files)
  (interactive)
  (let ((prompt)
	(key)
	(open-dir "open")
	(file)
	(dir default-directory)
	(current-dir default-directory))

    ;; デフォルトの圧縮ファイル名
    (setq file (file-name-nondirectory (car files)))
    (if (/= 0 (string-match ".[^.]+$" file))
	(setq file (substring file 0 (match-beginning 0))))
    (setq file (format "%s%s%s" dir file my-compress-default-extension))

    ;; filesリストのディレクトリ部分は削っておく
    (setq files (mapcar (function file-name-nondirectory) files))

    (catch 'break
      (while t
	;; プロンプト
	(setq prompt "")
	(setq prompt (format "%s%s [%s]\n" prompt "s : specify file name" file))
	(setq prompt (format "%s%s [%s]\n" prompt "o : open the directory after compress" open-dir))
	(setq prompt (format "%s---\n"     prompt))
	(setq prompt (format "%s%s\n"      prompt "e : excecute with s and o options"))
	(setq prompt (format "%s%s\n"      prompt "n : cancel"))
	(message (format "%s?" prompt))

	;; キーコード読み取り
	(setq key (read-char))
	(cond
	 ((= key ?s)
	  (setq file
		(read-file-name
		 (format "%sspecify file name and directory [%s]: " prompt file)
		 file))
	  (setq dir (file-name-directory file)))
	 ((= key ?o)
	  (if (string= open-dir "open")
	      (setq open-dir "not open")
	    (setq open-dir "open")))
	 ((or (or (= key ?e) (= key ?\n)) (= key ?\r))
	  ;; ディレクトリがなかったら作っておく
	  (if (not (file-directory-p dir))
	      (make-directory dir t))

	  ;; ファイルが既に存在していたら上書きするか確認する
	  (if (file-exists-p file)
	      (if (y-or-n-p (format "%scompress file already exists. Overwrite?: " prompt))
		  (delete-file file)
		(throw 'break t)))
	  ;; (set-buffer (get-buffer-create "*My Compress*"))
	  ;; (setq default-directory current-dir)
	  ;; call-process を使いたいが、リストを一つ一つの引数として使うに
	  ;; はどうするのかわからない。defmacroを使うのか?
	  (dired-do-shell-command (format "atool -a \"%s\" *"
					  (expand-file-name file)) '1 files)
	  ;; ディレクトリ開く
	  (if (string= open-dir "open")
	      (dired dir))

	  ;; ディレクトリの表示を更新
	  (if (string= default-directory
		       (file-name-directory (expand-file-name file)))
	      (revert-buffer))
	  (throw 'break t))
	 ((or (= key ?n) (= key ?q))
	  (throw 'break t))
	 )))
    ))

;; ディレクトリを指定してファイルを展開
(defun my-dired-do-uncompress (file)
  (interactive)
  (let ((prompt)
	(key)
	(dir default-directory)
	(current-dir default-directory) (open-dir "open"))

    (setq file (file-name-nondirectory file))

    (catch 'break
      (while t
	;; プロンプト
	(setq prompt "")
	(setq prompt (format "%s%s [%s]\n" prompt "s : specify uncompress directory" dir))
	(setq prompt (format "%s%s\n"      prompt "c : create directory with file base name"))
	(setq prompt (format "%s%s\n"      prompt "t : create temporary directory and uncompress there"))
	(setq prompt (format "%s%s [%s]\n" prompt "o : open the directory after uncompress" open-dir))
	(setq prompt (format "%s---\n"     prompt))
	(setq prompt (format "%s%s\n"      prompt "e : excecute with s and o options"))
	(setq prompt (format "%s%s\n"      prompt "v : view file list"))
	(setq prompt (format "%s%s\n"      prompt "n : cancel"))
	(message (format "%s?" prompt))

	;; キーコード読み取り
	(setq key (read-char))
	(cond
	 ((= key ?s)
	  (setq dir
		(read-file-name
		 (format "%sspecify uncompress directory [%s]: " prompt dir)
		 dir)))
	 ((= key ?c)
	  (setq dir (concat dir (substring file 0 (string-match "\\." file)))))
	 ((= key ?t)
	  (setq dir (make-temp-name "~/tmp/emacs-uncompress/")))
	 ((= key ?o)
	  (if (string= open-dir "open")
	      (setq open-dir "not open")
	    (setq open-dir "open")))
	 ((or (or (= key ?e) (= key ?\n)) (= key ?\r))
	  (if (not (file-directory-p dir))
	      (make-directory dir t))
	  (set-buffer (get-buffer-create "*My Compress*"))
	  (setq default-directory current-dir)
	  (call-process "atool" nil t t "-X" (expand-file-name dir) file)

	  ;; ディレクトリ開く
	  (if (string= open-dir "open")
	      (dired dir))

	  ;; ディレクトリの表示を更新
	  (if (string= default-directory
		       (expand-file-name dir))
	      (revert-buffer))
	  (throw 'break t))
	 ((= key ?v)
	  (switch-to-buffer-other-window (get-buffer-create "*My Compress*"))
	  (setq default-directory current-dir)
	  (goto-char (point-max))
	  (save-excursion
	    (call-process "atool" nil t t "-l" file))
	  (throw 'break t))
	 ((or (= key ?n) (= key ?q))
	  (throw 'break t))
	 )))
    ))

;; 拡張子が圧縮ファイルのとき、ファイルを解凍
(defun my-dired-do-check-and-uncompress ()
  (interactive)
  (let ((file (dired-get-filename))
	(extension-list my-compress-extension-list)
	(extention))
    (catch 'break
      (while extension-list
	(setq extension (car extension-list)
	      extension-list (cdr extension-list))
	(when (string-match extension file)
	  (my-dired-do-uncompress file)
	  (throw 'break t))))
    (if (string-match extension file)
	t
      nil)
    ))
