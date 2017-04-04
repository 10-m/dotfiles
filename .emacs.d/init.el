;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; コマンドラインメモ
;; ---------------------------------------------------------
;; 起動オプション -q 個人の初期化ファイル`~/.emacs'も`default.el'もロードしない。

;; 各パッケージに含まれるauto-autoloads.el をロードしない

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-default-init t)

;; パッケージ管理
(load "~/.emacs.d/my_package.el")

;; ローカル設定
(load "~/local/local.el")
(load "~/close/close.el")

;; 言語設定
(load "~/.emacs.d/lang.el")

;; dired
(load "~/.emacs.d/dired.el")

;; dired-compress
(load "~/.emacs.d/dired_compress.el")

;; frame
(load "~/.emacs.d/frame.el")

;; edit
(load "~/.emacs.d/edit.el")

;; devel
(load "~/.emacs.d/devel.el")

;; display
(load "~/.emacs.d/display")

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)
