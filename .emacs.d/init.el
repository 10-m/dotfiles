;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; コマンドラインメモ
;; ---------------------------------------------------------
;; 起動オプション -q 個人の初期化ファイル`~/.emacs'も`default.el'もロードしない。

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;; 各パッケージに含まれるauto-autoloads.el をロードしない
(setq inhibit-default-init t)

;; ローカル設定
(load "~/local/local.el")
(load "~/close/close.el")

;; パッケージ管理
(load "~/.emacs.d/my_package.el")

;; 言語設定
(load "~/.emacs.d/lang.el")

;; dired
(load "~/.emacs.d/dired.el")

;; dired-compress
(load "~/.emacs.d/dired_compress.el")

;; frame
(load "~/.emacs.d/frame.el")

;; move
(load "~/.emacs.d/move.el")

;; edit
(load "~/.emacs.d/edit.el")

;; view
(load "~/.emacs.d/view.el")

;; history
(load "~/.emacs.d/history.el")

;; clipboard
(load "~/.emacs.d/clipboard.el")

;; mail
(load "~/.emacs.d/mail.el")

;; shell
(load "~/.emacs.d/shell.el")

;; completion
(load "~/.emacs.d/completion.el")

;; find
(load "~/.emacs.d/find.el")

;; template
(load "~/.emacs.d/auto-insert.el")

;; tool
(load "~/.emacs.d/tool.el")

;; devel
(load "~/.emacs.d/devel.el")

;; perl
(load "~/.emacs.d/devel-perl.el")

;; shell
(load "~/.emacs.d/devel-shell.el")

;; C/C++
(load "~/.emacs.d/devel-c.el")

;; html, javascript
 (load "~/.emacs.d/devel-web.el")

;; emacs list
(load "~/.emacs.d/devel-elisp.el")

;; R
(load "~/.emacs.d/devel-R.el")

;; python
(load "~/.emacs.d/devel-python.el")

;; memo
(load "~/.emacs.d/devel-memo.el")

;; dictionay
(load "~/.emacs.d/dict.el")

;; misc
(load "~/.emacs.d/misc.el")

;; always end a file with a newline
;(setq require-final-newline 'query)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay nil)
 '(package-selected-packages
   (quote
    (helm-c-yasnippet wgre yaml-mode auto-complete undohist ido-migemo helm-c-moccur helm-descbinds yasnippet wgrep undo-tree sr-speedbar smex shell-history recentf-ext py-yapf migemo mew markdown-mode magit js2-mode jedi helm google-this flycheck f expand-region ess elscreen color-moccur browse-kill-ring)))
 '(yas-prompt-functions (quote (yas/dropdown-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#111111" :foreground "#EEEEEE"))))
 '(cursor ((((class color) (background dark)) (:background "#2e8b57")) (((class color) (background light)) (:background "#999999")) (t nil)))
 '(woman-bold ((t (:foreground "yellow")))))

(put 'upcase-region 'disabled nil)
