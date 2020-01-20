;; -*- coding: utf-8-unix -*-

(let ((default-directory (expand-file-name "~/.emacs.d/mannual-libs")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(let ((default-directory (expand-file-name "~/local/share/gtags")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; ---------------------------------------------------------
;; package.el
;; ---------------------------------------------------------
;; M-x package-install      パッケージを指定してアップデート
;; M-x package-refresh-contents パッケージ情報を更新

(require 'package)

;; パッケージ情報の更新
(package-refresh-contents)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; 初期化
(package-initialize)

(setq my/favorite-packages
  '(popup
    elscreen
    yasnippet
    magit
    undo-tree
    color-moccur
    auto-complete
    ;; moccur-edit
    wgre
    expand-region
    s
    f
    migemo
    mew
    browse-kill-ring
    helm
    helm-descbinds
    helm-c-moccur
    ido-migemo
    shell-history
    sr-speedbar
    google-this
    js2-mode
    flycheck
    py-yapf
    jedi
    markdown-mode
    smex
    recentf-ext
    undohist
    ess
    yaml-mode
    helm-c-yasnippet
    ;; gtags
    ))

;; my/favorite-packagesからインストールしていないパッケージをインストール
;; (package-refresh-contents)
;; (dolist (package my/favorite-packages)
;;   (unless (package-installed-p package)
;;     (package-install package)))
