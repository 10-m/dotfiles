;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; helm
;; ---------------------------------------------------------
(require 'helm-config)
(require 'helm-c-moccur)

(global-set-key "\C-c;" 'helm-for-files)

;; describe-bindingsをhelmで置き換え
(helm-descbinds-mode)

;; M-yにhelm-show-kill-ringを割り当てる
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)

;; moccur
(helm-migemo-mode 1)

(setq helm-idle-delay 0.1
      ;; helm-c-moccur用 `helm-idle-delay'
      helm-c-moccur-helm-idle-delay 0.1
      ;; バッファの情報をハイライトする
      helm-c-moccur-higligt-info-line-flag t
      ;; 現在選択中の候補の位置をほかのwindowに表示する
      helm-c-moccur-enable-auto-look-flag t
      ;; 起動時にポイントの位置の単語を初期パターンにする
      helm-c-moccur-enable-initial-pattern t)
(global-set-key (kbd "C-M-o") 'helm-c-moccur-occur-by-moccur)

(set-face-background 'helm-selection "yellow")
(set-face-foreground 'helm-selection "black")

;; ---------------------------------------------------------
;; 定型句 abbrev
;; ---------------------------------------------------------
;; C-x a +          モード固有の略語を設定
;; C-x a g          グローバルな略語を設定
;; C-x a '          略語を展開 (M-SPC)
;; M-x edit-abbrevs 略語の編集
;; 指定した範囲の略語を登録するためには，リージョンでその範囲を選択し，
;; C-u 0 C-x a + 変更が終わったら， C-c C-c として保存すると，その変更が
;; 適用
(setq abbrev-file-name "~/local/.abbrev_defs")
(setq save-abbrevs t)
(quietly-read-abbrev-file)

;; M-Space で展開
(define-key esc-map " " 'expand-abbrev)

;; 自動展開をしない
(add-hook 'pre-command-hook
      (lambda ()
        (setq abbrev-mode nil)))

;; ---------------------------------------------------------
;; yasnippest
;; ---------------------------------------------------------
(defface yas/field-highlight-face
  '((t (:background "white" :foreground "black"))) nil)
(require 'yasnippet)

;; コメントやリテラルではスニペットを展開しない
;; http://d.hatena.ne.jp/rubikitch/20080729/1217284678
(setq yas/buffer-local-condition
      '(or (not (memq (get-text-property (point) 'face)
              '(font-lock-comment-face
            font-lock-doc-face
            font-lock-string-face)))
       '(require-snippet-condition . force-in-comment)))

;; ディレクトリ分ける
;; http://d.hatena.ne.jp/antipop/20080321/1206090430
(if (not (file-directory-p (expand-file-name "~/local/snippets")))
    (make-directory (expand-file-name "~/local/snippets")))
(setq yas/root-directory
      (list (expand-file-name "~/.emacs.d/el-get/yasnippet/snippets")
        (expand-file-name "~/local/snippets")
        (expand-file-name "~/.emacs.d/snippets")))

;; face
(set-face-foreground 'yas/field-highlight-face "white")
(set-face-background 'yas/field-highlight-face "blue")

(custom-set-variables '(yas/prompt-functions '(yas/dropdown-prompt)))

;; 初期化
(yas/initialize)
(yas/reload-all)
(yas-global-mode 1)

;; [メモ]
;; テンプレート作成している最中に、C-c C-t で動作テストできる

(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(yas-global-mode 1)

;; ---------------------------------------------------------
;; 動的略語展開 dabbrev
;; ---------------------------------------------------------
;; M-/      候補で補完
;; C-M-/    候補を別ウィンドウに一覧表示

;; 日本語で dabbrev を使う
;; http://namazu.org/%7Etsuchiya/elisp/dabbrev-ja.el
;; (load "dabbrev-ja")

;; dabbvrev の拡張
(defvar my-dabbrev-expand-max 3)
(defvar my-dabbrev-expand-count 0)
(defvar my-dabbrev-expand-orig-pos nil)
(defun my-dabbrev-expand ()
  (interactive)
  (if (eq last-command this-command)
      (setq my-dabbrev-expand-count (+ 1 my-dabbrev-expand-count))
    (setq my-dabbrev-expand-orig-pos (point))
    (setq my-dabbrev-expand-count 0))
  (if (< my-dabbrev-expand-count my-dabbrev-expand-max)
      (dabbrev-expand nil)
    (delete-region my-dabbrev-expand-orig-pos (point))
    (setq my-dabbrev-expand-count 0)
    (dabbrev-completion 4)))
(global-set-key "\M-/" 'my-dabbrev-expand)

;; ---------------------------------------------------------
;; auto-complete
;; ---------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'text-mode)
(setq ac-comphist-file "~/tmp/emacs/ac-comphist.dat")

;; C-n / C-p で選択
;; C-sで検索可能
(setq ac-use-menu-map t)

(setq-default ac-sources
      '(ac-source-yasnippet
        ac-source-abbrev
        ac-source-filename
        ac-source-dictionary
        ac-source-words-in-same-mode-buffers))
(ac-set-trigger-key "TAB")

;; ---------------------------------------------------------
;; minibuffer completion
;; ---------------------------------------------------------
(require 'ido-migemo)
(ido-mode 1)
(ido-migemo-mode 1)
(icomplete-mode t)
(setq ido-save-directory-list-file "~/tmp/emacs/ido.last")

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; C-x C-f ido-find-file
;; C-f 通常のfind-file (ファイル作成などで使用)
;; C-d diredで開く

;; ---------------------------------------------------------
;; other
;; ---------------------------------------------------------
;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
