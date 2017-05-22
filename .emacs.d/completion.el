;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; anything-source
;; ---------------------------------------------------------
;; オブジェクト選択、RETでデフォルト動作、TABでアクション指定
(require 'anything)
(require 'anything-config)
(require 'anything-migemo)
(require 'anything-match-plugin)

(setq anything-c-adaptive-history-file "~/tmp/emacs/anything-c-adaptive-history")

;; ライン移動
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)

;; ソース移動
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)

;; バッファ検索
(setq anything-sources (list anything-c-source-buffers+
                             anything-c-source-recentf
                             anything-c-source-file-name-history
                             anything-c-source-bookmarks
                             anything-c-source-file-cache
                             anything-c-source-locate))
;; C-c ;        通常検索
;; C-u C-c ;    Migemo検索
(global-set-key "\C-cab" 'anything-migemo)

;; プログラム検索
(defun my-anything-program ()
   "show source"
   (interactive)
   (let ((anything-sources (list anything-c-source-imenu
                                 anything-c-source-occur
                                 anything-c-source-semantic)))
     (anything-at-point)))
(global-set-key "\C-cap" 'my-anything-program)

;; 辞書検索
(defun my-anything-dict ()
   "show source"
   (interactive)
   (let ((anything-sources (list anything-c-source-man-pages
                                 anything-c-source-info-pages)))
     (anything-at-point)))
(global-set-key "\C-cad" 'my-anything-dict)

;; ミニバッファ
;; C-rでanything検索

;; emacs検索
(defun my-anything-emacs ()
   "show source"
   (interactive)
   (let ((anything-sources (list anything-c-source-emacs-commands
                                 anything-c-source-emacs-functions)))
     (anything)))
(global-set-key "\C-cae" 'my-anything-emacs)

;; ---------------------------------------------------------
;; anything-complete
;; ---------------------------------------------------------
(require 'anything-complete)

;; C-c h シェルモードでヒストリ表示
(define-key shell-mode-map "\C-cah" 'anything-complete-shell-history)

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
(add-to-list 'ac-modes 'text-mode)
(setq ac-comphist-file "~/tmp/emacs/ac-comphist.dat")

;; C-n / C-p で選択
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
;; (require 'ido-migemo-mode)
(ido-mode 1)
;;(ido-migemo-mode 1)
(icomplete-mode t)
(setq ido-save-directory-list-file "~/tmp/emacs/ido.last")

;; ---------------------------------------------------------
;; iswitchb バッファの切換えを楽にする
;; ---------------------------------------------------------
;; [使い方]
;;! C-x b とすると，バッファの一覧がミニバッファに表示
;;! C-s ， C-r でバッファの選択を切り替え
;; (iswitchb-mode 1)

;; ;; iswitchb で migemo
;; (setq iswitchb-regexp t)
;; (setq iswitchb-use-migemo-p t)
;; (defadvice iswitchb-get-matched-buffers
;;   (before iswitchb-use-migemo activate)
;;   "iswitchb で migemo"
;;   (when iswitchb-use-migemo-p
;;     (ad-set-arg
;;      0 (migemo-get-pattern
;;         (ad-get-arg 0)))))

;; ---------------------------------------------------------
;; other
;; ---------------------------------------------------------
;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
