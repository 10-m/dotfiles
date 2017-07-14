;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; markdown-mode
;; ---------------------------------------------------------
;; C-c C-c p : Preview
;; Tab, S-Tab : Expand, Hold

(require 'markdown-mode)
(add-to-list 'auto-mode-alist'("\\.md\\'" . markdown-mode))
(setq markdown-command "pandoc")

;; ---------------------------------------------------------
;; flycheck
;; ---------------------------------------------------------
;; See devel.el
