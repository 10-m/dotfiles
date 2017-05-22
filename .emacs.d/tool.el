;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; tool option
;; ---------------------------------------------------------
;; default to unified diffs
(setq diff-switches "-u")

;; ---------------------------------------------------------
;; emacsclient/gnuclient
;; ---------------------------------------------------------
(server-start)

;; ---------------------------------------------------------
;; sr-speedbar
;; ---------------------------------------------------------
;; Space           toggle-expand (+/-)
;; e or C-m        edit
(require 'sr-speedbar)

;; Don't move to speedbar-window by C-o
(setq sr-speedbar-skip-other-window-p t)

;; Open speedbar and focus, or return original window if in speedbar
(global-set-key [f6] '(lambda ()
                        (interactive)
                        (if (equal major-mode 'speedbar-mode)
                            (other-window 1)
                          (sr-speedbar-open)
                          (sr-speedbar-select-window))))

;; Up dir
(define-key speedbar-mode-map [backspace] 'speedbar-up-directory)

;; Quit
(define-key speedbar-mode-map "q" 'sr-speedbar-close)
