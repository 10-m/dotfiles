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

(defun my-speedbar-expand-all-lines ()
  "Expand all items in the speedbar buffer.
 But be careful: this opens all the files to parse them."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line)
    (speedbar-expand-line)))
(define-key speedbar-mode-map "+" 'my-speedbar-expand-all-lines)

(defun my-speedbar-contract-all-lines ()
  "Contract all items in the speedbar buffer."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line)
    (speedbar-contract-line)))
(define-key speedbar-mode-map "-" 'my-speedbar-contract-all-lines)

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

;; ---------------------------------------------------------
;; browser
;; ---------------------------------------------------------
(if window-system
    (setq browse-url-browser-function 'browse-url-firefox
          browse-url-firefox-new-window-is-tab t)
  (setq browse-url-browser-function 'eww-browse-url))

(require 'google-this)
(global-set-key "\C-cb" 'google-this)

(setq google-this-location-suffix "co.jp")
(defun google-this-url () "URL for google searches."
  (concat google-this-base-url google-this-location-suffix
          "/search?q=%s&hl=ja&lr=lang_ja"))
