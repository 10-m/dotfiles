;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; auto-insert 設定
;; ---------------------------------------------------------
;; テンプレートの保存先
(setq auto-insert-directory "~/template/")
(auto-insert-mode 1)

;; テンプレート挿入時に尋ねない
;; デフォルトは 'function
(setq auto-insert-query nil)

;; ---------------------------------------------------------
;; 自動展開
;; ---------------------------------------------------------
;; http://www.02.246.ne.jp/~torutk/cxx/emacs/mode_extension.html

;; マクロの定義
(require 'cl)
(defvar template-replacements-alists
  '(("%file%" . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda () 
                              (setq file-without-ext (file-name-sans-extension
                                                      (file-name-nondirectory (buffer-file-name))))))
;;     ("%namespace%" .
;;      (lambda () (setq namespace (read-from-minibuffer "namespace: "))))
    ("%namespace%" .
     (lambda () (setq namespace "")))
    ("%include%" .
     (lambda () 
       (cond ((string= namespace "") (concat "\"" file-without-ext ".h\""))
             (t (concat "<" (replace-regexp-in-string "::" "/" namespace) "/"
                        file-without-ext ".h>")))))
    ("%include-guard%" . 
     (lambda ()
       (format "%s_H_"
               (upcase (concat 
                        (replace-regexp-in-string "::" "_" namespace)
                        (unless (string= namespace "") "_")
                        file-without-ext)))))
    ("%name%" . user-full-name)
    ("%mail%" . (lambda () (identity user-mail-address)))
    ("%cyear%" . (lambda () (substring (current-time-string) -4)))
;;     ("%bdesc%" . (lambda () (read-from-minibuffer "Brief description: ")))
    ("%namespace-open%" .
     (lambda ()
       (cond ((string= namespace "") "")
             (t (progn 
                  (setq namespace-list (split-string namespace "::"))
                  (setq namespace-text "")
                  (while namespace-list
                    (setq namespace-text (concat namespace-text "namespace "
                                                 (car namespace-list) " {\n"))
                    (setq namespace-list (cdr namespace-list))
                    )
                  (eval namespace-text))))))
    ("%namespace-close%" .
     (lambda ()
       (cond ((string= namespace "") "")
             (t (progn
                  (setq namespace-list (reverse (split-string namespace "::")))
                  (setq namespace-text "")
                  (while namespace-list
                    (setq namespace-text (concat namespace-text "} // " (car namespace-list) "\n"))
                    (setq namespace-list (cdr namespace-list))
                    )
                  (eval namespace-text))))))
    ))

;; 展開関数
(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))

;; ---------------------------------------------------------
;; テンプレート関数
;; ---------------------------------------------------------
;; .h ファイル
(defun my-c-header-template ()
  (let ((str
         (concat "__"
                 (replace-regexp-in-string
                  "\\."
                  "_"
                  (upcase (file-name-nondirectory (buffer-file-name))))
                 "__")))
  (insert "#ifndef " str "\n#define " str "\n\n\n\n#endif /* " str " */\n"))
  (previous-line 3))

;; ---------------------------------------------------------
;; テンプレート定義
;; ---------------------------------------------------------
(setq auto-insert-alist
      (append
       '(
         ;; ファイル名で指定
         ("\\.js$"                         . "template.js")
         ("\\.pl$"                         . ["template.pl" my-template])
         ("\\.cgi$"                        . "template.cgi")
         ("\\.pic$"                        . "template.pic")
         ("\\.pm$"                         . "template.pm")
         ("\\.pod$"                        . "template.pod")
         ("\\.el$"                         . "template.el")
         ("\\.sh$"                         . "template.sh")
         ("\\.c$"                          . "template.c")
         ("\\.cpp$"                        . "template.cpp")
         ("\\.dot$"                        . "template.dot")
         ("\\(\\.htm\\)\\|\\(\\.html\\)$"  . "template.html")
         ("\\(Makefile\\)\\|\\(\\.mk\\)$"  . "template.mk")
         ("\\.\\([Hh]\\|hh\\|hpp\\)\\'"    . my-c-header-template)
         ("\\.t$"                          . "template.t")
         ("\\.mp$"                         . "template.mp")
         ("\\.R$"                          . "template.R")
         ("\\.md$"                         . "template.md")
         ("\\.py$"                         . "template.py")
         )
       auto-insert-alist))

(defun my-date ()
  (insert (format-time-string "%D" (current-time))))
