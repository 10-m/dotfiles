;; -*- coding: utf-8-unix -*-

(add-to-list 'load-path "~/.emacs.d/mannual-libs")

(defconst my-elisp-directory "~/share/elisp" "The directory for my elisp file.")

(dolist (dir (let ((dir1 (expand-file-name "~/.emacs.d/mannual-libs"))
                   (dir2 (expand-file-name "~/local/share")))
               (list dir1
                     dir2
                     (format "%s%d" dir1 emacs-major-version)
                     (format "%s%d" dir2 emacs-major-version))))
  (when (and (stringp dir) (file-directory-p dir))
    (let ((default-directory dir))
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

;; ---------------------------------------------------------
;; El-get
;; ---------------------------------------------------------
;; M-x el-get-update      パッケージを指定してアップデート
;; M-x el-get-upadate-all 全てアップデート

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;;(el-get 'sync)

;; (add-to-list 'load-path "~/tmp/emacs/mb-url-master")
;; (require 'mb-url-http)
;; (advice-add 'url-http :override 'mb-url-http-curl)

(el-get-bundle popup)
(el-get-bundle auto-complete)
(el-get-bundle elscreen)
(el-get-bundle yasnippet)
(el-get-bundle magit)
(el-get-bundle undo-tree)
(el-get-bundle color-moccur)
(el-get-bundle moccur-edit)
(el-get-bundle expand-region)
(el-get-bundle s)
(el-get-bundle f)
(el-get-bundle recentf-ext)
(el-get-bundle migemo)
