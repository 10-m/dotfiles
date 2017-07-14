;; -*- coding: utf-8-unix -*-

(defun my-replace-anki-word (&optional flag)
   "thisandthat."
  (interactive (list  current-prefix-arg))
   (let ((beg (region-beginning)) (end (region-end)))
     (if flag
         (replace-regexp "[a-zA-Z]" "-" nil beg end)
       (replace-regexp "[a-zA-Z]" "_" nil beg end))))

;; (global-set-key [f6] 'my-replace-anki-word)

(defun my-dired-mark (arg)
  (interactive "P")
  (if (dired-get-subdir)
      (save-excursion (dired-mark-subdir-files))
    (let ((inhibit-read-only t))
      (dired-repeat-over-lines
       (prefix-numeric-value arg)
       (function (lambda () (delete-char 1) (insert ?M)))))))

(global-set-key [f7] 'my-dired-mark)
