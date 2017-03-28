;; -*- coding: euc-jp-unix -*-

;; http://d.hatena.ne.jp/Tetsujin/20090623/1245690877
;; �����ߥʥ���Emacs�˥ڡ����Ȥ�����

(defvar term-paste-mode-map
  (let ((map (make-keymap))
        (i ? ))
    (while (< i ?~) ;; ��ꤢ�������ڡ��� �� ~�ޤ�self-insert-command�򥻥åȤ��ƾ��
      (define-key map (char-to-string i) 'self-insert-command)
      (setq i (1+ i)))
    (define-key map "\C-m" 'newline)
    map))

(defcustom term-paste-mode-on-hook nil
  "Hook to run when term-paste-mode is activated."
  :group 'term-paste
  :type 'hook)

(defcustom term-paste-mode-off-hook nil
  "Hook to run when term-paste-mode is deactivated."
  :group 'term-paste
  :type 'hook)

(define-minor-mode term-paste-mode
  "Minor mode for pasting from any terminal applications."
  :lighter " Paste"
  :group 'term-paste
  (cond (term-paste-mode
         ;; minor-mode-map��ͥ���̤�夲��
         (setq minor-mode-map-alist
               (cons (cons 'term-paste-mode term-paste-mode-map)
                     (delete (assq 'term-paste-mode minor-mode-map-alist)
                             minor-mode-map-alist)))
         (run-hooks 'term-paste-mode-on-hook))
        (t
         (run-hooks 'term-paste-mode-off-hook)
         )))

(provide 'term-paste-mode)
