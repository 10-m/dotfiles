;; -*- coding: utf-8-unix -*-

;; ---------------------------------------------------------
;; Perl::Tidy
;; ---------------------------------------------------------
 (defun perltidy-region ()
   "Run perltidy on the current region."
   (interactive)
   (indent-region (point) (mark))
   (save-excursion
     (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
 (defun perltidy-defun ()
   "Run perltidy on the current defun."
   (interactive)
   (indent-region (point) (mark))
   (save-excursion (mark-defun)
                   (perltidy-region)))

;; デバッグ文挿入
(defun my-insert-perl-debug ()
  (interactive)

  (insert-string "use Time::HiRes qw/gettimeofday/;\n")
  (insert-string "use POSIX 'strftime';\n\n")
  (insert-string "sub T {\n");
  (insert-string "    my ($package, $file, $line) = caller(0);\n")
  (insert-string "    my ( $sec, $usec ) = gettimeofday;\n")
  (insert-string "    my $str = strftime( '%Y-%m-%d %H:%M:%S', localtime $sec );\n")
  (insert-string "    printf \"[%s.%03d] $package:$file:$line \", $str, $usec / 1000;\n")
  (insert-string "    print @_, \"\\n\";\n")
  (insert-string "}\n")
  (indent-according-to-mode))

;; ---------------------------------------------------------
;; perldoc などの情報を表示
;; ---------------------------------------------------------
;; カーソル付近、またはミニバッファで指定したのモジュールの情報を表示
;; [元ネタ]
;; http://saltyduck.blog12.fc2.com/blog-entry-24.html
;; http://d.hatena.ne.jp/hakutoitoi/20090208/1234069614
;; http://d.hatena.ne.jp/antipop/20080702/1214926316
;; http://perl-users.jp/articles/advent-calendar/2009/casual/12.html
;; [使い方]
;; M-x my-perldoc
;;  my-perldoc-* を選択する anything 起動
;; M-x my-perldoc-module
;;  perldoc -m を実行
;; M-x my-perldoc-pod
;;  perldoc。PODをバッファに表示
;; M-x my-perldoc-info
;;  モジュールの名前、パス、バージョンを表示
;; M-x my-perldoc-function
;;  perldoc -f
;; M-x my-perldoc-section
;;  perldoc [perl section]
;; M-x my-perldoc-file
;;  他のウィンドウにファイルをオープン
;; M-x my-perldoc-switch-file
;;  現在のウィンドウでファイルをオープン

;; my-perldoc-section で補完するセクションのリスト
(setq my-perldoc-section-completion-list
      (list "perldoc"
            "perl"
            "perlvar"
            "perlre"
            "perlretut"
            "perlopentut"
            "perlipc"
            "perlxs"
            "perlapi"
            "perlapio"
            "perlguts"
            "perlclib"))

(put 'perl-module-thing 'end-op
     (lambda ()
       (re-search-forward "\\=[a-zA-Z][a-zA-Z0-9_:]*" nil t)))
(put 'perl-module-thing 'beginning-op
     (lambda ()
       (if (re-search-backward "[^a-zA-Z0-9_:]" nil t)
           (forward-char)
         (goto-char (point-min)))))

(defun my-perldoc-get-module-name (prompt &optional collection)
  (interactive)
  (let ((module
         (substring-no-properties (thing-at-point 'perl-module-thing))))
    (setq prompt (or prompt "Module: "))
    (completing-read prompt collection nil nil module)))

(defun my-perldoc-create-and-switch-buffer (mode buffer-name text)
  (interactive)
  (let ((buffer (get-buffer-create buffer-name)))
    (unless (string-match "*Perl " (buffer-name))
      (progn
        (when (one-window-p)
          (split-window))
        (other-window 1) ))
    (switch-to-buffer buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert text)
    (goto-char (point-min))
    (funcall mode)
    (toggle-read-only 1)))

(defun my-perldoc-common (cmd mode prompt &optional collection)
  (interactive)
  (let ((module (my-perldoc-get-module-name prompt collection)))
    (let ((result
           (substring
            (shell-command-to-string (concat cmd " " module)) 0 -1)))
      (if (string-match "No module found for" result)
          (message "%s" result)
        (my-perldoc-create-and-switch-buffer
         mode (format "*Perl %s(%s)*" module cmd) result)))))

;; cperl-perldoc、cperl-perldoc-at-point でもいいか？
(defun my-perldoc-module ()
  "perldoc -m"
  (interactive)
  (my-perldoc-common "perldoc -t -T -m" 'cperl-mode "Module: "))

(defun my-perldoc-pod ()
  "perldoc -T"
  (interactive)
  (my-perldoc-common "perldoc -t -T" 'fundamental-mode "Module: "))

(defun my-perldoc-function ()
  "perldoc -f"
  (interactive)
  (my-perldoc-common "perldoc -t -T -f" 'fundamental-mode "Function: "))

(defun my-perldoc-section ()
  "perldoc"
  (interactive)
  (my-perldoc-common "perldoc -t -T"
                     'fundamental-mode
                     "Section: "
                     my-perldoc-section-completion-list))

(defun my-perldoc-info ()
  "show perl info using Module::Build::ModuleInfo"
  (interactive)
  (my-perldoc-common
   "show_perl_module_info.pl" 'fundamental-mode "Module: "))

(defun my-perldoc-file (&optional switch)
  "open file"
  (interactive)
  (let ((module (my-perldoc-get-module-name "File: "))
        (path))
    (setq path
          (substring
           (shell-command-to-string (concat "perldoc -l " module)) 0 -1))
    (if (string-match "No module found for" path)
        (message "%s" path)
      ;; Meadow のときは cygwin を使っていると仮定
      (if (featurep 'meadow)
          (setq path
                (substring
                 (shell-command-to-string
                  (concat "cygpath.exe -wl " path)) 0 -1)))
      (when (not switch)
        (when (one-window-p)
          (split-window))
        (other-window 1))
      (find-file path)
      (toggle-read-only 1))))

(defun my-perldoc-switch-file ()
  "open file at current buffer"
  (interactive)
  (my-perldoc-file t))

(setq my-perldoc-c-source
      '(
        (name . "Perldoc")
        (candidates . (lambda() (list
                                 "my-perldoc-module"
                                 "my-perldoc-pod"
                                 "my-perldoc-function"
                                 "my-perldoc-section"
                                 "my-perldoc-info"
                                 "my-perldoc-file"
                                 "my-perldoc-switch-file")))
        (type . command)
    ))

(defun my-perldoc ()
  "short-cut my-perldoc-*"
  (interactive)
  (anything (list my-perldoc-c-source) "my-perldoc-"))

;; ---------------------------------------------------------
;; perl-completion
;; ---------------------------------------------------------
(setq plcmp-use-keymap nil)
(require 'perl-completion)

;; ドキュメント
;; plcmp-cmd-show-doc
;; plcmp-cmd-show-doc-at-point
(define-key cperl-mode-map "\C-ccd" 'plcmp-cmd-show-doc)

;; モジュール補完
;; plcmp-cmd-complete-modules
(define-key cperl-mode-map "\C-ccM" 'plcmp-cmd-complete-modules)

;; メセッド補完
;; plcmp-cmd-complete-methods

;; メセッド補完 (smart-completion)
;; plcmp-cmd-smart-complete
;; $obj-> を補完
(define-key cperl-mode-map "\C-ccm" 'plcmp-cmd-smart-complete)

;; 関数補完
;; plcmp-cmd-complete-functions
(define-key cperl-mode-map "\C-ccf" 'plcmp-cmd-complete-functions)

;; 変数補完
;; plcmp-cmd-complete-variables
;; plcmp-cmd-complete-arrays
;; plcmp-cmd-complete-hashes
(define-key cperl-mode-map "\C-ccv" 'plcmp-cmd-complete-variables)
(define-key cperl-mode-map "\C-cca" 'plcmp-cmd-complete-arrays)
(define-key cperl-mode-map "\C-cch" 'plcmp-cmd-complete-hashes)

;; ---------------------------------------------------------
;; Hook
;; ---------------------------------------------------------
(autoload 'cperl-mode "cperl-mode" nil t)
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist
      (append '(("\\.pl$"  . cperl-mode))
              '(("\\.pm$"  . cperl-mode))
              '(("\\.cgi$" . cperl-mode))
              '(("\\.t$"   . cperl-mode))
              auto-mode-alist))

(add-hook 'cperl-mode-hook
          '(lambda()
             ;; Perl Completion
             (perl-completion-mode t)
             (setq fill-column 78)
             (setq auto-fill-mode t)
             (setq cperl-indent-level 4)
             (setq cperl-continued-statement-offset 0)
             (setq cperl-indent-parens-as-block t)
             (setq cperl-tab-always-indent t)
             (setq indent-tabs-mode nil)
             ;; Face for Array
             (set-face-foreground 'cperl-hash-face "pink")
             (set-face-italic-p 'cperl-hash-face nil)
             (set-face-background 'cperl-hash-face nil)
             (set-face-bold-p 'cperl-hash-face nil)
             ;; Face for Hash
             (set-face-foreground 'cperl-array-face "green")
             (set-face-italic-p 'cperl-array-face nil)
             (set-face-background 'cperl-array-face nil)
             (set-face-bold-p 'cperl-array-face nil)
             ;; C-c i でインデントリージョン
             (define-key (current-local-map) "\C-ci" 'perltidy-region)
             ;; デバッグコメント挿入
             (define-key (current-local-map) "\C-cp" 'my-insert-perl-debug)
             ;; brackets
             (define-key (current-local-map) "{" 'insert-pair)
             (define-key (current-local-map) "(" 'insert-pair)
             (define-key (current-local-map) "[" 'insert-pair)
             (define-key (current-local-map) ")" 'my-insert-brace)
             (define-key (current-local-map) "]" 'my-insert-bracket)
             (define-key (current-local-map) "}" 'my-insert-paran)
             ))
