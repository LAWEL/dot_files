;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'exec-path "~/.cabal/bin/")
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac用フォント設定
;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/

;; 英語
(set-face-attribute 'default nil
                    :family "Menlo" ;; font
                    :height 128)    ;; font size
;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font
;; 半角と全角の比を1:2にしたければ
(setq face-font-rescale-alist
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; Mac用フォント設定
;; オープニングメッセージを表示しない
(setq inhibit-startup-message t)
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
;; ディスプレイの設定
(display-time-mode 1)
(line-number-mode 1)
(column-number-mode 1)
;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)
;; command-keyをmeta-keyとする
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))
;; yes-or-noをy-or-nとする
(defalias 'yes-or-no-p 'y-or-n-p)
;; デフォルトをテキストモードにする
(setq default-major-mode 'text-mode)
;; トラックパッドのスクロール設定
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 3)) ; 1回でスクロールする行数
(defun scroll-up-with-lines ()
  "" (interactive) (scroll-up 3))
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [double-wheel-up] 'scroll-down-with-lines)
(global-set-key [double-wheel-down] 'scroll-up-with-lines)
(global-set-key [triple-wheel-up] 'scroll-down-with-lines)
(global-set-key [triple-wheel-down] 'scroll-up-with-lines)
;; ~/ を初期ディレクトリにする
(cd "~/")
;; scratchの初期メッセージを消去する
(setq initial-scratch-message "")
;; ビープ音を消す
(setq ring-bell-function 'ignore)
;; スクロールバーを消す
(scroll-bar-mode -1)
;; ツールバーを消す
(tool-bar-mode -1)
;; メニューバーを消す
(menu-bar-mode -1)
;; 対応す括弧をハイライト
(show-paren-mode t)
;; バックアップを残さない
(setq make-backup-files nil)
;; 文字色
(add-to-list 'default-frame-alist '(foreground-color . "white"))
;; 背景色
(add-to-list 'default-frame-alist '(background-color . "black"))
;; カーソル色
(add-to-list 'default-frame-alist '(cursor-color . "grey"))
;; マウスポインタ色
(add-to-list 'default-frame-alist '(mouse-color . "grey"))
;; 選択中のリージョンの色
(set-face-background 'region "MediumBlue")
;; 行番号表示
(require 'linum)
(global-linum-mode)
;; 現在行のハイライト
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)
;; 同名ファイルのバッファ名の識別文字列を変更する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; バッファの自動再読み込み
(global-auto-revert-mode 1)
;; 保存時にカレントバッファの行末空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs で全角スペース/タブ文字を可視化
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")

;; Emacsでインデントを、タブを使わないでスペースを使う設定
(setq-default tab-width 4 indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell の存在を確認
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))

;; Shell 名の設定
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabbar.el : バッファをタブで管理する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'tabbar)
(tabbar-mode t)
(tabbar-mwheel-mode -1) ; タブ上でマウスホイールの操作を無効にする
(setq tabbar-buffer-groups-function nil) ; グループ化しない
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil)))) ; 左に表示されるボタンを無効化する
(setq tabbar-separator '(1.5)) ; タブの長さの設定
;; 外観の設定
(set-face-attribute 'tabbar-default nil
                    :background "blue"
                    :foreground "yellow"
                    :height 0.9) ; defaultの設定
(set-face-attribute 'tabbar-unselected nil
                    :background "blue"
                    :foreground "yellow"
                    :box nil) ; 選択されていない項目の設定
(set-face-attribute 'tabbar-selected nil
                    :background "gray"
                    :foreground "black"
                    :box nil) ; 選択されている項目の設定
(set-face-attribute 'tabbar-button nil
                    :box nil)
(set-face-attribute 'tabbar-separator nil
                    :height 1.5)
;; *scratch*バッファ以外の * で始まるバッファはタブに表示しないように設定する
(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; カレントバッファは必ずいれる
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\ (aref (buffer-name b) 0)) nil)
                     ((equal "*scratch*" (buffer-name b)) b)
                     ((char-equal ?* (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key-Bind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "M-[") 'tabbar-backward)
(define-key global-map (kbd "M-]") 'tabbar-forward)
(define-key global-map (kbd "C--") 'undo)
(define-key global-map (kbd "M-i") 'indent-region)
(define-key global-map (kbd "C-c r") 'replace-string)
(define-key global-map (kbd "C-c c") 'comment-or-uncomment-region)
(define-key global-map (kbd "C-c f") 'flymake-mode)
(define-key global-map (kbd "C-c C-c") 'compile)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "M-r") 'revert-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERMINFO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs が保持する terminfor を使用する
(setq system-uses-terminfor nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 言語を日本語(UTF-8)にする
(set-language-environment 'Japanese);; <- \が円マークになったり日本語が文字化けしたりするのは、こいつが悪いかも
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)

;; gotoした際にエラーメッセージをminibufferに表示する
(defun display-error-message ()
  (message (get-char-property (point) 'help-echo)))
(defadvice flymake-goto-prev-error (after flymake-goto-prev-error-display-message)
  (display-error-message))
(defadvice flymake-goto-next-error (after flymake-goto-next-error-display-message)
  (display-error-message))
(ad-activate 'flymake-goto-prev-error 'flymake-goto-prev-error-display-message)
(ad-activate 'flymake-goto-next-error 'flymake-goto-next-error-display-message)

;; display flymake error in minibuffer
;; https://gist.github.com/415429
(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

;; flymakeのエラー行表示色
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "red"))))
 '(flymake-warnline ((((class color)) (:background "blue")))))

;; "flymakeが落ちる"対策
;; (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;;   (setq flymake-check-was-interrupted t))
;; (ad-activate 'flymake-post-syntax-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpa/yasnippet-0.8.0"))
(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/elpa/yasnippet-0.8.0/snippets"
        ))
(yas-global-mode 1)

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "M-s i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "M-s n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "M-s v") 'yas-visit-snippet-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (add-to-list 'load-path
;;              (expand-file-name "~/.emacs.d/elpa/auto-complete-1.4"))

;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")
;; (ac-config-default)
;; (setq ac-modes (append ac-modes '(objc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-insert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'autoinsert)

(setq auto-insert-directory "~/.emacs.d/insert/")
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-query nil)
(setq auto-insert-alist nil)

(add-to-list 'load-path "~/.emacs.d/insert/")
;;(require 'scala-skelton)
(require 'c++-skeleton)
(require 'yatex-skeleton)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++のflymakeでmakefileを不要にする
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-std=c++11" "-Wall" "-Wextra" "-fsyntax-only" "-Wshadow" "-pedantic-errors" local-file))))
(push '("\\.cc$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.h$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.hpp$" flymake-cc-init) flymake-allowed-file-name-masks)

;; cのflymakeでmakefileを不要にする
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "gcc" (list "-std=c++11" "-Wall" "-Wextra" "-fsyntax-only" "-Wshadow" local-file))))
(push '("\\.c$" flymake-cc-init) flymake-allowed-file-name-masks)

;; C++ mode hook
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "ellemtel")
             ;;(c-set-offset 'innamespace 0)
             (setq c-basic-offset 2)
             (setq tab-width 2)
             (flymake-mode t)))

;; C mode hook
(add-hook 'c-mode-hook
          '(lambda()
             (c-set-style "ellemtel")
             ;;(c-set-offset 'innamespace 0)
             (setq c-basic-offset 2)
             (setq tab-width 2)
             (flymake-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)
(defun flymake-java-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   buffer-file-name
   'flymake-get-java-cmdline))
(defun flymake-get-java-cmdline (source base-dir)
  (list "javac" (list "-J-Dfile.encoding=utf-8" "-encoding" "utf-8"
                      source)))
(push '("\\.java$" flymake-java-init) flymake-allowed-file-name-masks)
(add-hook 'java-mode-hook '(lambda () (flymake-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)
(require 'rust-mode)

;; flymake-rust.el
(defun flymake-rust-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "/usr/local/bin/rustc" (list "--no-trans" local-file))))

(add-to-list 'flymake-allowed-file-name-masks
  '(".+\\.r[cs]$" flymake-rust-init
    flymake-simple-cleanup flymake-get-real-file-name))

(defun flymake-rust-load ()
  (flymake-mode t)
  ;; change these bindings as you see fit
  ;; (local-set-key (kbd "C-c C-d") 'flymake-display-err-menu-for-current-line)
  ;; (local-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
  ;; (local-set-key (kbd "C-c C-p") 'flymake-goto-prev-error)
)
;(add-hook 'rust-mode-hook 'flymake-rust-load)

(provide 'flymake-rust)

;;; racerやrustfmt、コンパイラにパスを通す
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
;;; rustのファイルを編集するときにracerとflymakeを起動する
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flymake-rust-load)))

;;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)
;;; racerの補完サポートを使う
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)
                             ;;; この辺の設定はお好みで
                             (set (make-variable-buffer-local 'company-idle-delay) 0.1)
                             (set (make-variable-buffer-local 'company-minimum-prefix-length) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; flymake for ruby
;; (require 'flymake)
;; ;; Invoke ruby with '-c' to get syntax checking
;; (defun flymake-ruby-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "ruby" (list "-c" local-file))))
;; (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
;; (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
;; (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
;; (add-hook
;;  'ruby-mode-hook
;;  '(lambda ()
;;     ;; Don't want flymake mode for ruby regions in rhtml files
;;     (if (not (null buffer-file-name)) (flymake-mode))
;;     ;; エラー行で C-c d するとエラーの内容をミニバッファで表示する
;;     (define-key ruby-mode-map "\C-cd" 'credmp/flymake-display-err-minibuf)))

;; (defun credmp/flymake-display-err-minibuf ()
;;   "Displays the error/warning for the current line in the minibuffer"
;;   (interactive)
;;   (let* ((line-no             (flymake-current-line-no))
;;          (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
;;          (count               (length line-err-info-list))
;;          )
;;     (while (> count 0)
;;       (when line-err-info-list
;;         (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
;;                (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
;;                (text (flymake-ler-text (nth (1- count) line-err-info-list)))
;;                (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
;;           (message "[%s] %s" line text)
;;           )
;;         )
;;       (setq count (1- count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'haskell-cabal "haskell-cabal" nil t)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;; indent の有効.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;; Haskell Script の編集モード
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

;; ghciとの連携
(setq haskell-program-name "/usr/local/bin/ghci")

;;(add-hook 'haskell-mode-hook 'inf-haskell-mode) ;; enable

;; (defadvice inferior-haskell-load-file (after change-focus-after-load)
;;   "Change focus to GHCi window after C-c C-l command"
;;   (other-window 1))
;; (ad-activate 'inferior-haskell-load-file)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; (add-hook 'haskell-mode-hook (lambda () (flymake-mode)))

;; yesodのhaskellファイル開いたら怒られたから追加
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company racer rust-mode yasnippet typescript markdown-mode magit-tramp ghc flymake)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/scala-mode2/")
(require 'scala-mode2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load the ensime lisp code...
                                        ;(add-to-list 'load-path "~/.emacs.d/elisp/ensime_2.9.2-0.9.8.8/elisp/")
                                        ;(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
                                        ;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yatex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path
(add-to-list 'load-path "~/.emacs.d/elisp/yatex1.77/")

;; yatex-mode
(setq load-path
      (append '("~/lib/emacs/yatex") load-path))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq dvi2-command "xdvi"
      tex-command "platex"
      dviprint-command-format "dvips %s | lpr"
      YaTeX-kanji-code 3)
;; *.texの拡張子をもつファイルを開いた場合、自動的にyatexを起動
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(setq dvi2-command "xdvi"
      tex-command "platex"
      dviprint-command-format "dvips %s | lpr"
      YaTeX-kanji-code 3)

;; 自動改行をオフにする
(add-hook ' yatex-mode-hook'(lambda () (auto-fill-mode -1)))

;; TeX typeset
(setq tex-command "ptex2pdf -u -l -ot \"-kanji=utf8 -no-guess-input-enc -synctex=1\"")
(setq tex-command "pdflatex -synctex=1")
(setq tex-command "lualatex -synctex=1")
(setq tex-command "luajitlatex -synctex=1")
(setq tex-command "xelatex -synctex=1")

;; upbibtexの指定
(setq bibtex-command "upbibtex")

;; biberの設定
(setq bibtex-command "biber --bblencoding=utf8 -u -U --output_safechars")

;; mendexの指定
(setq makeindex-command "mendex -U")

;; color
;(setq YaTeX-use-font-lock t)
; section color
;(setq YaTeX-hilit-sectioning-face '(light時のforecolor/backcolor dark時の forecolor/backcolor))
;(setq YaTeX-hilit-sectioning-face '(white/snow3 snow1/snow3))
(add-hook 'yatex-mode-hook
'(lambda () (require 'font-latex)
            (font-latex-setup)
            (progn
              (modify-syntax-entry ?% "<" (syntax-table))
              (modify-syntax-entry 10 ">" (syntax-table))
              (make-variable-buffer-local 'outline-level)
              (setq outline-level 'latex-outline-level)
              (make-variable-buffer-local 'outline-regexp)
              (setq outline-regexp
                    (concat "[  \t]*\\\\\\(documentstyle\\|documentclass\\|chapter\\|"
                           "section\\|subsection\\|subsubsection\\|paragraph\\)"
                            "\\*?[ \t]*[[{]")
                    ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'markdown-mode "markdown-mode"
"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/helm")
(require 'helm-config)
(helm-mode 1)

;; global-map
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; helm-read-file-map
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; helm-find-files-map
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

(global-set-key (kbd "M-x") 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "$CONF_FILE") (delete-file "$CONF_FILE"))))
