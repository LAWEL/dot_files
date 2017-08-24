;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'exec-path "~/.cabal/bin/")
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac用フォント設定
;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/

;; 英語
(set-face-attribute 'default nil
                    :family "Menlo" ;; font
                    :height 128)    ;; font size
;; 日本語(GUIのみ)
(if (eq window-system 'ns)
    (set-fontset-font
     nil 'japanese-jisx0208
     (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font
  )
;; 半角と全角の比を1:2にしたければ
(setq face-font-rescale-alist
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; Mac用フォント設定
;; オープニングメッセージを表示しない
(setq inhibit-startup-message t)
;; ディスプレイの設定
(display-time-mode 1)
(line-number-mode 1)
(column-number-mode 1)
;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)
;; yes-or-noをy-or-nとする
(defalias 'yes-or-no-p 'y-or-n-p)
;; デフォルトをテキストモードにする
(setq default-major-mode 'text-mode)
;; ~/ を初期ディレクトリにする
(cd "~/")
;; scratchの初期メッセージを消去する
(setq initial-scratch-message "")
;; ビープ音を消す
(setq ring-bell-function 'ignore)
;; スクロールバーを消す(GUIのみ)
(if (eq window-system 'ns)
    (scroll-bar-mode -1)
  )
;; ツールバーを消す(GUIのみ)
(if (eq window-system 'ns)
    (tool-bar-mode -1)
  )
;; メニューバーを消す
(menu-bar-mode -1)
;; 対応す括弧をハイライト
(show-paren-mode t)
;; バックアップを残さない
(setq make-backup-files nil)
;; オートセーブしない
(setq auto-save-default nil)
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
;; markdown-modeでは行末空白削除を無効化(確認できてない)
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'delete-trailing-whitespace) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
             '("melpa" . "https://melpa.org/packages/"))
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
;; key-bind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "M-[") 'tabbar-backward)
(define-key global-map (kbd "M-]") 'tabbar-forward)
(define-key global-map (kbd "C--") 'undo)
(define-key global-map (kbd "C-c i") 'indent-region)
(define-key global-map (kbd "M-%") 'query-replace-regexp)
(define-key global-map (kbd "C-c c") 'comment-or-uncomment-region)
(define-key global-map (kbd "C-c C-c") 'compile)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "M-r") 'revert-buffer)

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

;; command-keyをmeta-keyとする
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; forward-word で単語の先頭に移動する
(defun forward-word+1 ()
  (interactive)
  (forward-word)
  (forward-char))

(global-set-key (kbd "M-f") 'forward-word+1)

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
;; Shell Script
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'shell-script-mode-hook
          '(lambda()
             (setq indent-tabs-mode t
                   tab-width 2
                   sh-basic-offset tab-width)
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ mode hook
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "ellemtel")
             (c-set-offset 'innamespace 0)
             (c-set-offset 'arglist-close 0)
             (c-set-offset 'member-init-intro 0)
             (c-set-offset 'case-label 0)
             (setq indent-tabs-mode t
                   tab-width 4
                   c-basic-offset tab-width)
             ))

;; C mode hook
;; (add-hook 'c-mode-hook
;;           '(lambda()
;;              (c-set-style "ellemtel")
;;              ;;(c-set-offset 'innamespace 0)
;;              (setq c-basic-offset 2)
;;              (setq tab-width 2)
;;              (flymake-mode t)
;;              ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode hook
(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook (lambda ()
                              (guess-style-guess-tab-width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; racerやrustfmt、コンパイラにパスを通す
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))

;;; エラー箇所へ移動するkey-bindの設定
(defun flycheck-rust-init-key ()
  (local-set-key (kbd "M-p") 'flycheck-previous-error)
  (local-set-key (kbd "M-n") 'flycheck-next-error)
  )

;;; rustのファイルを編集するときにracerとflycheckを起動する
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-mode)
                            (flycheck-rust-init-key)
                            ))
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

;; yesodのhaskellファイル開いたら怒られたから追加
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; globalな領域に移動させたい
 '(package-selected-packages
   (quote
    (exec-path-from-shell flycheck-rust company racer rust-mode yasnippet typescript markdown-mode magit-tramp ghc flymake)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))

;; ;; path
;; ;; (add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode-2.8.0/")

;; ;; Flymake Haskell
;; (require 'flymake)
;; (require 'haskell-mode)
;; (require 'haskell-cabal)


;; ;; Haskell mapping
;; (add-to-list 'auto-mode-alist '("//.hs$" . haskell-mode))
;; (add-to-list 'auto-mode-alist '("//.lhs$" . haskell-mode))
;; (add-to-list 'auto-mode-alist '("//.cabal//'$" . haskell-mode))

;; (defun flymake-haskell-make-command (temp-file)
;;   (list "make"
;;         (flymake-haskell-make-parameters temp-file)))

;; (defun flymake-haskell-make-parameters (temp-file)
;;   (list "-s"
;;         "-C"
;;         "."
;;         (concat "CHK_SOURCES=" temp-file)
;;         "SYNTAX_CHECK_MODE=1"
;;         "check-syntax"))

;; (defun flymake-haskell-default-ghc-command (local-file)
;;   (list "ghc"
;;         (flymake-haskell-default-ghc-parameters
;; 	 (file-name-nondirectory local-file))))

;; (defun flymake-haskell-default-ghc-parameters (local-file)
;;   (list "-fno-code" local-file))

;; (defun makefile-exists-p (path)
;;   (file-exists-p (concat path "Makefile")))

;; (defun flymake-haskell-init()
;;   (let* ((temp-file  (flymake-init-create-temp-buffer-copy
;; 		      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;; 		      temp-file
;; 		      (file-name-directory buffer-file-name))))
;;     (if (makefile-exists-p (file-name-directory buffer-file-name))
;;         (flymake-haskell-make-command temp-file)
;;       (flymake-haskell-default-ghc-command local-file))))

;; (push '(".+//hs$" flymake-haskell-init) flymake-allowed-file-name-masks)
;; (push '(".+//lhs$" flymake-haskell-init) flymake-allowed-file-name-masks)
;; (push '("^//(/.+/.hs//|/.lhs//)://([0-9]+//)://([0-9]+//)://(/.+//)" 1 2 3 4)
;;       flymake-err-line-patterns)

;; (custom-set-variables
;;  '(haskell-mode-hook '(turn-on-haskell-indentation)))

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
;;(setq YaTeX-use-font-lock t)
;; section color
;;(setq YaTeX-hilit-sectioning-face '(light時のforecolor/backcolor dark時の forecolor/backcolor))
;;(setq YaTeX-hilit-sectioning-face '(white/snow3 snow1/snow3))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU GLOBAL(gtags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.elの先頭に置くとエラー
;; shell から PATH を引き継ぐ with https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
