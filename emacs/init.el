;;reference https://qiita.com/hiroakit/items/2199ade2e93d162b118b
(setq coding-key 'utf-8)
(set-default-coding-systems coding-key)
;;maintain japanese font for ansi-term
;;http://d.hatena.ne.jp/inouetakuya/20110624/1308878780
(setq locale-coding-system coding-key)
(set-language-environment 'Japanese)
(prefer-coding-system coding-key)
(set-buffer-file-coding-system coding-key)
(set-terminal-coding-system coding-key)
(set-keyboard-coding-system coding-key)
(setq default-buffer-file-coding-system coding-key)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
;;(global-linum-mode 1)
;; optionキーとcommandキーを，両方meta キーにする
(setq ns-command-modifier (quote meta))
(scroll-bar-mode -1)
(global-auto-revert-mode 1)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(setq default-frame-alist
      (append (list
	       '(width . 100)
	       '(height . 60)
               '(top . 0)
               '(left . -130)
	       )
	      default-frame-alist))
(global-font-lock-mode t)
(setq x-select-enable-clipboard t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(show-paren-mode 1)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(tool-bar-mode 0)
(setq blink-cursor-interval 0.1)
(setq blink-cursor-delay 10.0)
(blink-cursor-mode 1)
(set-cursor-color "#FFFF00")
;;shows the line number and total at mode-line
(setcar mode-line-position
        '(:eval (format "%d" (count-lines (point-max) (point-min)))))
(when (>= emacs-major-version 23)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 130)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
;;   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
   '("Noto Sans Mono CJK JP" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'katakana-jisx0201
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'mule-unicode-0100-24ff
   '("monaco" . "iso10646-1"))
  
  (setq face-font-rescale-alist
        '(
          ;; ("^-apple-hiragino.*" . 1.2)
          ;; (".*osaka-bold.*" . 1.2)
          ;; (".*osaka-medium.*" . 1.2)
          ;; (".*courier-bold-.*-mac-roman" . 1.0)
          ;; (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          ;; (".*monaco-bold-.*-mac-roman" . 0.9)
          ;; ("-cdac$" . 1.3)
          (".*Hiragino Maru Gothic Pro.*" . 1.3))))


;; TODO add migemo

(defconst hp-inits-dir (concat user-emacs-directory "inits"))
(defvar hp-melpa-url "http://melpa.milkbox.net/packages/")
(defvar hp-marmalade-url "http://marmalade-repo.org/packages/")
(defvar hp-use-package-list
  '(
    ;; 以下に使用するパッケージを記述する
    init-loader
    yasnippet
    auto-complete
    foreign-regexp
    web-mode
    js2-mode
    csharp-mode
    cmake-mode
    helm
    ruby-mode
    ruby-additional
    ruby-block
    org-tree-slide
    ;;uniquify
    auto-save-buffers-enhanced
    key-chord
    open-junk-file
    undo-tree
    sequential-command
    smartparens
    )
)

;;; パッケージ
(require 'package)
(add-to-list 'package-archives (cons "melpa" hp-melpa-url))
(add-to-list 'package-archives (cons "marmalade" hp-marmalade-url))
(package-initialize)

;;; 未インストールのパッケージを探す
(require 'cl)
(let ((not-installed 
       (loop for x in hp-use-package-list
             when (not (package-installed-p x)) collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist 
        (pkg not-installed)
        (package-install pkg))))

;;; 各パッケージの設定ファイルはinits以下に置く．init-loaderがそれを読み込む
;;; ファイル命名規則が存在する (例 : 10-hoge.el)
(when (require 'init-loader nil t)
  (setq init-loader-show-log-after-init 'error-only)
  (when (file-directory-p (symbol-value 'hp-inits-dir))
    (init-loader-load hp-inits-dir)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Dropbox/Emacs/org-files/development-projects.org" "~/Dropbox/Emacs/org-files/reading-todos.org")))
 '(package-selected-packages
   (quote
    (smartparens org-tree-slide ruby-block ruby-additional helm cmake-mode csharp-mode js2-mode web-mode foreign-regexp auto-complete yasnippet init-loader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;--------------------------------------------------
;; helm
(require 'popwin)
(popwin-mode 1)
(require 'helm-config)
(helm-mode 1)

;; (defun my-helm ()
;;   (interactive)
;;   (helm :sources '(
;;                    helm-c-source-buffers-list
;;                    helm-c-source-recentf
;;                    helm-c-source-files-in-current-dir
;;                    helm-c-source-mac-spotlight
;;                    helm-c-source-buffer-not-found)
;;         :buffer "*my helm*"))

;; (global-set-key (kbd "C-x b") 'my-helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)

(setq helm-samewindow nil)
(push '("*helm-M-x*") popwin:special-display-config)

;; emacsの終了時に、履歴を保存する
(remove-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)
;; ディレイは0.2秒
(setq helm-input-idle-delay 0.02)
;; 候補のディレクトリが一つしかない場合に、自動的に展開しない
(setq helm-ff-auto-update-initial-value nil)

(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)



(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 0.5) ; 指定のアイドル秒で保存
(auto-save-buffers-enhanced t)
;; 2017-12-02 20:36:19 Saturday
;; sshでアクセスしているときは問題が起きるため
(setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "/sudo:" "/multi:"))


;;; org-mode
;;;
;; temporarily commented out
;; (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link) ;;hyperlink
(define-key global-map "\C-ca" 'org-agenda)
;; (define-key global-map "\C-cr" 'org-remember)

(setq org-return-follows-link t)
(setq org-agenda-files
      (list
       "~/Dropbox/Emacs/org-files/development-projects.org"
       ))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-src-fontify-natively t)
(setq org-hide-leading-stars t)
;;ToDo set-up
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCEL(c)")
	(sequence "REDO(r)" "|" "DONE(d)" "CANCEL(c)")))
(setq org-log-done 'time)
(setq org-tag-alist '(("LEARNING" . ?l) ("REFERENCE" . ?r) ("QUESTION" . ?q) ("UPTOHERE" . ?u)))
;;----------MobileOrg-------------------------------
(setq org-directory "~/Dropbox/Emacs/org-files")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Emacs/org-files/notes.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)


;;--------------------------------------------------
(require 'sequential-command-config)
(sequential-command-setup-keys)
;;--------------------------------------------------
(require 'smartparens-config)
(smartparens-global-mode t)
