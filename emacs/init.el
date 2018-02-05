(setq load-path (cons "~/.emacs.d/lib/" load-path))

;;--------------------------------------------------
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; installed for ensime
;; 2017-05-26 10:12:00 金曜日
(require 'use-package)
;;--------------------------------------------------
;;
;;coding system
;;
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
;;(setq ns-alternate-modifier (quote meta))
;; システムへ修飾キーを渡さない設定
;;(setq mac-pass-control-to-system nil)
;;(setq mac-pass-command-to-system nil)
;;(setq mac-pass-option-to-system nil)

;;--------------------------------------------------
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
;;--------------------------------------------------
;; font

(when (>= emacs-major-version 23)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 130)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
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
;;--------------------------------------------------
;; helm setting requires popwin
(require 'popwin)
(popwin-mode 1)
;;--------------------------------------------------
;; helm
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
;;--------------------------------------------------
(require 'uniquify)
;;(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;;(setq uniquify-ignore-buffers-re "*[^*]+*")

(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 0.5) ; 指定のアイドル秒で保存
(auto-save-buffers-enhanced t)
;; 2017-12-02 20:36:19 Saturday
;; sshでアクセスしているときは問題が起きるため
(setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "/sudo:" "/multi:"))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "wf" (setq set-input-method 'next-multiframe-window))
(key-chord-define-global "wp" (setq set-input-method 'previous-multiframe-window))
;;--------------------------------------------------
;;;
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
       "~/Dropbox/Emacs/org-files/gsx.org"
       "~/Dropbox/Emacs/org-files/development-projects.org"
       "~/Dropbox/Emacs/org-files/reading-todos.org"
       "~/Dropbox/Emacs/org-files/information-security-specialist.org"
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

;; custom functions

(defun si/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

;;org-publish---------------------------------------
(setq org-publish-project-alist
      '(
        ("ai-specification-org"
         :base-directory "~/Dropbox/Projects/AIrobotics/doc/org/"
         :recursive t
         :publishing-directory "~/Dropbox/Projects/AIrobotics/doc/html/"
         :publishing-function org-html-publish-to-html)
        ;; ("ai-specification-md"
        ;;  :base-directory "~/Dropbox/Projects/AIrobotics/doc/org/"
        ;;  :recursive t
        ;;  :publishing-directory "~/Dropbox/Projects/AIrobotics/doc/"
        ;;  :publishing-function org-md-export-to-markdown)
        ("ai-specification-images-md" ;;for markdown
         :base-directory "~/Dropbox/Projects/AIrobotics/doc/org/images/"
         :recursive t
         :base-extension "svg\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/Projects/AIrobotics/doc/images/"
         :publishing-function org-publish-attachment)
        ("ai-specification-images-html" ;;for html
         :base-directory "~/Dropbox/Projects/AIrobotics/doc/org/images/"
         :recursive t
         :base-extension "svg\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/Projects/AIrobotics/doc/html/images/"
         :publishing-function org-publish-attachment)
        ("AIrobotics" :components ("ai-specification-org"
                                   ;;"ai-specification-md"
                                   ;;"ai-specification-images-md"
                                   "ai-specification-images-html"))))

;; org-mode html export options
(setq org-html-html5-fancy t)
(setq org-html-doctype "html5")
(setq org-html-use-infojs nil)
(setq org-html-allow-name-attribute-in-anchors nil)
(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "htmlized-")
(setq org-html-text-markup-alist
      '((bold           . "<strong>%s</strong>")
        (italic         . "<em>%s</em>")
        (code           . "<code>%s</code>")
        (strike-through . "<span style=\"strike\">%s</span>")
        (underline      . "<span class=\"underline\">%s</span>")
        (verbatim       . "<code>%s</code>")))
(setq org-html-head-include-scripts nil)
;;(setq org-html-postamble nil)
(setq org-html-postamble 
      '(("en" "<p class=\"postamble\">Last Updated %d. Created by %c"</p>)))
(setq org-confirm-babel-evaluate nil)

;;-- plantuml------------------------------
;; org-plantuml-jar-path は plantuml.jar へのパス
(setq org-plantuml-jar-path "~/.emacs.d/elpa/contrib/scripts/plantuml.jar")
(defun org-mode-init ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages '(plantuml . t))))
(add-hook 'org-mode-hook 'org-mode-init)

;;--------------------------------------------------
;;
;; auto-complete
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (global-auto-complete-mode t)
;; (setq ac-auto-start 2)  ;; n文字以上の単語の時に補完を開始
;; (setq ac-delay 0.05)  ;; n秒後に補完開始
;; (setq ac-use-fuzzy t)  ;; 曖昧マッチ有効
;; (setq ac-use-comphist t)  ;; 補完推測機能有効
;; (setq ac-auto-show-menu 0.05)  ;; n秒後に補完メニューを表示
;; (setq ac-quick-help-delay 0.5)  ;; n秒後にクイックヘルプを表示
;; (setq ac-ignore-case nil)  ;; 大文字・小文字を区別する
;; ;;--------------------------------------------------
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))
;;--------------------------------------------------

;;;---------------------------------
;;; R & ESS の設定
;;;---------------------------------
;;(setq exec-path (cons "/Applications/R.app/Contents/MacOS/R" exec-path))
(setq-default inferior-R-program-name "/usr/local/Cellar/r/3.2.3_1/bin/R")
;; 拡張子が r, R の場合に R-mode を起動
(add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))
;; R-mode を起動する時に ess-site をロード
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
;; R を起動する時に ess-site をロード
(autoload 'R "ess-site" "start R" t)

(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; R-mode, iESS を起動する際に呼び出す初期化関数
(setq ess-loaded-p nil)
(defun ess-load-hook (&optional from-iess-p)
  ;; インデントの幅を4にする（デフォルト2）
  (setq ess-indent-level 4)
  ;; インデントを調整
  (setq ess-arg-function-offset-new-line (list ess-indent-level))
  ;; comment-region のコメントアウトに # を使う（デフォルト##）
  (make-variable-buffer-local 'comment-add)
  (setq comment-add 0)

  ;; 最初に ESS を呼び出した時の処理
  (when (not ess-loaded-p)
    ;; アンダースコアの入力が " <- " にならないようにする
    (ess-toggle-underscore nil)
    ;; C-c r を押した際に表示される候補数の上限値
    ;; 表示数が多いと処理が重くなる
    (setq anything-R-help-limit 40)
    (setq anything-R-local-limit 20)
    ;; C-c r で R の関数やオブジェクトを検索できるようにする
    (when (require 'anything-R nil t)
      ;; ess-smart-comma が導入されたので repospkg と localpkg はあまり必要なさそう
      (setq anything-for-R-list '(anything-c-source-R-help
                                  anything-c-source-R-local))
      (define-key ess-mode-map (kbd "C-c r") 'anything-for-R)
      (define-key inferior-ess-mode-map (kbd "C-c r") 'anything-for-R))
    ;; C-c C-g で オブジェクトの内容を確認できるようにする
    (require 'ess-R-object-popup nil t)
    ;; 補完機能を有効にする
    ;;(setq ess-use-auto-complete t)
    ;; anything を使いたいので IDO は邪魔
    (setq ess-use-ido nil)
    ;; キャレットがシンボル上にある場合にもエコーエリアにヘルプを表示する
    (setq ess-eldoc-show-on-symbol t)
    ;; 起動時にワーキングディレクトリを尋ねられないようにする
    (setq ess-ask-for-ess-directory nil)
    ;; # の数によってコメントのインデントの挙動が変わるのを無効にする
    (setq ess-fancy-comments nil)
    (setq ess-loaded-p t)
    (unless from-iess-p
      ;; ウィンドウが1つの状態で *.R を開いた場合はウィンドウを縦に分割して R を表示する
      (when (one-window-p)
        (split-window-horizontally)
        (let ((buf (current-buffer)))
          (ess-switch-to-ESS nil)
          (switch-to-buffer-other-window buf)))
      ;; R を起動する前だと auto-complete-mode が off になるので自前で on にする
      ;; cf. ess.el の ess-load-extras
      (when (and ess-use-auto-complete (require 'auto-complete nil t))
        (add-to-list 'ac-modes 'ess-mode)
        (mapcar (lambda (el) (add-to-list 'ac-trigger-commands el))
                '(ess-smart-comma smart-operator-comma skeleton-pair-insert-maybe))
        (setq ac-sources '(ac-source-R ac-source-filename)))))

  (if from-iess-p
      ;; R のプロセスが他になければウィンドウを分割する
      (if (> (length ess-process-name-list) 0)
          (when (one-window-p)
            (split-window-horizontally)
            (other-window 1)))
    ;; *.R と R のプロセスを結びつける
    ;; これをしておかないと補完などの便利な機能が使えない
    (ess-force-buffer-current "Process to load into: ")))

;; R-mode 起動直後の処理
(add-hook 'R-mode-hook 'ess-load-hook)

;; R 起動直前の処理
(add-hook 'ess-pre-run-hook (lambda () (ess-load-hook t)))

;;cmigemo-------------------------------------------
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\g"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8)
(load-library "migemo")
(migemo-init)

;;--------------------------------------------------
;;junk file
(require 'open-junk-file)
(setq open-junk-file-format "~/.emacs.d/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
;;--------------------------------------------------
(defun si/insert-now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %H:%M:%S %A")))

(defun si/insert-today ()
  "Insert string for today's date nicely formatted in American style,
e.g. 2014-06-08"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))

(defun si/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
;;--------------------------------------------------
;;undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
;;--------------------------------------------------
(require 'sequential-command-config)
(sequential-command-setup-keys)
;;--------------------------------------------------
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/Dropbox/Emacs/snippets"))
(yas-global-mode 1)
;;--------------------------------------------------
;; (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
;;--------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("b300379af88fdc3acda2bca448bf970a4c6ce6cc0b5099bce3a5d9f070dbdb8c" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "dacce23bc9bace2248ebbd89756fd74f213c754b7b022fa3f090e220faf1a813" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "63dd8ce36f352b92dbf4f80e912ac68216c1d7cf6ae98195e287fd7c7f7cb189" "23ccf46b0d05ae80ee0661b91a083427a6c61e7a260227d37e36833d862ccffc" "2dd75b609bc7dfeba3933f4273a7ae7610247926a113ed888f6915333f6866d9" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "e56f1b1c1daec5dbddc50abd00fcd00f6ce4079f4a7f66052cf16d96412a09a9" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default)))
 '(js2-basic-offset 2)
 '(org-agenda-files
   (quote
    ("~/Dropbox/Emacs/org-files/development-projects.org" "~/Dropbox/Emacs/org-files/reading-todos.org" "~/Dropbox/Emacs/org-files/information-security-specialist.org" "~/Dropbox/Emacs/org-files/gsx.org")))
 '(package-selected-packages
   (quote
    (pyim htmlize rubocop origami birds-of-paradise-plus-theme autumn-light-theme yascroll yaml-mode web-mode visual-regexp-steroids use-package undo-tree tao-theme smartparens smart-mode-line-powerline-theme sequential-command scala-mode2 robe request-deferred plantuml-mode org open-junk-file neotree minimap migemo markdown-mode key-chord js2-mode highlight-indentation helm groovy-mode gradle-mode google-c-style flycheck ess ensime enh-ruby-mode ein dockerfile-mode cmake-mode badger-theme auto-save-buffers-enhanced all-the-icons alert)))
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-table ((t (:foreground "LightSkyBlue")))))
;;--------------------------------------------------
;; theme load must come after "custom-safe-themes"
(load-theme 'badger t)
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'powerline)
(put 'upcase-region 'disabled nil)

;;--------------------------------------------------
(setq-default highlight-indentation-mode t)
;;--------------------------------------------------


;;--------------------------------------------------
;; special function for org-mode
(defun show-org-buffer (file)
  "Show an org-file on the current buffer"
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/Dropbox/Emacs/org-files/" file))))

(global-set-key (kbd "C-M-c")
                '(lambda ()
                   (interactive)
                   (show-org-buffer "schedule.org")))

(defvar org-capture-ical-file (concat org-directory "schedule.org"))
;; see org.pdf:p73
(setq org-capture-templates
      `(("t" "TODO 項目を INBOX に貼り付ける" entry
         (file+headline nil "INBOX") "** TODO %?\n\t")
        ("c" "同期カレンダーにエントリー" entry
         (file+headline ,org-capture-ical-file "Schedule")
         "** TODO %?\n\t")))

(setq org-refile-targets
      (quote (("schedule.org" :level . 1)
              ("next.org" :level . 1)
              ("sleep.org" :level . 1))))

(defun si/org-export-icalendar ()
  (interactive)
  (org-icalendar-export-to-ics nil "~/Dropbox/Emacs/org-files/development-projects.org"))
(define-key org-mode-map (kbd "C-c 1") 'si/org-export-icalendar)

;; iCal の説明文
(setq org-icalendar-combined-description "OrgModeのスケジュール出力")
;; カレンダーに適切なタイムゾーンを設定する（google 用には nil が必要）
(setq org-icalendar-timezone nil)
;; DONE になった TODO は出力対象から除外する
(setq org-icalendar-include-todo t)
;; （通常は，<>--<> で区間付き予定をつくる．非改行入力で日付がNoteに入らない）
(setq org-icalendar-use-scheduled '(event-if-todo))
;; DL 付きで終日予定にする：締め切り日（スタンプで時間を指定しないこと）
(setq org-icalendar-use-deadline '(event-if-todo))


;;--------------------------------------------------
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
;;--------------------------------------------------
;; 
;;Emacsで現在のいるディレクトリをiTermでひらく．またその逆 - Qiita
;;http://qiita.com/ganmacs/items/cfc5f9c2213a6a9e6579
(defun cd-on-iterm ()
  (interactive)
  (util/execute-on-iterm
   (format "cd %s" default-directory)))

(defun util/execute-on-iterm (command)
  (interactive "MCommand: ")
  (do-applescript
   (format "tell application \"iTerm2\"
              activate
              tell current session of current window
                write text \"%s\"
              end tell
            end tell"
           command)))
(global-set-key (kbd "C-c d") 'cd-on-iterm)
;;--------------------------------------------------
(global-company-mode)
(global-yascroll-bar-mode 1)
;;--------------------------------------------------
(require 'smartparens-config)
(smartparens-global-mode t)
;;--------------------------------------------------
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'ruby-mode-hook 'flycheck-mode)
(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
;;--------------------------------------------------
(defvar emacs-root
  (eval-when-compile
    '(
      (require 'visual-regexp-steroids)
      )))
;; 上記のように書かないと、コンパイル時に `regexp-string' 由来の警告を吐く

(setq vr/engine 'python) ; 'python, 'pcre2el, or 'emacs
;; python が インストールされてない環境では、上１行をコメントアウト、下２行をコメント解除
;; (setq vr/engine 'pcre2el)
;; (require 'pcre2el)

;; multiple-cursors ( https://github.com/magnars/multiple-cursors.el ) を使っている場合は下１行をコメント解除
; (global-set-key (kbd "C-c m") 'vr/mc-mark)
;; 普段の 'query-replace-regexp を visual-regexp に
;;(define-key global-map (kbd "C-x r") 'vr/query-replace)

;;--------------------------------------------------
;; change transparency
(defun si/set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))
;;--------------------------------------------------
;; TRAMPを使ってリモートのファイルを弄る - Qiita
;;http://qiita.com/miyakou1982/items/d05e1ce07ad632c94720
;; (add-to-list 'tramp-default-proxies-alist
;;              '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;;              '("pi3" "pi" "/ssh:pi@192.168.11.9:"))
;; (add-to-list 'tramp-default-proxies-alist
;;              '((regexp-quote (system-name)) nil nil))


;;--------------------------------------------------
;; ensime
(setenv "PATH" (concat "PATH_TO_SBT:" (getenv "PATH")))
(setenv "PATH" (concat "PATH_TO_SCALA:" (getenv "PATH")))

(require 'scala-mode2)
(require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq ensime-startup-notification nil)
;;--------------------------------------------------
;; scalafmt for emacs
(defun ensime-sbt-do-fmt ()
  "WORKAROUND "
  (interactive)
  (sbt:command "fmt"))
;;--------------------------------------------------
;;(require 'all-the-icons)
(require 'all-the-icons)
(require 'neotree)
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(global-set-key [f8] 'neotree-toggle)
(setq neo-show-hidden-files t)
(setq neo-create-file-auto-open t)
;;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;--------------------------------------------------
;; window size
;; 自分用に開発2017-02-12 13:24:49 日曜日
;; Window 分割を画面サイズに従って計算する
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

;; Window 分割・移動を C-t で
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;;--------------------------------------------------
(require 'tramp)
(setq tramp-default-method "ssh")
;;--------------------------------------------------
;; pyim mode
(require 'pyim)
(require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
(pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
;;(setq default-input-method "pyim")
;;--------------------------------------------------
