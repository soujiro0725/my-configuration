;;--------------------------------------------------
;;------------straight.el -----------------------
;;--------------------------------------------------
;;; code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;--------------------------------------------------
;;-------custom settings--------------------
;;--------------------------------------------------

;; use-package
(straight-use-package 'use-package)

;; use-packageをstraight.elにフォールバックする
(setq straight-use-package-by-default t)

;; init-loader
(use-package init-loader)
;;; ログはエラーが出た時のみ
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(init-loader-show-log-after-init (quote error-only)))

;;--------------------------------------------------
;;-----basic settings-------------------------
;;--------------------------------------------------

(setq coding-key 'utf-8)
(set-default-coding-systems coding-key)
(setq locale-coding-system coding-key)
(set-language-environment 'Japanese)
(prefer-coding-system coding-key)
(set-buffer-file-coding-system coding-key)
(set-terminal-coding-system coding-key)
(set-keyboard-coding-system coding-key)
(setq default-buffer-file-coding-system coding-key)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
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
;;-----custom packages settings-------
;;--------------------------------------------------


;;-----helm--------------------------------------
(use-package popwin)
(popwin-mode 1)

(setq helm-samewindow nil)
;; helm bufferをpopupする
(setq helm-display-function #'display-buffer)
(when (require 'popwin)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config
    '(("*complitation*" :noselect t)
      ("helm" :regexp t :height 0.4))))

(use-package helm)
(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; emacsの終了時に、履歴を保存する
(remove-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)

(use-package helm-swoop)

(global-set-key (kbd "M-s") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;;--------------------------------------------------


;;----recentf-ext-------------------------------
(use-package recentf-ext)
(recentf-mode 1)
(setq recentf-max-saved-items 200)
(setq recentf-save-file "~/.emacs.d/recentf")
(setq recentf-auto-cleanup 'never)
(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
        helm-source-recentf
        helm-source-bookmarks
        helm-source-file-cache
        helm-source-files-in-current-dir
        helm-source-bookmark-set
        helm-source-locate))
;;--------------------------------------------------


;;----company---------------------------------
(use-package company)
(global-company-mode)
(setq company-transformers '(company-sort-by-backend-importance))
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(setq company-selection-wrap-around t)
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection)
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "C-f") 'company-complete-selection)
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)
;;--------------------------------------------------


;;----auto-save-buffer-----------------------
(use-package auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 0.5)
(auto-save-buffers-enhanced t)
;;--------------------------------------------------

;;----smartparens----------------------------
(use-package smartparens)
(smartparens-global-mode t)
;;--------------------------------------------------

;;----org------------------------------------------
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
;;--------------------------------------------------

;;----migemo-----------------------------------
(use-package migemo)
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\g"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8)
(load-library "migemo")
(migemo-init)
;;--------------------------------------------------


;;----junk-file-----------------------------------
(use-package open-junk-file)
(setq open-junk-file-format "~/.emacs.d/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
;;--------------------------------------------------


;;--------------------------------------------------
(load-theme 'manoj-dark t)
;;--------------------------------------------------

;;---smart-mode-line------------------------
(use-package smart-mode-line)
(sml/setup)
(sml/apply-theme 'dark)
(put 'upcase-region 'disabled nil)
;;--------------------------------------------------

;;----sequential command----------------
;; (straight-use-package
;;  '(sequential-command
;;    :type git
;;    :host github
;;    :repo "soujiro0725/sequential-command"))
(use-package sequential-command) ;; no repository for this, so require it!
(require 'sequential-command-config)
(sequential-command-setup-keys)
;;--------------------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;--------------------------------------------------
;;----for programming ----------------------
;;--------------------------------------------------


;;----web-mode--------------------------------
(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(defun web-mode-hook ()
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))
(add-hook 'web-mode-hook 'web-mode-hook)
;;--------------------------------------------------


;;----js2-mode---------------------------------
(use-package js2-mode)
(straight-use-package 'company-tern)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;別途ternをインストールする必要がある
;; npm install -g tern
;;(add-hook 'js2-mode-hook 'tern-mode)
;;(add-to-list 'company-backends 'company-tern)
;;--------------------------------------------------


;;----fly-check---------------------------------
;;(use-package flycheck)
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;;--------------------------------------------------


;;----dockerfile--------------------------------
(use-package dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
;;--------------------------------------------------

;;----yaml--------------------------------
(use-package yaml-mode)
;;--------------------------------------------------

;;----lsp -----------------------------------------
;;(setq lsp-clients-python-executable "~/.pyenv/shims/python")

(add-to-list 'exec-path "~/.pyenv/shims")


(use-package lsp-mode
  :commands lsp)
(use-package company-lsp)
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package python-mode
  :config
  (require 'lsp-clients)
  (add-hook 'python-mode-hook #'lsp))

;; pip install python-language-server
;; pip install pyls-black

;;--------------------------------------------------


;;-----ein ----------------------------------------
(use-package ein)
;;--------------------------------------------------
