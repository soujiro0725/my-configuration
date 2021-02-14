;;; package --- ------------------------------------
;;; Commentary:
;;------------straight.el --------------------------
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
;;-------custom settings----------------------------
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
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(init-loader-show-log-after-init (quote error-only)))


;;--------------------------------------------------
;;-----basic settings-------------------------------
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
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(setq default-frame-alist
      (append (list
	       '(width . 100)
	       '(height . 60)
               '(top . 0)
               '(left . -130))
              default-frame-alist))
(global-font-lock-mode t)
(setq select-enable-clipboard t)
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

;; python-help bufferが自動的に立ち上がるのを防ぐ
(global-eldoc-mode -1)
;; 追加で設定
(add-hook 'python-mode-hook
          (lambda ()
            (kill-local-variable 'eldoc-documentation-function)))

;; 表示がずれるので、デフォルトではオフにする
;;(global-display-line-numbers-mode)

;;shows the line number and total at mode-line
(setcar mode-line-position
        '(:eval (format "%d" (count-lines (point-max) (point-min)))))

;; font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120)
(setq face-font-rescale-alist
      '(
        (".*Hiragino Maru Gothic Pro.*" . 0.9)))


;;----frame-cmds------------------------------------
(use-package frame-cmds)
;;--------------------------------------------------


;;--------------------------------------------------
;;-----custom packages settings-------
;;--------------------------------------------------

;;---projectile-------------------------------------
(use-package projectile)
(projectile-mode +1)
;;--------------------------------------------------


;;-----helm--------------------------------------
(use-package popwin)
(popwin-mode 1)


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

(use-package helm-projectile)
(global-set-key (kbd "C-c p") 'helm-projectile)
;; original key bind
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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


;;----auto-save-buffer-----------------------
(use-package auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1.0)
(auto-save-buffers-enhanced t)
;;--------------------------------------------------


;;----smartparens----------------------------
(use-package smartparens)
(smartparens-global-mode t)
;;--------------------------------------------------


;;----org------------------------------------------
(define-key global-map "\C-cl" 'org-store-link) ;;hyperlink
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(setq org-return-follows-link t)
(setq org-agenda-files
      (list
       "~/Dropbox/Emacs/org-files/development-projects.org"
       ))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-src-fontify-natively t)
(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)
;;ToDo set-up
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCEL(c)")
	(sequence "REDO(r)" "|" "DONE(d)" "CANCEL(c)")))
(setq org-log-done 'time)
(setq org-tag-alist '(("LEARNING" . ?l) ("REFERENCE" . ?r) ("QUESTION" . ?q) ("UPTOHERE" . ?u)))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-agenda-current-time-string "← now")
(setq org-agenda-time-grid ;; Format is changed from 9.1
      '((daily today require-timed)
        (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
        "-"
"────────────────"))
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


;;---which-key--------------------------------------
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))
;;--------------------------------------------------


;;--------------------------------------------------
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-old-hope t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))


(use-package doom-modeline
      :custom
      (doom-modeline-buffer-file-name-style 'truncate-with-project)
      (doom-modeline-icon t)
      (doom-modeline-major-mode-icon nil)
      (doom-modeline-minor-modes nil)
      :hook
      (after-init . doom-modeline-mode)
      :config
      (line-number-mode 0)
      (column-number-mode 0)
      (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))
;;--------------------------------------------------


;;---dashboard--------------------------------------
(use-package dashboard
  :bind (("<f10>" . open-dashboard)
     :map dashboard-mode-map
     ("c" . browse-calendar)
     ("w" . browse-tenki)
     ("m" . browse-gmail)
     ("t" . browse-tweetdeck)
     ("s" . browse-slack)
     ("h" . browse-homepage)
     ("l" . line-app-open)
     ("<f10>" . quit-dashboard))
  :hook (after-init . dashboard-setup-startup-hook)
  :config
  ;; Set the title
  (when (eq system-type 'darwin)
    (setq dashboard-banner-logo-title
      (concat "GNU Emacs " emacs-version " kernel "
          (car (split-string (shell-command-to-string "uname -r")))  " x86_64 Mac OS X "
          (car(split-string (shell-command-to-string "sw_vers -productVersion") "-")))))
  (when (eq system-type 'gnu/linux)
    (setq dashboard-banner-logo-title
      (concat "GNU Emacs " emacs-version " kernel "
          (car (split-string (shell-command-to-string "uname -r")))  " x86_64 Debian GNU/Linux "
          (car (split-string (shell-command-to-string "cat /etc/debian_version") "_")))))
  ;; Set the banner
  (setq dashboard-startup-banner "~/Projects/my-configuration/emacs/emacs.png")

  ;; Use icons
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;; Set the footer
  (setq dashboard-footer "Always be joyful. Never stop praying. Be thankful in all circumstances!")
  (setq dashboard-footer-icon
    (all-the-icons-octicon "dashboard" :height 1.1 :v-adjust -0.05 :face 'font-lock-keyword-face))
  (when (eq system-type 'darwin)
    (setq dashboard-items '((recents  . 10))))
  (when (eq system-type 'gnu/linux)
    (setq dashboard-items '((recents  . 8))))

  ;; Insert custom item
  (defun dashboard-insert-custom (list-size)
    (insert (if (display-graphic-p)
        (all-the-icons-faicon "google" :height 1.2 :v-adjust -0.05 :face 'error) " "))
    (insert "   Calendar: (c)    Weather: (w)    Mail: (m)    Twitter: (t)    LINE: (l)    Slack: (s)    GH: (h)"))
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t)

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (delete-other-windows)
    ;; Refresh dashboard buffer
    (if (get-buffer dashboard-buffer-name)
    (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    ;; Jump to the first section
    (goto-char (point-min))
    (dashboard-goto-recent-files))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
           (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil)))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (funcall (local-key-binding "r"))))
;;--------------------------------------------------


;;----indent----------------------------------------
(use-package highlight-indent-guides
    :diminish
    :hook
    ((prog-mode yaml-mode) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled t)
    (highlight-indent-guides-responsive t)
    (highlight-indent-guides-method 'character)) ; column
;;--------------------------------------------------


;;---switch-window----------------------------------
(use-package switch-window)
(setq switch-window-shortcut-style 'qwerty)
(global-set-key (kbd "C-x o") 'switch-window)
;;--------------------------------------------------


;;---undo-tree--------------------------------------
(use-package undo-tree)
(global-undo-tree-mode)
;;--------------------------------------------------


;;---visual regexp----------------------------------
(use-package visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;;--------------------------------------------------


;;----sequential command----------------
(straight-use-package
 '(sequential-command
   :type git
   :host github
   :repo "soujiro0725/sequential-command"))
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


;;----dockerfile--------------------------------
(use-package dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
;;--------------------------------------------------


;;----yaml--------------------------------
(use-package yaml-mode)
;;--------------------------------------------------


;;----yasnippet--------------------------------
(use-package yasnippet)
(setq yas-snippet-dirs
      '("~/Dropbox/Emacs/snippets"))
(yas-global-mode 1)
;;--------------------------------------------------


;;----lsp -----------------------------------------
(use-package lsp-mode
  :commands lsp)
(use-package company-lsp)
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company
  :config
  (global-company-mode)
  (push 'company-lsp company-backends))

(use-package python-mode
  :config
  (add-hook 'python-mode-hook #'lsp))

(add-to-list 'exec-path "~/.pyenv/shims")
(setq python-shell-interpreter "~/.pyenv/shims/python")

;; for use lsp, run the following lines
;; pip install python-language-server
;; pip install pyls-black

;; for c/c++
(use-package ccls
  :custom (ccls-executable "/usr/local/bin/ccls")
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp))))

;; for go
(use-package go-mode
    :config
    (add-hook 'go-mode-hook #'lsp))

;;---fly-check--------------------------------------
(use-package flycheck)
(global-flycheck-mode)
;;--------------------------------------------------


;;-----ein -----------------------------------------
(use-package ein)
;;--------------------------------------------------


;;---for gsx only-----------------------------------
(let ((local-settings "~/.emacs.d/gsx_init.el"))
 (when (file-exists-p local-settings)
   (load-file local-settings)))
;;--------------------------------------------------


;;---persp-mode----------------------------------

(use-package persp-mode
  :ensure t
  :commands persp-mode
  :config
  (set-face-attribute 'persp-selected-face nil :foreground "#81a2be"))

;;--------------------------------------------------

;;---minimap----------------------------------------
(use-package minimap
  :commands
  (minimap-create minimap-kill)
  :custom
  (minimap-major-modes '(prog-mode))

  (minimap-window-location 'right)
  (minimap-update-delay 0.2)
  (minimap-minimum-width 20)
  ;; :bind
  ;; ("C-c m" . soujiro0725/toggle-minimap)
  ;; :preface
  ;; (defun soujiro0725/toggle-minimap ()
  ;;   "Toggle minimap for current buffer."
  ;;   (interactive)
  ;;   (if (null (get-buffer minimap-buffer-name))
  ;;       (minimap-create)
  ;;     (minimap-kill)))
  :config
  (custom-set-faces
   '(minimap-active-region-background
     ((((background dark)) (:background "#555555555555"))
      (t (:background "#C847D8FEFFFF"))) :group 'minimap)))
;;--------------------------------------------------


;;---eshell---------------------------------------

(defun eshell-on-current-dir (&optional arg)
  "invoke eshell and cd to current directory"
  (interactive "P")
  (let ((dir default-directory))
    (eshell arg)
    (cd dir))
  (eshell-emit-prompt)
  (goto-char (point-max)))

(add-to-list 'popwin:special-display-config
             '("\\`\\*eshell" :regexp t :dedicated t :position bottom
               :height 0.3))

;;--------------------------------------------------

;;for elips under development 
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$")))
  (let ((dev-dir "~/.emacs.d/dev/eide"))
    (when (file-exists-p dev-dir)
      (load-directory "~/.emacs.d/dev/eide/src/"))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'init)
;;; init.el ends here
