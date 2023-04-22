;; configuration 20230422
;;

;; don't show the splash screen
(setq inhibit-startup-message t)


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
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq blink-cursor-interval 0.1)
(setq blink-cursor-delay 10.0)
(blink-cursor-mode 1)
(set-cursor-color "#FFFF00")


;;----------straight----------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil) ;; for >=v27
;;----------------------------
;; use-package
(straight-use-package 'use-package)
;; use-packageをstraight.elにフォールバックする
(setq straight-use-package-by-default t)
;; init-loader
(use-package init-loader)
;;----------------------------

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
;;------------------------------


(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-feather-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons
  :ensure t)
;;-------------------------------------------


;;---switch-window----------------------------------
(use-package switch-window)
(setq switch-window-shortcut-style 'qwerty)
(global-set-key (kbd "C-x o") 'switch-window)
;;--------------------------------------------------


;;----junk-file------------------------------
(use-package open-junk-file)
(setq open-junk-file-format "~/.emacs.d/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
;;--------------------------------------------------


;;----migemo---------------------------------
(use-package migemo)
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\g"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8)
(load-library "migemo")
(migemo-init)
;;-------------------------------------------

;;----auto-save-buffer-----------------------
(use-package auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1.0)
(auto-save-buffers-enhanced t)
;;--------------------------------------------

(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :config
  (global-set-key [(C-tab)] 'vterm-toggle)

  ;; you can cd to the directory where your previous buffer file exists
  ;; after you have toggle to the vterm buffer with `vterm-toggle'.
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

                                        ;Switch to next vterm buffer
  (define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
                                        ;Switch to previous vterm buffer
  (define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward))

;;--------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; frame-movement:
;; switch Emacs frames left/right

(provide 'frame-movement)

(defun frame-movement/frame-rowmajor (frame-a frame-b)
  "Given two frame objects, dig out their X and Y coordinates,
and compare them in row-major order.  The first one is whichever
frame is closer to the top of the screen; of two frames at the
same height, the first one is on the left."
  (let* ((frame-a-pos (frame-position frame-a))
	 (ax (car frame-a-pos))
	 (ay (cdr frame-a-pos))
	 (frame-b-pos (frame-position frame-b))
	 (bx (car frame-b-pos))
	 (by (cdr frame-b-pos)))
    (and (<= ay by)
	 (< ax bx))))

(defun frame-movement/sort-by-position (framelist)
  (sort framelist #'frame-movement/frame-rowmajor))

(defun frame-movement/select-frame-offset (offset)
  "Find the frame that is OFFSET positions away from the currently-selected
frame in the list of all visible frames sorted by their X/Y positions,
and select-frame-set-input-focus it."
  (let* ((sorted-frames-vector (vconcat
				(frame-movement/sort-by-position
				 (visible-frame-list))))
	 (current-frame-index (seq-position sorted-frames-vector 
					    (selected-frame)))
	 (target-index (mod (+ current-frame-index offset) 
			    (length sorted-frames-vector)))
	 (target-frame (elt sorted-frames-vector target-index)))
    (select-frame-set-input-focus target-frame)))

(defun frame-movement/select-next-frame (arg)
  "Switch to the ARG'th rightward adacent visible frame 
at the same height as the currently-selected frame.
If there are no more frames to the right in the same row,
go to the rightmost frame in the next row.  
Otherwise, wrap around to the highest leftmost frame."
  (interactive "p")
  (frame-movement/select-frame-offset arg))

(defun frame-movement/select-prev-frame (arg)
  "Switch to the ARG'th leftward adacent visible frame 
at the same height as the currently-selected frame.
If there are no more frames to the left in the same row,
go to the rightmost frame in the previous row.  
Otherwise, wrap around to the lowest rightmost frame."
  (interactive "p")
  (frame-movement/select-frame-offset (- arg)))

(global-set-key (kbd "C-x 5 n") 'frame-movement/select-next-frame)
(global-set-key (kbd "C-x 5 p") 'frame-movement/select-prev-frame)
