;; https://lupan.pl/emacs.html
(require 'cl)

;; Directory with local Emacs lisp files
(let ((path (expand-file-name "~/.emacs.d/lisp")))
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path)))

;; M-x eval-buffer
;; M-x package-refresh-contents
;; M-x package-install RET package-name RET

;; Add MELPA package list
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stb" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/"))
      tls-checktrust t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h")
      gnutls-verify-error t)

(package-initialize)

(require 'use-package) ;; requires installing package "use-package"

;;
;; BUFFER, FILE AND WINDOW SELECTION ENHANCEMENTS
;;

;; Use more efficient buffer/file selection
(use-package helm
  :ensure nil
  :init
  (setq help-split-window-default-side 'other)
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x C-f" . helm-find-files)
   ("C-c o" . helm-occur)
   ("C-x b" . helm-mini)
   ("C-h a" . helm-apropos)
   ("C-h d" . helm-info-at-point)
   ("C-c L" . helm-locate)
   ("C-c r" . helm-resume)
   ("C-c i" . helm-imenu)))

(use-package helm-swoop
  :ensure nil
  :bind
  (("C-S-s" . helm-swoop)))

(use-package helm-descbinds
  :ensure nil
  :init
  (helm-descbinds-mode))

(use-package helm-git-grep
  :ensure nil
  :bind
  (("C-c j" . helm-git-grep)
   ("C-c J" . helm-git-grep-at-painf)))

(use-package helm-ls-git
  :ensure nil
  :bind
  (("C-c g" . helm-ls-git-ls)))

(use-package helm-c-yasnippet
  :ensure nil
  :bind
  (("C-c y" . helm-yas-complete)))

;; Use more efficient changing windows
(use-package ace-window
  :ensure nil
  :bind
  (("C-x o" . ace-window)))

;; TODO Check the functional and keybindings
(use-package windmove
  :ensure nil
  :demand
  :bind
  (("C-S-n" . windmove-down)
   ("C-S-p" . windmove-up)
   ("C-S-b" . windmove-left)
   ("C-S-f" . windmove-right))
  :config
  (windmove-default-keybindings))

;; A like tabs
(use-package helm-spaces
  :ensure nil
  :bind
  (("C-c b" . helm-spaces)))

;; Allow for undo/redo of window manipulations (sush as C-x 1)
(winner-mode 1) ;; C-c left/C-c right

;; Remind of keys than can follow a key sequence
(use-package which-key
  :ensure nil
  :config
  (which-key-mode 1))

(use-package avy
  :ensure nil
  :bind
  (("C-:" . avy-goto-char-timer)))

(use-package treemacs
  :ensure nil
  :bind
  (("C-c t" . treemacs-toggle)))

;;
;; EDITING ENHANCEMENTS
;;

(use-package better-defaults
  :ensure nil
  :defer)

;; Context aware insertion of pairs parenthesis
(use-package smartparens
  :ensure nil
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(")   ;; how do people live without this?
  (sp-pair "[" "]" :wrap "C-c [") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "S-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "S-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))


;; Edit with multiple cursors
(use-package multiple-cursors
  :ensure nil
  :bind
  (("C-c n" . mc/mark-next-like-this)
   ("C-c p" . mc/mark-previous-like-this)))

;;
;; APPEARANCE
;;

(setq inhibit-startup-screen t
      ediff-window-setup-function #'ediff-setup-windows-plain)
(set-scroll-bar-mode 'right)
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode  0)
;; (scroll-bar-mode   0)          ;; disable scroll-bar
;; (blink-cursor-mode 0)          ;; disable blink cursor
(setq use-dialog-box nil)         ;; disable gui dialog window
(setq redisplay-dont-pause t)     ;; advanced displaing of buffer
(setq ring-bell-function 'ignore) ;; disable bell

;; (setq require-final-newline t)
;; (setq-default require-final-newline t)

;; Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; Linum plugin
(require 'linum)
(line-number-mode   t)    ;; show number of string in mode-line
(global-linum-mode  t)    ;; show nubers of strings at all buffers
(column-number-mode t)    ;; show number of column at mode-line
(setq linum-format " %d") ;; format of number strings

;; Fringe setting
(fringe-mode '(8 . 0)) ;;  make the left fringe 8 pixels wide and the right disappear
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Scrolling setting
(setq scroll-step               1)
(setq scroll-margin             5)
(setq scroll-conservatively 10000)

;; Show-paren-mode settings
;; (show-paren-mode t) ;; enable highlight expressions between {},[],()
;;(setq show-paren-style 'expression) ;; enable color higlight expression between {},[],()

;; Show messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight current line
(global-hl-line-mode t)


(defun set-frame-font-hack (size &optional frames)
  "Set font to Hack:pixelsize=SIZE:antialias=true:autohint=false.
Argument FRAMES has the same meaning as for `set-frame-font'"
  (interactive "n[Hack] size: ")
  (set-frame-font (format "Hack:pixelsize=%d:antialias=true:autohint=true" size) nil frames))

(defun set-frame-font-anonymous-pro (size &optional frames)
  "Set font to Anonymous Pro:pixelsize=SIZE:antialias=true:autohint=false.
Argument FRAMES has the same meaning as for `set-frame-font'"
  (interactive "n[Anonymous Pro] size: ")
  (set-frame-font (format "Anonymous Pro:pixelsize=%d:antialias:true:autohint=true" size) nil frames))

(use-package powerline
  :ensure nil
  :defer)

(use-package solarized-theme
  :ensure nil
  :defer)

;; easy switch between themes
(use-package helm-themes
  :ensure nil
  :defer
  :config
  ;; need to update powerline after changing theme
  (advice-add 'helm-themes :after #'powerline-reset))

(defun my-make-frame-function(frame)
  (if (not (featurep 'powerline))
      (powerline-center-theme)))

(setq my-dark-theme 'solarized-dark
      my-light-theme 'solarized-light)

(defun my-light-theme ()
  "Switch to light theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (when (load-theme my-light-theme t)
    (powerline-reset)))

(defun my-dark-theme ()
  "Switch to dark theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (when (load-theme my-dark-theme t)
    (powerline-reset)))

(when window-system
  (my-make-frame-function (selected-frame))
  (set-frame-size (selected-frame) 160 50)
  (set-frame-position (selected-frame) 130 10)
  (load-theme 'solarized-light t)
  (set-frame-font (format "Hack:pixelsize=%d:antialias:true:autohint=true" 16)))

(add-hook 'after-make-frame-functions
	  #'my-make-frame-function)

;;
;; CONVENIENCE FUNCTIONS, ALIASES, AND KEY BINDINGS
;;

(defun am ()
  "Change dictionary to american."
  (interactive)
  (setq ispell-local-dictionary "american"))

(defun ru ()
  "Change dictionary to russian."
  (interactive)
  (setq ispell-local-dictionary "russian"))

;; (defalias 'fi #'set-frame-font-inconsolata)
;; (defalias 'fa #'set-frame-font-anonymous-pro)
(defalias 'st #'magit-status)
(defalias 'ir #'ispell-region)
(defalias 'md #'markdown-mode)

;; Disable backup/autosave files
(setq make-backup-files nil)
;; (setq auto-save-default nil)
(setq auto-save-list-file-name nil)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;; Auto-complete
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "S-SPC") 'dabbrev-expand)

;; Bind keys
(global-set-key (kbd "C-c k") #'compile)
(global-set-key (kbd "C-c q") #'bury-buffer)

(defun backward-kill-word-or-kill-region (arg)
  "C-w delete previous word or region"
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning)
		   (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word-or-kill-region)
(add-hook 'ido-setup-hook
	  (lambda ()
	    (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)))

(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;; list of buffers on C-x C-b

;; Bookmarks settings
(require 'bookmark)
(setq bookmark-save-flag t)                          ;; auto-save bookmarks in file
(when (file-exists-p (concat user-emacs-directory "bookmarks"))
  (bookmark-load bookmark-default-file t))
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
(global-set-key (kbd "<f5>") 'bookmark-jump)         ;; jump on bookmark on F5
(global-set-key (kbd "<C-f5>") 'bookmark-set)        ;; create bookmark
(global-set-key (kbd "<M-f5>") 'bookmark-bmenu-list) ;; open list of bookmarks

(defun comment-or-uncomment-this (&optional lines)
  "Comment current line or selected region"
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
	  (comment-or-uncomment-region (mark) (point))
	(comment-or-uncomment-region (poin) (mark)))
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-end-position lines))
    (next-line)))
(global-set-key (kbd "C-;") 'comment-or-uncomment-this)

(defun kill-current-buffer ()
  "C-x C-k current killing buffer"
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

(defun create-new-line-with-indent ()
  "Create new line with indent from middle line."
  (interactive)
  (end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "<C-return>") 'create-new-line-with-indent)

;; Delete trailing whitespaces, format buffer and untabify when save buffer
(defun format-current-buffer()
  (indent-region (point-min) (point-max))
  nil)
(defun untabify-current-buffer()
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max)))
  nil)
(add-to-list 'write-file-functions 'format-current-buffer)
(add-to-list 'write-file-functions 'untabify-current-buffer)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(defun move-line-down ()
  "Move current line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))
(global-set-key (kbd "<M-down>") 'move-line-down)

(defun move-line-up ()
  "Move current line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (previous-line 2)
    (move-to-column col)))
(global-set-key (kbd "<M-up>") 'move-line-up)

(use-package shell-pop
  :ensure nil
  :init
  (setq shell-pop-full-span t)
  :bind
  (("C-c s" . shell-pop)))

(use-package helm-mt
  :ensure nil
  :bind
  (("C-c S" . helm-mt)))

(use-package magit
  :ensure nil
  :bind
  (("C-c m" . magit-status)))

;;
;;
(require 'init-python)
(require 'init-cl)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(package-selected-packages
   (quote
    (material-theme highlight-sexp slime paren-face paredit magit helm-mt shell-pop helm-themes leuven-theme nimbus-theme powerline which-key use-package treemacs solarized-theme smartparens multiple-cursors helm-swoop helm-spaces helm-ls-git helm-git-grep helm-descbinds helm-c-yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
