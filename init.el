;; M-x eval-buffer
;; M-x package-refresh-contents
;; M-x package-install RET package-name RET

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Set font
;; (set-face-attribute 'default nil :font "Anonimous Pro Minus-13")

;; Start window size
(when (window-system)
   (set-frame-size (selected-frame) 150 50))
;; Set top left corner of frame
(set-frame-position (selected-frame) 80 50)

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; disable gui menu
(tool-bar-mode     -1) ;; disable tool-bar
(scroll-bar-mode   -1) ;; disable scroll-bar
(blink-cursor-mode -1) ;; disable blink cursor
(setq use-dialog-box nil) ;; disable gui dialog window
(setq redisplay-dont-pause t) ;; advanced displaing of buffer
(setq ring-bell-function 'ignore) ;; disable bell
(global-set-key (kbd "<f12>") 'menu-bar-mode) ;; show menu on press F12

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t) ;; show greeting screen C-h C-a

;; Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;;
(delete-selection-mode t)

;; Size of file per percent (%)
(size-indication-mode t)

;; Hour format
(setq display-time-24hr-format t)
;; (setq display-time-day-and-date nil)
(display-time-mode t)
;; (setq calendar-date-display-form (quote ((format "%04s-%02d-%02d" year (string-to-int month) (string-to-int day)))))
;; (setq calendar-time-display-form (quote (24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))))
;; (setq calendar-week-start-day t)
;; (setq european-calendar-style t)

;; Linum plugin
(require 'linum)
(line-number-mode   t) ;; show number of string in mode-line
(global-linum-mode  t) ;; show nubers of strings at all buffers
(column-number-mode t) ;; show number of column at mode-line
(setq linum-format " %d") ;; format of number strings

;; Fringe setting
(fringe-mode '(8 . 0)) ;;  make the left fringe 8 pixels wide and the right disappear
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Syntax highlighting
(require 'font-lock)
(setq font-lock-maximum-decoration t)

;; Scrolling setting
(setq scroll-step               1)
(setq scroll-margin            10)
(setq scroll-conservatively 10000)

;; Show-paren-mode settings
(show-paren-mode t) ;; enable highlight expressions between {},[],()
(setq show-paren-style 'expression) ;; enable color higlight expression between {},[],()

;; Electric-modes settings
(electric-pair-mode    1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок
(electric-indent-mode -1) ;; отключить индентацию  electric-indent-mod'ом (default in Emacs-24.4)

;; Disable backup/autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)
;; (setq auto-save-list-file-name nil)

;; Indent setting
(setq-default indent-tabs-mode nil) ;; disable TAB indent
(setq-default tab-width          4) ;; widht of tabulation - 4 space symbols
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)
(global-set-key (kbd "RET") 'newline-and-indent)
(setq lisp-indent-function 'common-lisp-indent-function)

;; Show messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Clipboard settings
;; (setq x-select-enable-clipboard t)

;; End of file newlines
(setq require-final-newline    t)
(setq next-line-add-newlines nil)

;; Easy transition between buffers: M-arrow-keys
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;; IDO plugin
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-virtual-buffers      t)
(setq ido-enable-flex-matching t)

;; C-w dalete previous word or region
(defun backward-kill-word-or-kill-region (arg)
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

;; Buffer selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;; list of buffers on C-x C-b
(global-set-key (kbd "<f2>") 'bs-show) ;; buffer selection on F2

;; Bookmarks settings
(require 'bookmark)
(setq bookmark-save-flag t) ;; auto-save bookmarks in file
(when (file-exists-p (concat user-emacs-directory "bookmarks"))
    (bookmark-load bookmark-default-file t))
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
(global-set-key (kbd "<f5>") 'bookmark-jump) ;; jump on bookmark on F5
(global-set-key (kbd "<C-f5>") 'bookmark-set) ;; create bookmark
(global-set-key (kbd "<M-f5>") 'bookmark-bmenu-list) ;; open list of bookmarks

;; Dired
(require 'dired)
(setq dired-recursive-deletes 'top) ;; delete not empty directories

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan t)
(setq imenu-use-popup-menu nil)
(global-set-key (kbd "<f8>") 'imenu)

;; Run shell
(global-set-key (kbd "C-x /") 'shell-command)

;; Word-wrap mode
(global-set-key (kbd "<f6>") 'toggle-truncate-lines)

;; Comment function
(defun comment-or-uncomment-this (&optional lines)
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

;; C-x C-k killing buffer
(defun kill-current-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

;; Auto-complete
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "S-SPC") 'dabbrev-expand)

;; Highlight current line
(global-hl-line-mode t)

;;
(defun create-new-line-with-indent ()
    "Create new line with indent from middle line."
    (interactive)
    (end-of-line 1)
    (newline-and-indent))
(global-set-key (kbd "<C-return>") 'create-new-line-with-indent)
;;
;;
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
;; (require 'init-haskell)
