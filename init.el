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
;; (when (window-system)
;;    (set-frame-size (selected-frame) 122 34))
;; Set top left corner of frame
;; (set-frame-position (selected-frame) 60 30)

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; disable gui menu
(tool-bar-mode     -1) ;; disable tool-bar
(scroll-bar-mode   -1) ;; disable scroll-bar
(blink-cursor-mode -1) ;; disable blink cursor
(setq use-dialog-box nil) ;; disable gui dialog window
(setq redisplay-dont-pause t) ;; advanced displaing of buffer
(setq ring-bell-function 'ignore) ;; disable bell
(global-set-key (kbd "<f12>") 'menu-bar-mode) ;; show menu

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
(display-time)
(setq calendar-date-display-form (quote ((format "%04s-%02d-%02d" year (string-to-int month) (string-to-int day)))))
(setq calendar-time-display-form (quote (24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))))
(setq calendar-week-start-day t)
(setq european-calendar-style t)

;; Linum plugin
(require 'linum)
(line-number-mode   t) ;; show number of string in mode-line
(global-linum-mode  t) ;; show nubers of strings at all buffers
(column-number-mode t) ;; show number of column at mode-line
(setq linum-format "  %d") ;; format of number strings

;; Fringe setting
(fringe-mode '(8 . 0)) ;;
(setq-default insicate-empty-lines t)
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
(setq imeny-use-popup-menu nil)
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
;;
;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
; (require 'init-haskell)
