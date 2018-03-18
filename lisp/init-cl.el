;;
;; Lisp and Emacs lisp modes
;;

;; in emacs 25.1: M-. runs xref-find-definitions M-, jumps back

(global-set-key (kbd "C-c e l") #'find-library)

;; (use-package slime
;;   :ensure nil
;;   :init
;;   (load (expand-file-name "~/.roswell/helper.el"))
;;   (setq inferior-lisp-program "ros -Q run"
;; 	slime-contribs '(slime-fancy))

;;   (setf slime-lisp-implementations
;; 	`((sbcl    ("sbcl" "--dynamic-space-size" "2000"))
;; 	  (roswell ("ros" "-Q" "run"))))
;;   (setf slime-default-lisp 'roswell))

;; (let ((path (expand-file-name "/home/novg/Documents/doc/HyperSpec/")))
;;   (if (file-accessible-directory-p path)
;;       (setq common-lisp-hyperspec-root (concat "file://" path))
;;     (warn "No HyperSpec directory found.")))

(use-package paren-face
  :ensure t
  :defer)

(defun my-emacs-lisp-mode-hook-fn ()
  (set (make-local-variable 'lisp-indent-function) #'lisp-indent-function)
  (smartparens-mode t)
  (local-set-key (kbd "C-c S") (global-key-binding (kbd "M-s")))
  (local-set-key (kbd "C-c C-z")
		 (lambda () (interactive) (switch-to-buffer "*scratch*")))
  (show-paren-mode t)
  (setq show-paren-style 'expression)
  (paren-face-mode))

;; (defun my-lisp-mode-hook-fn ()
;;   (set (make-local-variable 'lisp-indent-function) #'common-lisp-indent-function)
;;   (smartparens-mode t)
;;   (local-set-key (kbd "C-c S") (global-key-binding (kbd "M-s")))
;;   (show-paren-mode t)
;;   (setq show-paren-style 'expression)
;;   (paren-face-mode))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook-fn)
;; (add-hook 'lisp-mode-hook #'my-lisp-mode-hook-fn)

;;

(provide 'init-cl)
