;;
;; Lisp and Emacs lisp modes
;;

;; in emacs 25.1: M-. runs xref-find-definitions M-, jumps back

(global-set-key (kbd "C-c e l") #'find-library)

(use-package slime
  :ensure nil
  :defer)

(setq slime-lisp-implementations '((sbcl ("sbcl")))
      slime-default-lisp 'sbcl
      slime-contribs '(slime-fancy))

(let ((path (expand-file-name "/home/novg/Documents/doc/HyperSpec/")))
  (if (file-accessible-directory-p path)
      (setq common-lisp-hyperspec-root (concat "file://" path))
    (warn "No HyperSpec directory found.")))

(use-package paren-face
  :ensure nil
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

(defun my-lisp-mode-hook-fn ()
  (set (make-local-variable 'lisp-indent-function) #'common-lisp-indent-function)
  (smartparens-mode t)
  (local-set-key (kbd "C-c S") (global-key-binding (kbd "M-s")))
  (show-paren-mode t)
  (setq show-paren-style 'expression)
  (paren-face-mode))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook-fn)
(add-hook 'lisp-mode-hook #'my-lisp-mode-hook-fn)

(defun slime-qlot-exec (directory)
  "from https://github.com/fukamachi/qlot/blob/master/README.markdown"
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "/home/novg/.roswell/bin/qlot"
               :program-args '("exec" "ros" "-S" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH="
                                  (mapconcat #'identity exec-path ":"))
                          (concat "QUICKLISP_HOME="
                                  (file-name-as-directory directory) "quicklisp/"))))
;;
;; OLD SETTINGS
;;
;;
;; Load quicklisp slime-helper
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Set up SBCL
;; (setq inferior-lisp-program "sbcl")
;;
;;
;; (let ((path (expand-file-name "/home/novg/Documents/doc/HyperSpec/")))
;; (if (file-accessible-directory-p path)
;; (setq common-lisp-hyperspec-root (concat "file://" path))
;; (warn "No HyperSpec directory found")))
;;
;;
;;

(provide 'init-cl)
