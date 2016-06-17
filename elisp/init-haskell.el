(require 'haskell-mode)


(let ((my-stack-path (expand-file-name "~/local/bin")))
  (setenv "PATH" (concat my-stack-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-stack-path))

;; (add-hook 'haskell-mode-hook #'hindent-mode)

;; (setq haskell-tags-on-save t)

;; (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)

;; ghc-mod
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(global-set-key (kbd "<f8>") 'ghc-toggle-check-command)

;; company-ghc
(require 'company)
(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)
(setq company-ghc-show-info t)

(provide 'init-haskell)
