;;EVIL -- VIM-MODE
;; (add-to-list 'load-path "~/.emacs.d/elpa/evil-1.0.8")
(require 'evil)
(evil-mode 1)
;; (add-to-list 'load-path "~/.emacs.d/elpa/evil-numbers-0.3")
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

(provide 'init-evil)
