;; manual dependencies
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/" "flycheck-20150207.329"))

(quelpa 'let-alist :stable t)
(quelpa 'flycheck-clojure :stable t)

(require 'flycheck)

(eval-after-load 'flycheck '(flycheck-clojure-setup))

(add-hook 'after-init-hook #'global-flycheck-mode)
