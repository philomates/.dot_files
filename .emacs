(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;(custom-set-variables
;'(session-use-package t nil (session)))

(add-to-list 'load-path "~/.emacs.d/evil") ; only without ELPA/el-get
(require 'evil)
(evil-mode 1)
(add-to-list 'load-path "~/.emacs.d/key-chord")
(require 'key-chord)
(key-chord-mode 1)
; (key-chord-define evil-normal-state-map "jj" 'evil-force-normal-state)
(key-chord-define evil-visual-state-map "jj" 'evil-change-to-previous-state)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jj" 'evil-normal-state)

(load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
(load-file "~/.emacs.d/pg-ssr.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-include-dirs (quote ("/home/mates/agda/agda-stdlib/src/" ".")))
 '(session-use-package t nil (session)))
(setq coq-compile-before-require t)
(setq coq-load-path '("/home/mates/home_repo/programming/coq/metatheory"
                      "/home/mates/home_repo/programming/coq/ln/tlc"
                      "/home/mates/home_repo/programming/coq/cpdt/src"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 83 :width normal)))))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

