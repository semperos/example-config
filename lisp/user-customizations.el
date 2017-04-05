(message "Enabling customizations")

(require 'cl)

;; Packages
(quelpa 'browse-kill-ring :stable t)
(quelpa 'ag :stable t)
(quelpa 'lua-mode :stable t)
(quelpa 'magit :stable t)
(quelpa 'markdown-mode :stable t)
(quelpa 'yaml-mode :stable t)
(quelpa 'ponylang-mode :stable t)

;;;;;;;;;;;;;;;;
;; Essentials ;;
;;;;;;;;;;;;;;;;

;; Give me the power
(setq disabled-command-function nil)

;; Buffers, menus, etc
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)

;; Kill ring
(require 'browse-kill-ring)
(setq kill-ring-max 5000)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

(defun paste-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun copy-for-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'copy-for-osx)
(setq interprogram-paste-function 'paste-from-osx)

;; Files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq
   backup-by-copying t           ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.file_backups")) ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)            ; use versioned backups

;;;;;;;;;;;;;;;;;;;;;;
;; Terminal Madness ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Tmux / xterm
;; From Scott McLeod's config
(defadvice terminal-init-screen
      ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
      (before tmux activate)
    ;; Docstring.  This describes the advice and is made available inside emacs;
    ;; for example when doing C-h f terminal-init-screen RET
    "Apply xterm keymap, allowing use of keys passed through tmux."
    ;; This is the elisp code that is run before `terminal-init-screen'.
    (if (getenv "TMUX")
        (let ((map (copy-keymap xterm-function-map)))
          (set-keymap-parent map (keymap-parent input-decode-map))
          (set-keymap-parent input-decode-map map))))
;; (define-key key-translation-map "\e[39;6" (kbd "C-'"))

;; From http://emacs.stackexchange.com/questions/1020/problems-with-keybindings-when-using-terminal
;; xterm with the resource ?.VT100.modifyOtherKeys: 1
;; GNU Emacs >=24.4 sets xterm in this mode and define
;; some of the escape sequences but not all of them.
(defun semperos/character-apply-modifiers (c &rest modifiers)
  "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
  (if (memq 'control modifiers) (setq c (if (or (and (<= ?@ c) (<= c ?_))
                                                (and (<= ?a c) (<= c ?z)))
                                            (logand c ?\x1f)
                                          (logior (lsh 1 26) c))))
  (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
  (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
  (vector c))

(defun semperos/eval-after-load-xterm ()
  (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
    (let ((c 32))
      (while (<= c 126)
        (mapc (lambda (x)
                (define-key xterm-function-map (format (car x) c)
                  (apply 'semperos/character-apply-modifiers c (cdr x))))
              '(;; with ?.VT100.formatOtherKeys: 0
                ("\e\[27;3;%d~" meta)
                ("\e\[27;5;%d~" control)
                ("\e\[27;6;%d~" control shift)
                ("\e\[27;7;%d~" control meta)
                ("\e\[27;8;%d~" control meta shift)
                ;; with ?.VT100.formatOtherKeys: 1
                ("\e\[%d;3~" meta)
                ("\e\[%d;5~" control)
                ("\e\[%d;6~" control shift)
                ("\e\[%d;7~" control meta)
                ("\e\[%d;8~" control meta shift)))
        (setq c (1+ c))))))
(eval-after-load "xterm" '(semperos/eval-after-load-xterm))

;; Helper functions
(defun semperos/wc (&optional start end)
  "Prints number of lines, words and characters in region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (message "Lines: %3d, Words: %3d, Chars: %3d" (count-lines start end) n (- end start))))

(defun semperos/switch-window-or-buffer ()
  (interactive)
  (if (equal current-prefix-arg '(4))
      (switch-to-buffer (other-buffer))
    (other-window 1)))

(defun semperos/switch-to-terminal (&optional arg)
  (interactive "p")
  (switch-to-buffer (get-buffer-create "*eshell*"))
  (when (= arg 4)
    (split-window-below)
    (switch-to-buffer (other-buffer))
    (other-window 1)))

(defun semperos/kill-matching-buffers
    (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. The optional second argument indicates whether to kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (let* ((buffers-to-kill (remove-if-not (lambda (buf)
                                           (let ((name (buffer-name buf)))
                                             (and name
                                                  (not (string-equal name ""))
                                                  (or internal-too (/= (aref name 0) ?\s))
                                                  (string-match regexp name))))
                                         (buffer-list))))
    (if (zerop (length buffers-to-kill))
        (message "No buffers match that regular expression.")
      (let* ((buffer-names (map 'list (lambda (buf) (concat " * " (buffer-name buf))) buffers-to-kill))
             (prompt (concat (mapconcat 'identity
                                        (cons "Matching Buffers:\n================" buffer-names)
                                        "\n")
                             "\n\nAre these the buffers to kill? "))
             (continue? (y-or-n-p (concat prompt "\n"))))
        (when continue?
          (dolist (buffer buffers-to-kill)
            (progn
              (message (concat "Killing buffer " (buffer-name buffer)))
              (kill-buffer buffer)))
          (message "Killed all matching buffers."))))))

  (defun semperos/kill-clojure-buffers ()
    (interactive)
    (semperos/kill-matching-buffers "\\.clj"))

;; Key Bindings
(global-set-key (kbd "C-o") 'ido-find-file)
(global-set-key (kbd "C-q") 'semperos/switch-window-or-buffer)
(global-set-key (kbd "C-c t") 'semperos/switch-to-terminal)
(add-hook 'paredit-mode-hook
            (lambda ()
              (local-set-key (kbd "C-M-s") 'paredit-forward-slurp-sexp)))

;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; enable fuzzy matching

;; magit
(require 'magit)
(global-set-key (kbd "C-x C-g") 'magit-status)

;;;;;;;;;;;;;;;;
;; Entrypoint ;;
;;;;;;;;;;;;;;;;

(eshell)
(ielm)
