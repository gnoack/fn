
(require 'paredit)
(add-hook 'lisp-mode-hook #'paredit-mode)

;; FN support.
(add-to-list 'auto-mode-alist '("\\.fn\\'" . lisp-mode))
(dolist (symbol (list 'with-asserts 'defm 'def 'with 'flet 'when 'dispatcher
                      'peg-let 'with-gensyms 'import 'match))
  (put symbol 'lisp-indent-function 1))

(font-lock-add-keywords 'lisp-mode
  '(("\\<case\\>"       . font-lock-keyword-face)
    ("\\<def\\>"        . font-lock-keyword-face)
    ("\\<defm\\>"       . font-lock-keyword-face)
    ("\\<defmacro\\>"   . font-lock-keyword-face)
    ("\\<defn\\>"       . font-lock-keyword-face)
    ("\\<deftype\\>"    . font-lock-keyword-face)
    ("\\<dispatcher\\>" . font-lock-keyword-face)
    ("\\<import\\>"     . font-lock-keyword-face)
    ("\\<labels\\>"     . font-lock-keyword-face)
    ("\\<match\\>"      . font-lock-keyword-face)
    ("\\<peg-let\\>"    . font-lock-keyword-face)
    ("\\<when\\>"       . font-lock-keyword-face)))

;; TODO: Copied from a messy .emacs -- does that even work?
(setq inferior-lisp-program "/home/me/proj/fn/fn")