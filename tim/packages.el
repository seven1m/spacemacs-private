;;; packages.el --- tim Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar tim-packages
  '(
    editorconfig
    elixir-mode
    haml-mode
    php-mode
    ruby-refactor
    rust-mode
    slim-mode
    web-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar tim-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function tim/init-<package-tim>
;;
;; (defun tim/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun tim/init-editorconfig ())
(defun tim/init-elixir-mode ())
(defun tim/init-haml-mode ())
;; (defun tim/init-jsx-mode ()
;;   (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;;   (autoload 'jsx-mode "jsx-mode" "JSX mode" t))
(defun tim/init-php-mode ())
(defun tim/init-ruby-refactor ()
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
  (setq-default ruby-refactor-add-parens t)
  (evil-leader/set-key
    "orc" 'ruby-refactor-extract-constant
    "orv" 'ruby-refactor-extract-local-variable
    "orm" 'ruby-refactor-extract-to-method
    "orp" 'ruby-refactor-add-parens))
(defun tim/init-rust-mode ())
(defun tim/init-slim-mode ())
(defun tim/init-yafolding ()
  (use-package yafolding))
;; (defun tim/init-web-mode ()
;;   (require 'web-mode)
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))

