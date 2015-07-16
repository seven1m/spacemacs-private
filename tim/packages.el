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
    haml-mode
    jsx-mode
    php-mode
    rust-mode
    slim-mode
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
(defun tim/init-haml-mode ())
(defun tim/init-jsx-mode ()
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
  (autoload 'jsx-mode "jsx-mode" "JSX mode" t))
(defun tim/init-php-mode ())
(defun tim/init-rust-mode ())
(defun tim/init-slim-mode ())
