;;; extensions.el --- tim Layer extensions File for Spacemacs
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

(defvar tim-pre-extensions
  '(
    )
  "List of all extensions to load before the packages.")

(defvar tim-post-extensions
  '(
    better-ruby-identifiers
    two-space-indent
    new-empty-buffer
    fill-col-120
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function tim/init-<extension-tim>
;;
;; (defun tim/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun tim/init-better-ruby-identifiers ()
  (add-hook 'enh-ruby-mode-hook
    (lambda () (modify-syntax-entry ?_ "w" enh-ruby-mode-syntax-table)))
)

(defun tim/init-two-space-indent ()
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (setq-default
               tab-width 2
               indent-tabs-mode nil
               evil-shift-width 2))))

(defun tim/init-new-empty-buffer ()
  (defun new-empty-buffer ()
    "Open a new empty buffer."
    (interactive)
    (let ((buf (generate-new-buffer "untitled")))
      (switch-to-buffer buf)
      (funcall (and initial-major-mode))
      (setq buffer-offer-save t)))
  (evil-leader/set-key
    "bN" 'new-empty-buffer)
  )

(defun tim/init-fill-col-120 ()
  (set-fill-column 120)
  (add-hook 'enh-ruby-mode-hook
    (fci-mode 1)))
