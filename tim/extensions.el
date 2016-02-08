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
    better-identifiers
    disable-smartparens
    two-space-indent
    new-empty-buffer
    save-buffer-always
    goto-file
    x-skips-clipboard
    ruby-folding
    ruby-test
    term-keys
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

(defun tim/init-better-identifiers ()
  (add-hook 'enh-ruby-mode-hook
    (lambda () (modify-syntax-entry ?_ "w" enh-ruby-mode-syntax-table)))
  (add-hook 'elixir-mode-hook
    (lambda () (modify-syntax-entry ?_ "w" elixir-mode-syntax-table)))
)

(defun tim/init-two-space-indent ()
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (setq-default
               tab-width 2
               indent-tabs-mode nil
               evil-shift-width 2)))
  (add-hook 'web-mode-hook
            (lambda ()
              (setq-default
               tab-width 2
               indent-tabs-mode nil
               evil-shift-width 2
               js-indent-level 2)))
  (add-hook 'elixir-mode-hook
            (lambda ()
              (setq-default
               tab-width 2
               indent-tabs-mode nil
               evil-shift-width 2
               js-indent-level 2))))

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

(defun tim/init-save-buffer-always ()
  (defun save-buffer-always ()
    "Save the buffer even if it is not modified."
    (interactive)
    (set-buffer-modified-p t)
    (save-buffer))
  (global-set-key (kbd "C-s") 'save-buffer-always)
  )

(defun tim/init-disable-smartparens ()
  (add-hook 'evil-insert-state-entry-hook 'turn-off-smartparens-mode)
  (add-hook 'evil-insert-state-exit-hook 'turn-on-smartparens-mode)
  )

(defun tim/init-goto-file ()
  (define-key evil-normal-state-map "gf" 'projectile-rails-goto-file-at-point)
  )

(defun tim/init-x-skips-clipboard ()
  (evil-define-operator evil-delete-char-into-black-hole-register (beg end type register)
    "Delete next character."
    :motion evil-forward-char
    (interactive "<R><x>")
    (let ((black-hole-register 95))
      (evil-delete beg end type black-hole-register)))
  (define-key evil-normal-state-map "x" 'evil-delete-char-into-black-hole-register))

; only works with ruby-mode right now (not enh-ruby-mode) -- don't know why
(defun tim/init-ruby-folding ()
  (add-hook 'ruby-mode-hook
            (lambda () (hs-minor-mode)))

  (eval-after-load "hideshow"
    '(add-to-list 'hs-special-modes-alist
                  `(ruby-mode
                    ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
                    ,(rx (or "}" "]" "end"))                       ; Block end
                    ,(rx (or "#" "=begin"))                        ; Comment start
                    ruby-forward-sexp nil)))

  (evil-leader/set-key
    "oh" 'hs-hide-block)
  (evil-leader/set-key
    "os" 'hs-show-block))

(defun tim/init-ruby-test ()
  (add-hook 'ruby-test-mode-hook
    (lambda ()
      (defun ruby-test-spec-command (filename &optional line-number)
        (let (command options)
          (setq command "bundle exec spring rspec")
          (setq options ruby-test-rspec-options)
          (if line-number
              (setq filename (format "%s:%s" filename line)))
          (format "%s %s %s" command (mapconcat 'identity options " ") filename)))
    (evil-leader/set-key
      "otf" 'ruby-test-run)
    (evil-leader/set-key
      "otm" 'ruby-test-run-at-point)))
  (add-hook 'enh-ruby-mode-hook
    (lambda ()
      (defun alternate-file-in-horizontal-split ()
        "Open the alternate file in a horizontal split window."
        (interactive)
        (split-window-below-and-focus)
        (ruby-test-toggle-implementation-and-specification))
      (defun alternate-file-in-vertical-split ()
        "Open the alternate file in a vertical split window."
        (interactive)
        (split-window-right-and-focus)
        (ruby-test-toggle-implementation-and-specification))
      (evil-leader/set-key
        "oas" 'alternate-file-in-horizontal-split)
      (evil-leader/set-key
        "oav" 'alternate-file-in-vertical-split)))
  )

(defun tim/init-term-keys ()
  (evil-leader/set-key
    "ott" 'ansi-term)
  (evil-leader/set-key
    "otq" 'term-quit-subjob)
  )
