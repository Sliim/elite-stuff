;;; msfconso.el --- The msf.el run script.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run script for msf.el utility

;; Usage:
;;
;; Install deps with Cask:
;;     $ cask install
;;
;; Load msfconso.el:
;;     $ emacs -Q -nw -l msfconso.el
;;
;; It will automatically load the cache and open helm interface.
;; You can reopen interface with C-c m.

;;; Code:

(setq msf-user-dir ".msf4")

(add-to-list 'load-path (file-name-directory load-file-name))
(let ((default-directory (expand-file-name (concat (file-name-directory load-file-name) ".cask/"
                                                   (number-to-string emacs-major-version) "."
                                                   (number-to-string emacs-minor-version) "/elpa"))))
  (normal-top-level-add-subdirs-to-load-path))

(require 'msf)

(global-set-key (kbd "C-c m") 'helm-msf-files)
(helm-msf)
