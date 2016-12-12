;;; searchsploit.el --- The exploitdb.el run script.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run script for exploitdb.el utility

;; Usage:
;;
;; Install deps with Cask:
;;     $ cask install
;;
;; Load searchsploit.el:
;;     $ emacs -Q -nw -l searchsploit.el
;;
;; It will automatically load the cache and open helm interface.
;; You can reopen interface with C-c m.

;;; Code:

(setq exploitdb-path "/usr/share/exploitdb/"
      exploitdb-workspace-path (expand-file-name (concat (getenv "HOME") "/codz/pentest/stuff/exploits/")))

(add-to-list 'load-path (file-name-directory load-file-name))
(let ((default-directory (expand-file-name (concat (file-name-directory load-file-name) ".cask/"
                                                   (number-to-string emacs-major-version) "."
                                                   (number-to-string emacs-minor-version) "/elpa"))))
  (normal-top-level-add-subdirs-to-load-path))

(require 'exploitdb)

(global-set-key (kbd "C-c s") 'helm-searchsploit)
(helm-searchsploit)
