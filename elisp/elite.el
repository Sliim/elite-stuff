;;; elite.el --- Run all elite stuff.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run all elite stuff!

;; Usage:
;;
;; Install deps with Cask:
;;     $ cask install
;;
;; Load elite.el:
;;     $ emacs -Q -nw -l elite.el

;;; Code:
(setq msf-user-dir ".msf4"
      exploitdb-path "/usr/share/exploitdb/"
      exploitdb-workspace-path (expand-file-name (concat (getenv "HOME") "/codz/pentest/stuff/exploits/")))

(add-to-list 'load-path (file-name-directory load-file-name))
(let ((default-directory (expand-file-name (concat (file-name-directory load-file-name) ".cask/"
                                                   (number-to-string emacs-major-version) "."
                                                   (number-to-string emacs-minor-version) "/elpa"))))
  (normal-top-level-add-subdirs-to-load-path))

(require 'org-msf)
(require 'elite-mode)
(require 'elite-hash)

(setq gdb-many-windows t)

(setq alert-default-style 'libnotify
      alert-log-messages t
      alert-default-icon "kali-cewl")

(push '("*Shell Command Output*") popwin:special-display-config)
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
(setq async-shell-command-buffer 'new-buffer)
;;(push '("*Async Shell Command*"  :stick t ) popwin:special-display-config)
;;(push '("*EShell Command Output*") popwin:special-display-config)

(global-set-key (kbd "C-c e m") 'elite-mode)
(global-set-key (kbd "C-c e a") 'ascii-on)
(global-set-key (kbd "C-c e b e") 'base64-encode-region)
(global-set-key (kbd "C-c e b d") 'base64-decode-region)

(global-set-key (kbd "C-c m f") 'helm-msf-files)
(global-set-key (kbd "C-c m h") 'helm-msf-hosts)
(global-set-key (kbd "C-c m s") 'helm-msf-services)
(global-set-key (kbd "C-c m S") 'helm-msf-sessions)
(global-set-key (kbd "C-c m r") 'helm-msf-routes)
(global-set-key (kbd "C-c m c") 'helm-msf-consoles)
(global-set-key (kbd "C-c m j") 'helm-msf-jobs)
(global-set-key (kbd "C-c m t") 'helm-msf-threads)
(global-set-key (kbd "C-c m w") 'helm-msf-workspaces)
(global-set-key (kbd "C-c m n") 'helm-msf-notes)
(global-set-key (kbd "C-c m p") 'helm-msf-creds)
(global-set-key (kbd "C-c m v") 'helm-msf-vulns)
(global-set-key (kbd "C-c m l") 'helm-msf-loots)
(global-set-key (kbd "C-c m m") 'helm-msf-modules)

(global-set-key (kbd "C-c s") 'helm-searchsploit)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x s") 'elite-save-buffers)

(msf>eshell-console '("msf-console"))
(elite-mode)
(elite-teamserver-infos)
(elite-update-mode-line)
