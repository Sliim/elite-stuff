;;; msf-hosts.el --- The Metasploit Framework Emacs helm interfaces.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Package-Requires: ((helm "1.6.3") (eshell "2.4.2"))
;; Keywords: msf metasploit emacs helm eshell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs helm interfaces for The Metasploit Framework.
;; Provides search capability for metasploit modules, tools,
;; plugins and scripts.
;;
;; Currently, Metasploit's modules, plugins, tools and scripts are
;; loaded from the metasploit-framework sources.  Others sources will
;; be added to expand the helm candidates such as custom modules and
;; msfrpc interaction.

;; Usage:
;;
;; Copy this file in your loadpath and:
;;
;;     M-x helm-hosts

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'msf)
(require 'msf-notes)

(defvar msf/hosts-actions
  '(("Run Nmap scan" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmds '("msf-console"))
               (opts (read-string "Nmap options>> ")))
           (add-to-list 'cmds (concat "db_nmap " opts " " (msf>get-host-from-candidate candidate)) t)
           (msf>eshell-console cmds)))))
    ("Run Nmap scan in new Tmux window" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((opts (read-string "Nmap options>> ")))
           (msf>tmux-run-and-wait (concat "nmap " opts " " (msf>get-host-from-candidate candidate))
                                  (concat "nmap-" (msf>get-host-from-candidate candidate)))))
       (helm-msf-hosts)))
    ("Search vulns" .
     (lambda (candidate)
       (let ((msf/current-rhost (msf>get-host-from-candidate candidate)))
         (helm-msf-vulns))))
    ("Search notes" .
     (lambda (candidate)
       (let ((msf/current-rhost (msf>get-host-from-candidate candidate)))
         (helm-msf-notes))))
    ("Search loots" .
     (lambda (candidate)
       (let ((msf/current-rhost (msf>get-host-from-candidate candidate)))
         (helm-msf-loots))))
    ("Generate report" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((bufname (concat "Report-Host-" (msf>get-host-from-candidate candidate) ".org"))
               (host (msf>get-host-from-candidate candidate)))
           (message (concat "Generating report for " host ".."))
           (get-buffer-create bufname)
           (switch-to-buffer-other-window bufname)
           (with-current-buffer bufname
             (org-mode)
             (insert (shell-command-to-string (concat "msf-org-report -c " host))))))))
    ("Set as RHOST" .
     (lambda (candidate)
       (msf>set-opt "RHOST" (msf>get-host-from-candidate candidate))))
    ("Set as RHOSTS" .
     (lambda (candidate)
       (msf>set-opt "RHOSTS" (msf>get-host-from-candidate candidate))))
    ("Set as RHOST & RHOSTS" .
     (lambda (candidate)
       (msf>set-opt "RHOST" (msf>get-host-from-candidate candidate))
       (msf>set-opt "RHOSTS" (msf>get-host-from-candidate candidate))))
    ("Remove host" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((host (msf>get-host-from-candidate candidate)))
           (when (y-or-n-p (concat "Delete host " host "?"))
             (msf>shell-command (concat "msf-rm-host -c " host))))))))
  "MSF Hosts actions.")

(defvar msf/c-source-hosts
  (helm-build-in-buffer-source "MSF Hosts"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message "[*] Loading active hosts..")
              (insert (shell-command-to-string "msf-get-hosts -c -l"))))
    :action msf/hosts-actions)
  "MSF Hosts helm source definition.")

(defun msf>get-host-from-candidate (candidate)
  "Get Host address from CANDIDATE."
  (substring candidate 0 (string-match " - " candidate)))

;;;###autoload
(defun helm-msf-hosts ()
  "MSF Active Hosts."
  (interactive)
  (helm :sources '(msf/c-source-hosts)
        :candidate-number-limit 9999
        :buffer "*msf-hosts*"
        :prompt "MSF Active hosts> "
        :full-frame nil))

(provide 'msf-hosts)

;;; msf-hosts.el ends here
