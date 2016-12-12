;;; msf-sessions.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-sessions

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

(defvar msf/sessions-actions
  '(("Console/Shell" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmds '()))
           (add-to-list 'cmds (concat "msf-shell " (msf>get-session-id-from-candidate candidate)))
           (msf>eshell-console cmds)))))
    ("Console/Meterpreter" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmds '()))
           (add-to-list 'cmds (concat "msf-meterpreter " (msf>get-session-id-from-candidate candidate)))
           (msf>eshell-console cmds)))))
    ("Upgrade sessions" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmd (concat "msf-session-upgrade " (msf>get-session-id-from-candidate candidate))))
           (msf>shell-command cmd)))))
    ("Find compatible post modules" .
     (lambda (candidate)
       (let ((msf/current-sessid (msf>get-session-id-from-candidate candidate)))
         (helm-msf-find-compatible-modules msf/current-sessid))))
    ("Add routes" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((sessid (msf>get-session-id-from-candidate candidate)))
           (let ((cmd (concat "msf-add-route "))
                 (dest (read-string (concat "Add route throught session #" sessid ". Destination: "))))
             (msf>shell-command (concat cmd dest " " sessid)))))))
    ("Search routes" .
     (lambda (_candidate)
       (helm-msf-routes)))
    ("Clean session" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmd (concat "msf-clean-session " (msf>get-session-id-from-candidate candidate))))
           (msf>shell-command cmd)))))
    ("Kill session" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmd (concat "msf-kill-session " (msf>get-session-id-from-candidate candidate))))
           (msf>shell-command cmd))))))
  "MSF Sessions actions.")

(defvar msf/c-source-sessions
  (helm-build-in-buffer-source "MSF Sessions"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message "[*] Loading active sessions..")
              (insert (shell-command-to-string "msf-get-sessions -l"))))
    :action msf/sessions-actions)
  "MSF Sessions helm source definition.")

(defvar msf/c-source-session-post-modules
  (helm-build-in-buffer-source "MSF Post"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message (concat "[*] Searching post modules for session #" msf/current-sessid ".."))
              (insert (shell-command-to-string (concat "msf-session-get-compatible-modules -l " msf/current-sessid)))))
    :action msf/post-module-actions)
  "MSF session post modules helm source definition.")

(defun msf>get-session-id-from-candidate (candidate)
  "Get Session ID from CANDIDATE."
  (substring candidate 0 (string-match " - " candidate)))

(defun helm-msf-find-compatible-modules (sessid)
  "Find compatibles post modules for this SESSID."
  (helm :sources '(msf/c-source-session-post-modules)
        :candidate-number-limit 999
        :buffer (concat "*msf-session-" sessid "-post*")
        :prompt "MSF post> "
        :full-frame nil))

;;;###autoload
(defun helm-msf-sessions ()
  "MSF Active Sessions."
  (interactive)
  (helm :sources '(msf/c-source-sessions)
        :candidate-number-limit 9999
        :buffer "*msf-sessions*"
        :prompt "MSF Active sessions> "
        :full-frame nil))

(provide 'msf-sessions)

;;; msf-sessions.el ends here
