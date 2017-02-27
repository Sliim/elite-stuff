;;; msf-jobs.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-jobs

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

(defvar msf/jobs-actions
  '(("Nothing" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (message (concat candidate " - ID: " (msf>get-job-id-from-candidate candidate))))))
    ("Generate payload (handlers only)" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((payload (msf>get-job-payload-from-candidate candidate))
               (lhost (msf>get-job-lhost-from-candidate candidate))
               (lport (msf>get-job-lport-from-candidate candidate)))
           (let ((port (read-string (concat "LPORT [" lport "]: ")))
                 (host (read-string (concat "LHOST [" lhost "]: "))))
             (when (not (string= "" port))
               (setf lport port))
             (when (not (string= "" host))
               (setf lhost host)))
           (let ((cmds '("msf-console")))
             (add-to-list 'cmds (concat "use " payload) t)
             (add-to-list 'cmds "generate -h" t)
             (msf>eshell-console cmds "Payload-Generate-help"))
           (let ((cmds '("msf-console")))
             (add-to-list 'cmds (concat "use " payload) t)
             (add-to-list 'cmds (concat "set LHOST " lhost) t)
             (add-to-list 'cmds (concat "set LPORT " lport) t)
             (add-to-list 'cmds (concat "generate " (read-string "Options for payload generation>> ")) t)
             (msf>eshell-console cmds (concat "Payload-generate-" payload "-" lhost "-" lport)))))))
    ("Kill job" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmd (concat "msf-kill-job " (msf>get-job-id-from-candidate candidate))))
           (msf>shell-command cmd)))
       (helm-msf-jobs))))
  "MSF Jobs actions.")

(defvar msf/c-source-jobs
  (helm-build-in-buffer-source "MSF Jobs"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (alert "Loading active jobs.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
              (insert (shell-command-to-string "msf-get-jobs -l"))))
    :action msf/jobs-actions)
  "MSF Jobs helm source definition.")

(defun msf>get-job-id-from-candidate (candidate)
  "Get Job ID from CANDIDATE."
  (substring candidate 0 (string-match " - " candidate)))

(defun msf>get-job-payload-from-candidate (candidate)
  "Get Job payload from CANDIDATE."
  (car (split-string (car (last (split-string candidate " - "))) " ")))

(defun msf>get-job-lhost-from-candidate (candidate)
  "Get Job lhost from CANDIDATE."
  (car (split-string (car (last (split-string (car (last (split-string candidate " - "))) " "))) ":")))

(defun msf>get-job-lport-from-candidate (candidate)
  "Get Job lport from CANDIDATE."
  (car (last (split-string (car (last (split-string (car (last (split-string candidate " - "))) " "))) ":"))))


;;;###autoload
(defun helm-msf-jobs ()
  "MSF Active Jobs."
  (interactive)
  (helm :sources '(msf/c-source-jobs)
        :candidate-number-limit 9999
        :buffer "*msf-jobs*"
        :prompt "MSF Active jobs> "
        :full-frame nil))

(provide 'msf-jobs)

;;; msf-jobs.el ends here
