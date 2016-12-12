;;; msf-creds.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-creds

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

(defvar msf/creds-actions
  '(("Copy password" .
     (lambda (candidate)
       (kill-new (msf>get-cred-from-credential-candidate candidate)))))
  "MSF Creds actions.")

(defvar msf/c-source-creds
  (helm-build-in-buffer-source "MSF Creds"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (let ((cmd "msf-get-creds -l"))
                (when msf/current-rhost
                  (setf cmd (concat cmd " " msf/current-rhost)))
                (message "[*] Loading creds..")
                (insert (shell-command-to-string cmd)))))
    :action msf/creds-actions)
  "MSF Creds helm source definition.")

(defun msf>get-cred-from-credential-candidate (candidate)
  "Get cred from credential CANDIDATE."
  (car (last (split-string candidate " "))))

;;;###autoload
(defun helm-msf-creds ()
  "MSF Active Creds."
  (interactive)
  (helm :sources '(msf/c-source-creds)
        :candidate-number-limit 9999
        :buffer "*msf-creds*"
        :prompt "MSF creds> "
        :full-frame nil))

(provide 'msf-creds)

;;; msf-creds.el ends here
