;;; msf-consoles.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-consoles

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

(defvar msf/consoles-actions
  '(("Attach in EShell" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmds '()))
           (add-to-list 'cmds (concat "msf-console " (msf>get-console-id-from-candidate candidate)))
           (msf>eshell-console cmds)))))
    ("Kill console" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmd (concat "msf-kill-console " (msf>get-console-id-from-candidate candidate))))
           (msf>shell-command cmd))))))
  "MSF Consoles actions.")

(defvar msf/c-source-consoles
  (helm-build-in-buffer-source "MSF Consoles"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message "[*] Loading active consoles..")
              (insert (shell-command-to-string "msf-get-consoles -l"))))
    :action msf/consoles-actions)
  "MSF Consoles helm source definition.")

(defun msf>get-console-id-from-candidate (candidate)
  "Get Console ID from CANDIDATE."
  (substring candidate 0 (string-match " - " candidate)))

(defun msfrpc-create-new-console ()
  "Create new console."
  (interactive)
  (msf>eshell-console '("msf-console")))

;;;###autoload
(defun helm-msf-consoles ()
  "MSF Active Consoles."
  (interactive)
  (helm :sources '(msf/c-source-consoles)
        :candidate-number-limit 9999
        :buffer "*msf-consoles*"
        :prompt "MSF Active consoles> "
        :full-frame nil))

(provide 'msf-consoles)

;;; msf-consoles.el ends here
