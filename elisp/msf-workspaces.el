;;; msf-workspaces.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-workspaces

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

(defvar msf/workspaces-actions
  '(("Set as current workspace" .
     (lambda (candidate)
       (let ((cmd (concat "msf-set-workspace "candidate)))
         (msf>shell-command cmd))))
    ("Generate report" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((bufname (concat "Report-Workspace-" (replace-regexp-in-string "*" "" candidate) ".org"))
               (workspace (replace-regexp-in-string "*" "" candidate)))
           (message (concat "Generating report for " workspace " workspace.."))
           (get-buffer-create bufname)
           (switch-to-buffer-other-window bufname)
           (with-current-buffer bufname
             (org-mode)
             (insert (shell-command-to-string (concat "msf-org-report -w " workspace))))))))
    ("Delete workspace" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (when (y-or-n-p (concat "Delete workspace " candidate "?"))
           (let ((cmd (concat "msf-rm-workspace "candidate)))
             (msf>shell-command cmd))))))
    ("Add new workspace" .
     (lambda (_candidate)
       (let ((new_ws (read-string "Workspace name: ")))
         (msf>shell-command (concat "msf-add-workspace " new_ws))))))
  "MSF Workspaces actions.")

(defvar msf/c-source-workspaces
  (helm-build-in-buffer-source "MSF Workspaces"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message "[*] Loading workspaces list..")
              (insert (shell-command-to-string "msf-get-workspaces -l"))))
    :action msf/workspaces-actions)
  "MSF Workspaces helm source definition.")

;;;###autoload
(defun helm-msf-workspaces ()
  "MSF Active Workspaces."
  (interactive)
  (helm :sources '(msf/c-source-workspaces)
        :candidate-number-limit 9999
        :buffer "*msf-workspaces*"
        :prompt "MSF Active workspaces> "
        :full-frame nil))

(provide 'msf-workspaces)

;;; msf-workspaces.el ends here
