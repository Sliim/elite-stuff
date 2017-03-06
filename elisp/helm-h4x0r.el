;;; helm-h4x0r.el --- The Metasploit Framework Emacs helm interfaces.
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

(defvar helm-h4x0r/nmap-actions
  '(("Run Nmap in msf console" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (when (string-match "MSFConsole" candidate)
           (let ((cmds '("msf-console"))
                 (opts (read-string "Nmap options: "))
                 (target (read-string "Target: ")))
             (add-to-list 'cmds (concat "db_nmap " opts " " target) t)
             (msf>eshell-console cmds (concat "MSFConsole-Nmap-" target))))
         (when (string-match "Eshell" candidate)
           (let ((cmds '())
                 (opts (read-string "Nmap options: "))
                 (target (read-string "Target: ")))
             (add-to-list 'cmds (concat "nmap " opts " " target) t)
             (msf>eshell-console cmds (concat "Nmap-" target))))
         (when (string-match "AsyncShell" candidate)
           (let ((opts (read-string "Nmap options: "))
                 (target (read-string "Target: ")))
             (msf>async-process "nmap" (concat opts " " target))))
         (when (string-match "Tmux" candidate)
           (let ((opts (read-string "Nmap options: "))
                 (target (read-string "Target: ")))
             (msf>tmux-run-and-wait (concat "nmap " opts " " target))))))))
  "Nmap actions.")

(defvar helm-h4x0r/c-source-nmap
  (helm-build-in-buffer-source "H4x0r Nmap Commands"
    :init (lambda ()
            (let ((cmds '("[MSFConsole] Run nmap"
                          "[Eshell] Run nmap"
                          "[AsyncShell] Run nmap"
                          "[Tmux] Run nmap")))
              (with-current-buffer (helm-candidate-buffer 'local)
                (insert (mapconcat 'identity cmds "\n")))))
    :action helm-h4x0r/nmap-actions)
  "Nmap commands helm source definition.")

;;;###autoload
(defun helm-h4x0r ()
  "Helm H4x0r Commands."
  (interactive)
  (helm :sources '(helm-h4x0r/c-source-nmap)
        :candidate-number-limit 9999
        :buffer "*helm-h4x0r*"
        :prompt "Command> "
        :full-frame nil))

(provide 'helm-h4x0r)

;;; helm-h4x0r.el ends here
