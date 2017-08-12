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

(defvar nmap-path "/usr/share/nmap"
  "Nmap path.")

(defvar helm-h4x0r/nmap-actions
  '(("Execute in eshell" .
     (lambda (candidate)
       (let ((cmds '())
             (opts (read-string "Nmap options: "))
             (target (read-string "Target: ")))
         (if (string-match "NSE" candidate)
             (let ((execute-in "eshell"))
               (helm-nse))
           (progn
             (add-to-list 'cmds (concat "nmap " opts " " target) t)
             (msf>eshell-console cmds (concat "Nmap-" target)))))))
    ("Execute in async shell" .
     (lambda (candidate)
       (let ((opts (read-string "Nmap options: "))
             (target (read-string "Target: ")))
         (if (string-match "NSE" candidate)
             (let ((execute-in "async"))
               (helm-nse))
           (msf>async-process "nmap " opts " " target)))))
    ("Execute with tmux" .
     (lambda (candidate)
       (let ((opts (read-string "Nmap options: "))
             (target (read-string "Target: ")))
         (if (string-match "NSE" candidate)
             (let ((execute-in "tmux"))
               (helm-nse))
           (msf>tmux-run-and-wait (concat "nmap " opts " " target)))))))
  "Nmap actions.")

(defvar helm-h4x0r/nse-actions
  '(("Execute" .
     (lambda (_candidate)
       (let ((cmds '())
             (candidate (mapconcat 'identity (helm-marked-candidates) ",")))
         (when (string-match "eshell" execute-in)
           (add-to-list 'cmds (concat "nmap --script=" candidate " " opts " " target) t)
           (msf>eshell-console cmds (concat "Nmap-" target)))
         (when (string-match "async" execute-in)
           (msf>async-process "nmap --script=" candidate " " opts " " target))
         (when (string-match "tmux" execute-in)
           (msf>tmux-run-and-wait (concat "nmap --script=" candidate " " opts " " target))))))
    ("Open" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (find-file (nse-script-path candidate))))))
    "NSE actions.")

(defvar helm-h4x0r/c-source-nmap
  (helm-build-in-buffer-source "H4x0r Nmap Commands"
    :init (lambda ()
            (let ((cmds '("Execute Nmap command"
                          "Execute NSE script")))
              (with-current-buffer (helm-candidate-buffer 'local)
                (insert (mapconcat 'identity cmds "\n")))))
    :action helm-h4x0r/nmap-actions)
  "Nmap commands helm source definition.")

(defvar helm-h4x0r/c-source-nse
  (helm-build-in-buffer-source "H4x0r NSE Scripts"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity (directory-files (concat nmap-path "/scripts")) "\n"))))
    :action helm-h4x0r/nse-actions)
  "NSE scripts helm source definition.")

(defun nse-script-path (script)
  "Return nse SCRIPT path."
  (concat nmap-path "/scripts/" script))

;;;###autoload
(defun helm-h4x0r ()
  "Helm H4x0r Commands."
  (interactive)
  (helm :sources '(helm-h4x0r/c-source-nmap)
        :candidate-number-limit 9999
        :buffer "*helm-h4x0r*"
        :prompt "command> "
        :full-frame nil))

(defun helm-nse ()
  "Helm Nmap NSE scripts."
  (interactive)
  (helm :sources '(helm-h4x0r/c-source-nse)
        :candidate-number-limit 9999
        :buffer "*helm-nse*"
        :prompt "script> "
        :full-frame nil))

(provide 'helm-h4x0r)

;;; helm-h4x0r.el ends here
