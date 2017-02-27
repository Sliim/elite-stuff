;;; msf-vulns.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-vulns

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

(defvar msf/vulns-actions
  '(("Show in new buffer" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((bufname (concat "Vulns-" (msf>get-host-from-vuln-candidate candidate) ".org")))
           (get-buffer-create bufname)
           (switch-to-buffer-other-window bufname)
           (with-current-buffer bufname
             (org-mode)
             (insert (concat "* " (msf>get-info-from-vuln-candidate candidate) "\n"))
             (insert (concat "** Refs: \n- "
                             (mapconcat 'identity
                                        (split-string
                                         (replace-regexp-in-string "URL-" "" (msf>get-refs-from-vuln-candidate candidate)) ",") "\n- ") "\n"))))))))
  "MSF Vulns actions.")

(defvar msf/c-source-vulns
  (helm-build-in-buffer-source "MSF Vulns"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (let ((cmd "msf-get-vulns -l"))
                (when msf/current-rhost
                  (setf cmd (concat cmd " " msf/current-rhost)))
                (alert "Loading vulns.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
                (insert (shell-command-to-string cmd)))))
    :action msf/vulns-actions)
  "MSF Vulns helm source definition.")

(defun msf>get-host-from-vuln-candidate (candidate)
  "Get host from vuln CANDIDATE."
  (substring candidate 0 (string-match " - " candidate)))
(defun msf>get-info-from-vuln-candidate (candidate)
  "Get info from vuln CANDIDATE."
  (substring candidate 0 (string-match "|refs:" candidate)))
(defun msf>get-refs-from-vuln-candidate (candidate)
  "Get JSON references from vuln CANDIDATE."
  (substring candidate (+ 7 (string-match "|refs:" candidate))))

;;;###autoload
(defun helm-msf-vulns ()
  "MSF Active Vulns."
  (interactive)
  (helm :sources '(msf/c-source-vulns)
        :candidate-number-limit 9999
        :buffer "*msf-vulns*"
        :prompt "MSF vulns> "
        :full-frame nil))

(provide 'msf-vulns)

;;; msf-vulns.el ends here
