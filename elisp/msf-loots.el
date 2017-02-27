;;; msf-loots.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-loots

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

(defvar msf/loots-actions
  '(("Nothing" .
     (lambda (_candidate)
       (helm-msf-loots))))
  "MSF Loots actions.")

(defvar msf/c-source-loots
  (helm-build-in-buffer-source "MSF Loots"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (let ((cmd "msf-get-loots -l"))
                (when msf/current-rhost
                  (setf cmd (concat cmd " " msf/current-rhost)))
                (alert "Loading loots.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
                (insert (shell-command-to-string cmd)))))
    :action msf/loots-actions)
  "MSF Loots helm source definition.")

;;;###autoload
(defun helm-msf-loots ()
  "MSF Active Loots."
  (interactive)
  (helm :sources '(msf/c-source-loots)
        :candidate-number-limit 9999
        :buffer "*msf-loots*"
        :prompt "MSF loots> "
        :full-frame nil))

(provide 'msf-loots)

;;; msf-loots.el ends here
