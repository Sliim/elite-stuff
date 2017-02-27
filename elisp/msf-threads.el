;;; msf-threads.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-threads

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

(defvar msf/threads-actions
  '(("Nothing" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (message (concat candidate " - ID: " (msf>get-thread-id-from-candidate candidate))))))
    ("Kill thread" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmd (concat "msf-kill-thread " (msf>get-thread-id-from-candidate candidate))))
           (msf>shell-command cmd))))))
  "MSF Threads actions.")

(defvar msf/c-source-threads
  (helm-build-in-buffer-source "MSF Threads"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (alert "Loading active threads.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
              (insert (shell-command-to-string "msf-get-threads -l"))))
    :action msf/threads-actions)
  "MSF Threads helm source definition.")

(defun msf>get-thread-id-from-candidate (candidate)
  "Get Thread ID from CANDIDATE."
  (substring candidate 0 (string-match " - " candidate)))

;;;###autoload
(defun helm-msf-threads ()
  "MSF Active Threads."
  (interactive)
  (helm :sources '(msf/c-source-threads)
        :candidate-number-limit 9999
        :buffer "*msf-threads*"
        :prompt "MSF Active threads> "
        :full-frame nil))

(provide 'msf-threads)

;;; msf-threads.el ends here
