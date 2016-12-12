;;; msf-notes.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-notes

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

(defvar msf/notes-actions
  '(("Show in new buffer" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((bufname (concat "Notes-" (msf>get-host-from-note-candidate candidate) ".org")))
           (get-buffer-create bufname)
           (switch-to-buffer-other-window bufname)
           (with-current-buffer bufname
             (org-mode)
             (insert (concat "* " (msf>get-info-from-note-candidate candidate) "\n"))
             (insert (concat "** Data: \n#+BEGIN_SRC ruby :results output\nrequire 'json'\ndata=" (msf>get-data-from-note-candidate candidate) "\nputs data.to_json\n#+END_SRC\n"))))))))
  "MSF Notes actions.")

(defvar msf/c-source-notes
  (helm-build-in-buffer-source "MSF Notes"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (let ((cmd "msf-get-notes -l"))
                (when msf/current-rhost
                  (setf cmd (concat cmd " " msf/current-rhost)))
                (message "[*] Loading notes..")
                (insert (shell-command-to-string cmd)))))
    :action msf/notes-actions)
  "MSF Notes helm source definition.")

(defun msf>get-host-from-note-candidate (candidate)
  "Get host from note CANDIDATE."
  (substring candidate 0 (string-match " - " candidate)))
(defun msf>get-info-from-note-candidate (candidate)
  "Get info from note CANDIDATE."
  (substring candidate 0 (string-match "|hash_data:" candidate)))
(defun msf>get-data-from-note-candidate (candidate)
  "Get JSON data from note CANDIDATE."
  (substring candidate (+ 12 (string-match "|hash_data:" candidate))))

;;;###autoload
(defun helm-msf-notes ()
  "MSF Active Notes."
  (interactive)
  (helm :sources '(msf/c-source-notes)
        :candidate-number-limit 9999
        :buffer "*msf-notes*"
        :prompt "MSF notes> "
        :full-frame nil))

(provide 'msf-notes)

;;; msf-notes.el ends here
