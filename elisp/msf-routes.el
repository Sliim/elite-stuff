;;; msf-routes.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-routes

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

(defvar msf/routes-actions
  '(("Add new route" .
     (lambda (_candidate)
       (let ((cmd (concat "msf-add-route "
                          (read-string "Destination: ")
                          " " (read-string "Gateway: "))))
         (message "[*] Adding route..")
         (msf>shell-command cmd))))
    ("Remove routes" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmd (concat "msf-rm-route "
                            (msf>get-route-dest-from-candidate candidate) " "
                            (msf>get-route-gateway-from-candidate candidate))))
           (message "[*] Removing route..")
           (msf>shell-command cmd))))))
  "MSF Routes actions.")

(defvar msf/c-source-routes
  (helm-build-in-buffer-source "MSF Routes"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message "[*] Loading active routes..")
              (insert (shell-command-to-string "msf-get-routes -l"))))
    :action msf/routes-actions)
  "MSF Routes helm source definition.")

(defun msf>get-route-dest-from-candidate (candidate)
  "Get Route destination from CANDIDATE."
  (substring candidate (+ 3 (string-match " - " candidate))))
(defun msf>get-route-gateway-from-candidate (candidate)
  "Get Route gateway from CANDIDATE."
  (substring candidate 0 (string-match " - " candidate)))

;;;###autoload
(defun helm-msf-routes ()
  "MSF Active Routes."
  (interactive)
  (helm :sources '(msf/c-source-routes)
        :candidate-number-limit 9999
        :buffer "*msf-routes*"
        :prompt "MSF Active routes> "
        :full-frame nil))

(provide 'msf-routes)

;;; msf-routes.el ends here
