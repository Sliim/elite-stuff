;;; msf-services.el --- The Metasploit Framework Emacs helm interfaces.
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
;;     M-x helm-services

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

(defvar msf/services-actions
  '(("Search Auxiliary" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname (msf>get-service-name-from-candidate candidate)))
         (helm-msf-search-auxiliary msf/current-sname))))
    ("Search Scanners" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname (msf>get-service-name-from-candidate candidate)))
         (helm-search-scanners msf/current-sname))))
    ("Search Exploits" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname (msf>get-service-name-from-candidate candidate))
             (msf/current-ros ""))
         (helm-search-exploits msf/current-sname))))
    ("Search Exploits for this platform" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname (msf>get-service-name-from-candidate candidate))
             (msf/current-ros (msf>get-os-name-from-candidate candidate)))
         (helm-search-exploits msf/current-sname)))))
  "MSF Services actions.")

(defvar msf/c-source-services
  (helm-build-in-buffer-source "MSF Services"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message "[*] Loading host's services..")
              (insert (shell-command-to-string "msf-get-hosts-services -c"))))
    :action msf/services-actions)
  "MSF Services helm source definition.")

(defvar msf/c-source-auxiliary
  (helm-build-in-buffer-source "MSF Auxiliaries"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message (concat "[*] Searching auxiliary modules for " msf/current-sname " service"))
              (insert (shell-command-to-string (concat "msf-search-auxiliary " msf/current-sname)))))
    :action msf/auxiliary-module-actions)
  "MSF auxiliary modules helm source definition.")
(defvar msf/c-source-scanners
  (helm-build-in-buffer-source "MSF Scanners"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message (concat "[*] Searching scanner modules for " msf/current-sname " service"))
              (insert (shell-command-to-string (concat "msf-search-scanners " msf/current-sname)))))
    :action msf/auxiliary-module-actions)
  "MSF scanner modules helm source definition.")
(defvar msf/c-source-exploits
  (helm-build-in-buffer-source "MSF Exploits"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (message (concat "[*] Searching exploit modules for " msf/current-sname " service, platform: " msf/current-ros))
              (insert (shell-command-to-string (concat "msf-search-exploits " msf/current-sname " " msf/current-ros)))))
    :action msf/exploits-module-actions)
  "MSF exploit modules helm source definition.")

(defun msf>get-port-from-services-candidate (candidate)
  (let ((service (substring candidate 0 (string-match " " candidate))))
    (substring service (1+ (string-match ":" service)))))
(defun msf>get-host-from-services-candidate (candidate)
  (let ((service (substring candidate 0 (string-match " " candidate))))
    (substring service 0 (string-match ":" service))))
(defun msf>get-service-name-from-candidate (candidate)
  (if (string-match "\\([a-z]\\)\\/\\([a-zA-Z0-9]+\\)" candidate)
      (match-string 2 candidate)
    "/"))
(defun msf>get-os-name-from-candidate (candidate)
  (if (string-match "- \\([a-zA-Z0-9]+\\) " candidate)
      (match-string 1 candidate)
    "/"))

(defun helm-msf-search-auxiliary (sname)
  "Find MSF Auxiliary for given SNAME."
  (helm :sources '(msf/c-source-auxiliary)
        :candidate-number-limit 999
        :buffer (concat "*msf-auxiliary-" sname "*")
        :prompt "MSF auxiliary> "
        :full-frame nil))
(defun helm-search-scanners (sname)
  "Find MSF Scanners for given SNAME."
  (helm :sources '(msf/c-source-scanners)
        :candidate-number-limit 999
        :buffer (concat "*msf-scanners-" sname "*")
        :prompt "MSF scanners> "
        :full-frame nil))
(defun helm-search-exploits (sname)
  "Find MSF Exploits for given SNAME."
  (helm :sources '(msf/c-source-exploits)
        :candidate-number-limit 999
        :buffer (concat "*msf-exploits-" sname "*")
        :prompt "MSF exploits> "
        :full-frame nil))

;;;###autoload
(defun helm-msf-services ()
  "MSF Hosts services."
  (interactive)
  (helm :sources '(msf/c-source-services)
        :candidate-number-limit 9999
        :buffer "*msf-hosts-services*"
        :prompt "MSF hosts/services> "
        :full-frame nil))

(provide 'msf-services)

;;; msf-services.el ends here
