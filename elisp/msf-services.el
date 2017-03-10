;;; msf-services.el --- The Metasploit Framework Emacs helm interfaces.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Package-Requires: ((helm "1.6.3") (eshell "2.4.2"))
;; Keywords: msf metasploit emacs helm eshell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides metasploit's services interaction.

;; Usage:
;;
;; Copy this file in your loadpath and:
;;
;;     M-x helm-msf-services

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
             (msf/current-sname (msf>get-service-name-from-candidate candidate))
             (msf/current-ros ""))
         (helm-msf-search-auxiliary msf/current-sname))))
    ("Search Scanners" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname (msf>get-service-name-from-candidate candidate))
             (msf/current-ros "scanner"))
         (helm-msf-search-auxiliary msf/current-sname))))
    ("Search Auxiliary (All)" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname "")
             (msf/current-ros ""))
         (helm-msf-search-auxiliary msf/current-sname))))
    ("Search all Exploits" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname "")
             (msf/current-ros ""))
         (helm-msf-search-exploits msf/current-sname))))
    ("Search Exploits for this service name" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname (msf>get-service-name-from-candidate candidate))
             (msf/current-ros ""))
         (helm-msf-search-exploits msf/current-sname))))
    ("Search Exploits for this platform" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname "")
             (msf/current-ros (msf>get-os-name-from-candidate candidate)))
         (helm-msf-search-exploits msf/current-sname))))
    ("Search Exploits for this service name and platform" .
     (lambda (candidate)
       (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
             (msf/current-rhost (msf>get-host-from-services-candidate candidate))
             (msf/current-sname (msf>get-service-name-from-candidate candidate))
             (msf/current-ros (msf>get-os-name-from-candidate candidate)))
         (helm-msf-search-exploits msf/current-sname))))
    ("Browse HTTP services in eww" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((msf/current-rport (msf>get-port-from-services-candidate candidate))
               (msf/current-rhost (msf>get-host-from-services-candidate candidate)))
           (if (y-or-n-p (format "%s:%s - SSL? " msf/current-rhost msf/current-rport))
               (setf scheme "https")
             (setf scheme "http"))
           (eww (format "%s://%s:%s"
                        scheme
                        (msf>get-host-from-services-candidate candidate)
                        (msf>get-port-from-services-candidate candidate))))))))
  "MSF Services actions.")

(defvar msf/c-source-services
  (helm-build-in-buffer-source "MSF Services"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (alert "Loading host's services.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
              (insert (shell-command-to-string "msf-get-hosts-services -c"))))
    :action msf/services-actions)
  "MSF Services helm source definition.")

(defvar msf/c-source-auxiliary-modules
  (helm-build-in-buffer-source "MSF Auxiliary"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (alert (concat "Searching auxiliary modules for `" msf/current-sname "` service") :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
              (insert (shell-command-to-string (concat "msf-search-modules auxiliary " msf/current-sname " " msf/current-ros)))))
    :action msf/auxiliary-module-actions)
  "MSF auxiliary modules helm source definition.")
(defvar msf/c-source-exploits-modules
  (helm-build-in-buffer-source "MSF Exploits"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (alert (concat "Searching exploit modules for `" msf/current-sname "` service, platform: `" msf/current-ros "`") :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
              (insert (shell-command-to-string (concat "msf-search-modules exploits " msf/current-sname " " msf/current-ros)))))
    :action msf/exploits-module-actions)
  "MSF exploit modules helm source definition.")

(defun msf>get-port-from-services-candidate (candidate)
  "Get service's port from helm CANDIDATE string."
  (let ((service (substring candidate 0 (string-match " " candidate))))
    (substring service (1+ (string-match ":" service)))))
(defun msf>get-host-from-services-candidate (candidate)
  "Get service's host from helm CANDIDATE string."
  (let ((service (substring candidate 0 (string-match " " candidate))))
    (substring service 0 (string-match ":" service))))
(defun msf>get-service-name-from-candidate (candidate)
  "Get service's name from helm CANDIDATE string."
  (if (string-match "\\([a-z]\\)\\/\\([a-zA-Z0-9]+\\)" candidate)
      (match-string 2 candidate)
    "/"))
(defun msf>get-os-name-from-candidate (candidate)
  "Get service's operating system from helm CANDIDATE string."
  (if (string-match "- \\([a-zA-Z0-9]+\\) " candidate)
      (match-string 1 candidate)
    "/"))

(defun helm-msf-search-auxiliary (sname)
  "Find MSF Auxiliary for given SNAME."
  (helm :sources '(msf/c-source-auxiliary-modules)
        :candidate-number-limit 999
        :buffer (concat "*msf-auxiliary-" sname "*")
        :prompt "MSF auxiliary> "
        :full-frame nil))
(defun helm-msf-search-exploits (sname)
  "Find MSF Exploits for given SNAME."
  (helm :sources '(msf/c-source-exploits-modules)
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
