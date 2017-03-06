;;; msf-opts.el --- MSF Options management.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; MSF Options managements

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

(defvar msf/user-opts '()
  "MSF user options.")
(defvar msf/current-rport nil
  "Current RPORT.")
(defvar msf/current-rhost nil
  "Current RHOST.")
(defvar msf/current-sessid nil
  "Current Session ID.")
(defvar msf/current-payload nil
  "Current Payload.")

(defun msf>render-opts (opts)
  "Render OPTS list in multiple line string."
  (let ((opts-str ""))
    (dolist (opt opts)
      (setf opts-str (concat opts-str "\n" (car opt) " " (cdr opt))))
    opts-str))

(defun msf>render-opts-cli (opts)
  "Render OPTS list in multiple line string for resources generation."
  (let ((opts-str ""))
    (dolist (opt opts)
      (setf opts-str (concat opts-str "\nset " (car opt) " " (cdr opt))))
    opts-str))

(defun msf>render-opts-oneline (opts)
  "Render OPTS list in one line string."
  (let ((opts-list '()))
    (dolist (opt opts)
      (setf opts-list (add-to-list 'opts-list (concat (car opt) "=" (cdr opt)) t)))
    (mapconcat 'identity opts-list ";")))

(defun msf>get-current-opts ()
  "Retrieve current options."
  (let ((opts '()))
    (when msf/current-rport
      (setf opts (acons "RPORT" msf/current-rport opts)))
    (when msf/current-rhost
      (setf opts (acons "RHOST" msf/current-rhost opts))
      (setf opts (acons "RHOSTS" msf/current-rhost opts)))
    (when msf/current-sessid
      (setf opts (acons "SESSION" msf/current-sessid opts)))
    (when msf/current-payload
      (setf opts (acons "PAYLOAD" msf/current-payload opts)))
    opts))

(defun msf>merge-with-current-opts (opts)
  "Merge OPTS with current options."
  (append opts (msf>get-current-opts)))

(defun msf>clear-opts ()
  "Clear all user options."
  (interactive)
  (setq msf/user-opts '()))

(defun msf>set-opt(key &optional value)
  "Set opt KEY with VALUE."
  (let ((o (reverse msf/user-opts)))
    (delete* key o :test 'equal :key 'car)
    (setf o (reverse (remove* key (reverse o) :test 'equal :key 'car)))
    (when value
      (setf o (acons key value o)))
    (setq msf/user-opts (reverse o))))

(defun msf>read-opts ()
  "Read MSF options from user input."
  (interactive)
  (let ((user-input "-"))
    (cl-loop until (string= "" user-input)
             do (setf user-input (read-string (concat "Current opts:" (msf>render-opts (msf>merge-with-current-opts msf/user-opts)) "\n\nOverwrite or Add options >> ")))
             do (let ((k (car (split-string user-input)))
                      (v (car (cdr (split-string user-input)))))
                  (msf>set-opt k v))))
  msf/user-opts)

(defun msf>read-module-opts (module)
  "Display MODULE options and read MSF options from user input."
  (interactive)
  (let ((msg (concat "Retrieving options for module " module)))
    (message msg)
    (alert msg :icon "kali-metasploit" :title "Loading Module options.." :category 'pwnage)
    (let ((bufname (concat "*module-options-" module "*"))
          (options (concat "Options for " module ":\n\n" (shell-command-to-string (concat "msf-module " candidate " info;")))))
      (get-buffer-create bufname)
      (switch-to-buffer bufname)
      (with-current-buffer bufname
        (insert options))))
  (msf>read-opts))

(provide 'msf-opts)
