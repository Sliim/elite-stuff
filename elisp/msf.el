;;; msf.el --- The Metasploit Framework Emacs helm interfaces.
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
;; By default the metasploit-framework sources directory is set to
;; /usr/share/metasploit-framework, you can customize this value by:
;;
;;     (setq msf-path "/opt/msf")
;;
;; Copy this file in your loadpath and:
;;
;;     M-x helm-msf

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

(require 'helm)
(require 'helm-files)
(require 'eshell)
(require 'emamux)
(require 'msf-opts)
(require 'msf-modules)
(require 'async)

(defcustom msf-path "/usr/share/metasploit-framework/"
  "The Metasploit framework path!"
  :type 'string)

(defcustom msf-user-dir ".msf5"
  "MSF user directory name."
  :type 'string)

(defvar msf/abs-path
  (expand-file-name msf-path)
  "The MSF absolute path.")

(defvar msf/user-dir
  (expand-file-name (concat (getenv "HOME") "/" msf-user-dir "/"))
  "The MSF user absolute path.")

(defvar msf/modules-path (expand-file-name (concat msf/abs-path "/modules/"))
  "The MSF Modules path.")

(defvar msf/tools-path (expand-file-name (concat msf/abs-path "/tools/"))
  "The MSF Tools path.")

(defvar msf/scripts-path (expand-file-name (concat msf/abs-path "/scripts/"))
  "The MSF Scripts path.")

(defvar msf/plugins-path (expand-file-name (concat msf/abs-path "/plugins/"))
  "The MSF Plugins path.")

(defvar msf/user-modules-path (expand-file-name (concat msf/user-dir "/modules/"))
  "The User Modules path.")

(defvar msf/user-plugins-path (expand-file-name (concat msf/user-dir "/plugins/"))
  "The User Plugins path.")

(defvar msf/user-scripts-path (expand-file-name (concat msf/user-dir "/scripts/"))
  "The User Scripts path.")

(defvar msf/console (expand-file-name (concat msf/abs-path "/msfconsole"))
  "The msfconsole bin script.")

(defvar msf/common-file-actions
  '(("Find File" . (lambda (_candidate)
                     (dolist (candidate (helm-marked-candidates))
                       (let ((files (msf>find-files candidate)))
                         (while files (find-file (pop files)))))))
    ("Open with Tmux" . (lambda (_candidate)
                          (dolist (candidate (helm-marked-candidates))
                            (let ((files (msf>find-files candidate)))
                              (while files (msf>tmux-open (pop files)))))
                          (helm-msf-files))))
  "MSF Common file actions.")

(defvar msf/module-file-msfconsole-actions
  '(("[MSFConsole][Emacs] Module Infos" .
     (lambda (candidate)
       (let ((m (msf>module-name-from-candidate-file candidate)))
         (alert (concat "[*] Loading " m " module info. This may take a while..") :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
         (shell-command (concat (expand-file-name
                                 (concat msf/abs-path "/msfconsole"))
                                " -q -L -x \"info " m ";quit\""))
         (alert (concat "[*] Module " m " loaded.") :icon "kali-metasploit" :title "Metasploit" :category 'pwnage))))
    ("[MSFConsole][Eshell] Load with msfconsole" .
     (lambda (candidate)
       (let ((m (msf>module-name-from-candidate-file candidate)))
         (alert (concat "[*] Loading " m " module in msfconsole. This may take a while..") :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
         (msf>eshell-msfconsole (concat "use " m ";show options;"))))))
  "MSF Modules file actions with msfconsole.")

(defvar msf/module-file-actions
  (append msf/common-file-actions
          msf/module-file-msfconsole-actions)
  "MSF All Modules file actions.")

(defvar msf/script-actions
  (append msf/common-file-actions
          '(("[EShell] Launch resources" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (let ((cmd '()))
                   (add-to-list 'cmd (concat "msf-resource " (file-name-nondirectory (concat candidate ".rc"))
                                             " " (msf>render-opts-oneline
                                                  (msf>merge-with-current-opts (msf>read-opts))) ";"))
                   (msf>eshell-console cmd (concat "Resource-" candidate))))))
            ("[Tmux] Launch resources" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (let ((r (file-name-nondirectory (concat candidate ".rc"))))
                   (msf>tmux-run-and-wait (concat "msf-resource " r " " (msf>render-opts-oneline
                                                                (msf>merge-with-current-opts (msf>read-opts))) ";"))))
               (helm-msf-files)))))
  "MSF Script actions.")

(defvar msf/plugin-actions
  (append msf/common-file-actions
          '(("[RPC] Load plugin" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (let ((p (file-name-nondirectory candidate)))
                   (let ((opts (read-string (concat "Loading plugin " p ": Any options? >> "))))
                     (msf>shell-command (concat "msf-plugin-load " p " "  opts ";")))))))
            ("[RPC] Unload plugin" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (let ((p (file-name-nondirectory candidate)))
                   (msf>shell-command (concat "msf-plugin-unload " p ";"))))))))
  "MSF Script actions.")

(defvar msf/c-source-modules-files
  (helm-build-in-buffer-source "MSF Modules files"
    :init (lambda ()
            (unless (boundp 'msf-modules)
              (setq msf-modules (msf>get-files msf/modules-path 5)))
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity msf-modules "\n"))))
    :action msf/module-file-actions)
  "MSF Modules files helm source definition.")

(defvar msf/c-source-scripts-files
  (helm-build-in-buffer-source "MSF Scripts files"
    :init (lambda ()
            (unless (boundp 'msf-scripts)
              (setq msf-scripts (msf>get-files msf/scripts-path 2)))
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity msf-scripts "\n"))))
    :action msf/script-actions)
  "MSF Scripts helm source definition.")

(defvar msf/c-source-tools-files
  (helm-build-in-buffer-source "MSF Tools files"
    :init (lambda ()
            (unless (boundp 'msf-tools)
              (setq msf-tools (msf>get-files msf/tools-path 2)))
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity msf-tools "\n"))))
    :action msf/common-file-actions)
  "MSF Tools helm source definition.")

(defvar msf/c-source-plugins-files
  (helm-build-in-buffer-source "MSF Plugins files"
    :init (lambda ()
            (unless (boundp 'msf-plugins)
              (setq msf-plugins (msf>get-files msf/plugins-path 1)))
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity msf-plugins "\n"))))
    :action msf/plugin-actions)
  "MSF Plugins helm source definition.")

(defvar msf/c-source-user-modules-files
  (helm-build-in-buffer-source "MSF User Modules files"
    :init (lambda ()
            (unless (boundp 'msf-user-modules)
              (setq msf-user-modules (msf>get-files msf/user-modules-path 5)))
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity msf-user-modules "\n"))))
    :action msf/module-file-actions)
  "MSF User Modules files helm source definition.")

(defvar msf/c-source-user-scripts-files
  (helm-build-in-buffer-source "MSF User Scripts files"
    :init (lambda ()
            (unless (boundp 'msf-user-scripts)
              (when (file-directory-p msf/user-scripts-path)
                (setq msf-user-scripts (msf>get-files msf/user-scripts-path 2))
                (with-current-buffer (helm-candidate-buffer 'local)
                  (insert (mapconcat 'identity msf-user-scripts "\n"))))))
    :action msf/script-actions)
  "MSF User Scripts helm source definition.")

(defvar msf/c-source-user-plugins-files
  (helm-build-in-buffer-source "MSF User Plugins files"
    :init (lambda ()
            (unless (boundp 'msf-user-plugins)
              (setq msf-user-plugins (msf>get-files msf/user-plugins-path 1)))
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity msf-user-plugins "\n"))))
    :action msf/plugin-actions)
  "MSF User Plugins helm source definition.")

(defun msf>module-name-from-candidate-file (candidate)
  "Return module name from CANDIDATE file name."
  (let ((module (replace-regexp-in-string
                 "modules/\\|s/singles\\|s/stagers\\|s/stages/"
                 "" candidate))) module))

(defun msf>find-files (component)
  "Return the absolute path for the given COMPONENT."
  (file-expand-wildcards (concat msf/abs-path component "*")))

(defun msf>get-files (directory maxdepth)
  "List msf ruby files in given DIRECTORY.
Recurse only to depth MAXDEPTH.  If zero or negative, then do not recurse."
  (message (concat "Searching MSF files in " directory ".."))
  (let* ((msf-files '())
         (current-directory-list
          (directory-files directory t)))
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))
          (setq msf-files (append msf-files (msf>get-files f (- maxdepth -1)))))
         (t))
        (when (file-regular-p f)
          (setq file (file-name-sans-extension
                      (replace-regexp-in-string
                       msf/user-dir ""
                       (replace-regexp-in-string
                        msf/abs-path "" f))))
          (message file)
          (setq msf-files (cons file msf-files))))
      (setq current-directory-list (cdr current-directory-list)))
    msf-files))

(defun msf>eshell-msfconsole (command)
  "Run msfconsole inside eshell and execute given COMMAND."
  (let ((command (concat msf/console " -x \"" command "\"")))
    (msf>eshell-console '(command))) "MSFConsole")

(defun msf>eshell-buffer-name (name)
  "Return correct buffer NAME."
  (replace-regexp-in-string "/" "-" name))

(defun msf>eshell-console (cmds &optional buffer-name)
  "Run CMDS in RPC console with eshell.  Set BUFFER-NAME if specified."
  (let ((create-new-buffer t))
    (when buffer-name
      (when (get-buffer (msf>eshell-buffer-name buffer-name))
        (setq create-new-buffer nil)
        (switch-to-buffer (msf>eshell-buffer-name buffer-name))))

    (when create-new-buffer
      (eshell 'Z)
      (when buffer-name
        (rename-buffer (msf>eshell-buffer-name buffer-name)))))

  (dolist (cmd cmds)
    (end-of-buffer)
    (eshell-kill-input)
    (insert cmd)
    (eshell-send-input))
  (end-of-buffer))

(defun msf>async-process (prog args)
  "Run Asynchronous process with given PROG with ARGS."
  (async-start-process prog prog
                       (lambda (proc)
                         (message "Async process %s finished: %d" proc (process-exit-status proc))
                         (alert (format "%s - exitstatus: %d" proc (process-exit-status proc))
                                :icon "kali-shellnoob"
                                :title "Async process finished"
                                :category 'pwnage))
                       args)
  (alert (concat "Started " prog " async process")
         :icon "kali-shellnoob"
         :title "Async process started"
         :category 'pwnage))

(defun msf>shell-command (cmd)
  "Run Shell CMD."
  (shell-command cmd))

(defun msf>tmux-open (file)
  "Open FILE in new tmux window."
  (emamux:tmux-run-command t "new-window" "-d"
                           "-n" (file-name-nondirectory file)
                           "less" file))

(defun msf>tmux-run (cmd &optional wname)
  "Run CMD in new tmux window named WNAME."
  (when (not wname)
    (setf wname "msf-command"))
  (emamux:tmux-run-command t "new-window" "-d" "-n" wname cmd))

(defun msf>tmux-run-and-wait (cmd &optional wname)
  "Run CMD in new tmux window named WNAME and wait for user input."
  (msf>tmux-run (concat cmd ";echo Press Enter to Quit;read") wname))

;;;###autoload
(defun helm-msf-files ()
  "Find MSF Modules, Scripts, Tools and Plugins."
  (interactive)
  (helm :sources '(msf/c-source-modules-files
                   msf/c-source-scripts-files
                   msf/c-source-plugins-files
                   msf/c-source-tools-files
                   msf/c-source-user-modules-files
                   msf/c-source-user-scripts-files
                   msf/c-source-user-plugins-files)
        :candidate-number-limit 9999
        :buffer "*msf*"
        :prompt "msf> "
        :full-frame t))

(defun msf-eshell-console-from-resource-buffer ()
  "Run eshell console and load current buffer as a msf resource."
  (interactive)
  (let ((cmds '("msf-console"))
        (buffer-name (file-name-sans-extension (buffer-name))))
    (add-to-list 'cmds (buffer-substring (point-min) (point-max)) t)
    (msf>eshell-console cmds buffer-name)))

(provide 'msf)

;;; msf.el ends here
