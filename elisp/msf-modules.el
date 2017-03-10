;;; msf-modules.el --- The Metasploit Framework Emacs helm interfaces.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Package-Requires: ((helm "1.6.3") (eshell "2.4.2"))
;; Keywords: msf metasploit emacs helm eshell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Modules helm interface throught msfrpc server.

;; Usage:
;;
;; Copy this file in your loadpath and:
;;
;;     M-x helm-msf-modules

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

(defvar msf/exploits-modules nil
  "Exploits module list.")
(defvar msf/auxiliary-modules nil
  "Auxiliary module list.")
(defvar msf/payloads-modules nil
  "Payloads module list.")
(defvar msf/post-modules nil
  "Post module list.")
(defvar msf/encoders-modules nil
  "Encoders module list.")
(defvar msf/nops-modules nil
  "NOPs module list.")

(defvar msf/module-actions
  '(("Module Infos" .
     (lambda (candidate)
       (msf>shell-command (concat "msf-module " candidate " info;"))))
    ("Open in Console" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmds '("msf-console")))
           (add-to-list 'cmds (concat "use " candidate) t)
           (add-to-list 'cmds "info" t)
           (msf>eshell-console cmds (concat "Console-" candidate))))))
    ("Modules Infos [Tmux]" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (msf>tmux-run (concat "msf-module " candidate " info;")))))
    ("Copy Module name to clipboard" .
     (lambda (candidate)
       (kill-new candidate))))
  "MSF Modules actions.")

(defcustom msf-module-run-function nil
  "Function to run when launching module.")

(defun msf>module-run-async-shell (module options command)
  "Load MODULE, set OPTIONS and run COMMAND in async shell."
  (funcall msf-module-run-function module options command)
  (msf>async-process "msf-module" module command (msf>render-opts-oneline options)))

(defun msf>module-run-eshell-console (module options command)
  "Load MODULE, set OPTIONS and run COMMAND in eshell."
  (funcall msf-module-run-function module options command)
  (let ((cmd '()))
    (add-to-list 'cmd (concat "msf-console-module " module " \"" command "\" \""
                              (msf>render-opts-oneline options) "\";"))

    (msf>eshell-console cmd module)))

(defun msf>module-run-tmux-window (module options command)
  "Load MODULE, set OPTIONS and run COMMAND in new Tmux window."
  (funcall msf-module-run-function module options command)
  (msf>tmux-run (concat "msf-module " module " \"" command "\" \""
                        (msf>render-opts-oneline options) "\";")))

(defvar msf/auxiliary-module-actions
  (append '(("Set options and launch modules in new console" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-eshell-console candidate (msf>merge-with-current-opts (msf>read-module-opts candidate)) "run -j"))))
            ("Set options and launch modules" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-async-shell candidate (msf>merge-with-current-opts (msf>read-module-opts candidate)) "run -j"))))
            ("Launch module with defined options" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-async-shell candidate (msf>merge-with-current-opts msf/user-opts) "run -j"))))
            ("Launch modules in new Tmux window" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-tmux-window candidate (msf>merge-with-current-opts (msf>read-module-opts candidate)) "run -j")))))
          msf/module-actions)
  "MSF Auxiliary module actions.")

(defvar msf/exploits-module-actions
  (append '(("Set options and launch modules in new console" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-eshell-console candidate (msf>merge-with-current-opts (msf>read-module-opts candidate)) "exploit -j -z"))))
            ("Set options and launch modules" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-async-shell candidate (msf>merge-with-current-opts (msf>read-module-opts candidate)) "exploit -j -z"))))
            ("Launch module with defined options" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-async-shell candidate (msf>merge-with-current-opts msf/user-opts) "exploit -j -z"))))
            ("Launch modules in new Tmux window" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-tmux-window candidate (msf>merge-with-current-opts (msf>read-module-opts candidate)) "exploit -j -z")))))
          msf/module-actions)
  "MSF Exploits module actions.")

(defvar msf/post-module-actions
  (append '(("Set options and launch modules in new console" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-eshell-console candidate (msf>merge-with-current-opts (msf>read-module-opts candidate)) "run -j"))))
            ("Launch module with defined options" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-async-shell candidate (msf>merge-with-current-opts msf/user-opts) "run -j"))))
            ("Launch modules in new Tmux window" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>module-run-tmux-window candidate (msf>merge-with-current-opts (msf>read-module-opts candidate)) "run -j")))))
          msf/module-actions)
  "MSF Post module actions.")

(defvar msf/payload-module-actions
  (append '(("Set options and Start handler" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>async-process "msf-handler" candidate (msf>render-opts-oneline (msf>merge-with-current-opts (msf>read-module-opts candidate)))))))
            ("Start handler with defined options" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>async-process "msf-handler" candidate (msf>render-opts-oneline (msf>merge-with-current-opts msf/user-opts))))))
            ("Generate payload" .
             (lambda (_candidate)
               (dolist (candidate (helm-marked-candidates))
                 (msf>shell-command (concat "msf-module " candidate " \"generate "
                                            (read-string (concat "Command line options for " candidate ": ")) "\";"))))))
          msf/module-actions)
  "MSF Payloads modules actions.")

(defvar msf/c-source-module-exploits
  (helm-build-in-buffer-source "MSF Exploits"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (when (not msf/exploits-modules)
                (alert "Loading exploits modules.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
                (setq msf/exploits-modules (shell-command-to-string "msf-get-modules -l exploits")))
              (insert msf/exploits-modules)))
    :action msf/auxiliary-module-actions)
  "MSF Exploits module helm source definition.")
(defvar msf/c-source-module-auxiliary
  (helm-build-in-buffer-source "MSF Auxiliary"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (when (not msf/auxiliary-modules)
                (alert "Loading auxiliary modules.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
                (setq msf/auxiliary-modules (shell-command-to-string "msf-get-modules -l auxiliary")))
              (insert msf/auxiliary-modules)))
    :action msf/auxiliary-module-actions)
  "MSF Auxiliary module helm source definition.")
(defvar msf/c-source-module-payloads
  (helm-build-in-buffer-source "MSF Payloads"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (when (not msf/payloads-modules)
                (alert "Loading payloads modules.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
                (setq msf/payloads-modules (shell-command-to-string "msf-get-modules -l payloads")))
              (insert msf/payloads-modules)))
    :action msf/payload-module-actions)
  "MSF Payloads module helm source definition.")
(defvar msf/c-source-module-post
  (helm-build-in-buffer-source "MSF Post"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (when (not msf/post-modules)
                (alert "Loading post modules.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
                (setq msf/post-modules (shell-command-to-string "msf-get-modules -l post")))
              (insert msf/post-modules)))
    :action msf/module-actions)
  "MSF Post module helm source definition.")
(defvar msf/c-source-module-encoders
  (helm-build-in-buffer-source "MSF Encoders"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (when (not msf/encoders-modules)
                (alert "Loading encoders modules.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
                (setq msf/encoders-modules (shell-command-to-string "msf-get-modules -l encoders")))
              (insert msf/encoders-modules)))
    :action msf/post-module-actions)
  "MSF Encoders module helm source definition.")
(defvar msf/c-source-module-nops
  (helm-build-in-buffer-source "MSF NOPs"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (when (not msf/nops-modules)
                (alert "Loading nops modules.." :icon "kali-metasploit" :title "Metasploit" :category 'pwnage)
                (setq msf/nops-modules (shell-command-to-string "msf-get-modules -l nops")))
              (insert msf/nops-modules)))
    :action msf/module-actions)
  "MSF NOPs module helm source definition.")

;;;###autoload
(defun helm-msf-modules ()
  "MSF Modules."
  (interactive)
  (helm :sources '(msf/c-source-module-exploits
                   msf/c-source-module-auxiliary
                   msf/c-source-module-payloads
                   msf/c-source-module-post
                   msf/c-source-module-encoders
                   msf/c-source-module-nops)
        :candidate-number-limit 9999
        :buffer "*msf-modules*"
        :prompt "MSF Modules> "
        :full-frame nil))

(provide 'msf-modules)

;;; msf-modules.el ends here
