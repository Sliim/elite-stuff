;;; elite-history.el --- Manage elite history.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Package-Requires: ()
;; Keywords: msf metasploit emacs helm eshell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Manage launched modules history and replay

;; Usage:
;;
;; Copy this file in your loadpath and:
;;
;;     M-x helm-elite-history

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

(defvar elite/history (make-hash-table :test 'equal)
  "History of launched rocket.")

(defvar elite/history-modules-actions
  '(("Replay module in console" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmd '()))
           (add-to-list 'cmd (concat "msf-console-module " candidate))
           (msf>eshell-console cmd (elite>get-module-name-from-history-candidate candidate)))))))
  "Elite modules history actions.")

(defvar elite/history-eshell-actions
  '(("Replay eshell command." .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (msf>eshell-console (read candidate))))))
  "Elite eshell commands history actions.")

(defvar elite/history-async-actions
  '(("Rerun async process." .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((prog (car (read candidate)))
               (args (cdr (read candidate))))
           (apply 'msf>async-process prog args))))))
  "Elite async processes history actions.")

(defvar elite/c-source-module-history
  (helm-build-in-buffer-source "Modules history"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (dolist (hist (gethash "modules" elite/history))
                (insert (car hist))
                (setf hist (cdr hist))
                (insert (concat " \"" (car hist) "\" \"" (msf>render-opts-oneline (car (cdr hist))) "\"\n")))))
    :action elite/history-modules-actions)
  "Launched modules history helm source definition.")

(defvar elite/c-source-eshell-history
  (helm-build-in-buffer-source "Eshell commands history"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (dolist (hist (gethash "eshell" elite/history))
                (insert (format "%S\n" hist)))))
    :action elite/history-eshell-actions)
  "Elite eshell commands history helm source definition.")

(defvar elite/c-source-async-history
  (helm-build-in-buffer-source "Async processes history"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (dolist (hist (gethash "async" elite/history))
                (insert (format "%S\n" hist)))))
    :action elite/history-async-actions)
  "Elite async processes history helm source definition.")

(defun elite>get-module-name-from-history-candidate (candidate)
  "Get module name from history CANDIDATE."
  (substring candidate 0 (string-match " " candidate)))

(defun elite>load-history (file)
  "Load history from FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (setq elite/history (read (current-buffer))))))

(defun elite>add-module-history (module command options)
  "Add MODULE history with COMMAND and OPTIONS."
  (let ((elements `(,module ,command ,options))
        (hist (gethash "modules" elite/history)))
    (if hist
        (unless (member elements hist)
          (add-to-list 'hist elements)
          (puthash "modules" hist elite/history))
      (puthash "modules" `((,module ,command ,options)) elite/history))))

(defun elite>add-eshell-history (commands)
  "Add eshell command history with COMMANDS."
  (if (gethash "eshell" elite/history)
      (let ((hist (gethash "eshell" elite/history)))
        (unless (member commands hist)
          (add-to-list 'hist commands)
          (puthash "eshell" hist elite/history)))
    (puthash "eshell" `(,commands) elite/history)))

(defun elite>add-async-history (prog args)
  "Add async process history for PROG with its ARGS."
  (add-to-list 'args prog)
  (if (gethash "async" elite/history)
      (let ((hist (gethash "async" elite/history)))
        (unless (member args hist)
          (add-to-list 'hist args)
          (puthash "async" hist elite/history)))
    (puthash "async" `(,args) elite/history)))

;;;###autoload
(defun helm-elite-history ()
  "Elite Modules history."
  (interactive)
  (helm :sources '(elite/c-source-module-history
                   elite/c-source-eshell-history
                   elite/c-source-async-history)
        :candidate-number-limit 9999
        :buffer "*elite-history*"
        :prompt "Elite history> "
        :full-frame nil))

(provide 'elite-history)

;;; elite-history.el ends here
