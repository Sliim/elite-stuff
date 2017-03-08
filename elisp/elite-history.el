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

(defvar elite/modules-history '()
  "History of launched modules.")

(defvar elite/modules-history-actions
  '(("Replay module in console" .
     (lambda (_candidate)
       (dolist (candidate (helm-marked-candidates))
         (let ((cmd '()))
           (add-to-list 'cmd (concat "msf-console-module " candidate))
           (msf>eshell-console cmd (elite>get-module-name-from-history-candidate candidate)))))))
  "Elite modules history actions.")

(defvar elite/c-source-module-history
  (helm-build-in-buffer-source "Elite history"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (dolist (hist elite/modules-history)
                (insert (car hist))
                (setf hist (cdr hist))
                (insert (concat " \"" (car hist) "\" \"" (msf>render-opts-oneline (car (cdr hist))) "\"\n")))))
    :action elite/modules-history-actions)
  "Elite modules launched history helm source definition.")

(defun elite>get-module-name-from-history-candidate (candidate)
  "Get module name from history CANDIDATE."
  (substring candidate 0 (string-match " " candidate)))

;;;###autoload
(defun helm-elite-history ()
  "Elite Modules history."
  (interactive)
  (helm :sources '(elite/c-source-module-history)
        :candidate-number-limit 9999
        :buffer "*elite-history*"
        :prompt "Elite history> "
        :full-frame nil))

(provide 'elite-history)

;;; elite-history.el ends here
