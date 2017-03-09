;;; elite-mode.el --- Elite mode.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

(require 'helm-mode)
(require 'msf)
(require 'msf-hosts)
(require 'msf-services)
(require 'msf-sessions)
(require 'msf-routes)
(require 'msf-consoles)
(require 'msf-jobs)
(require 'msf-threads)
(require 'msf-workspaces)
(require 'msf-notes)
(require 'msf-creds)
(require 'msf-vulns)
(require 'msf-loots)
(require 'msf-modules)
(require 'elite-history)
(require 'helm-h4x0r)
(require 'exploitdb)
(require 'popwin)
(require 'alert)

(defvar elite-mode-hook nil
  "Elite mode hook.")

(defcustom elite-reports-directory (file-truename "reports/")
  "Directory for generated reports.")

(defvar elite-mode-map
  (let ((map (make-keymap)))
    (define-key map "f" 'helm-msf-files)
    (define-key map "h" 'helm-msf-hosts)
    (define-key map "s" 'helm-msf-services)
    (define-key map "S" 'helm-msf-sessions)
    (define-key map "r" 'helm-msf-routes)
    (define-key map "c" 'helm-msf-consoles)
    (define-key map "C" 'msfrpc-create-new-console)
    (define-key map "j" 'helm-msf-jobs)
    (define-key map "t" 'helm-msf-threads)
    (define-key map "w" 'helm-msf-workspaces)
    (define-key map "n" 'helm-msf-notes)
    (define-key map "p" 'helm-msf-creds)
    (define-key map "v" 'helm-msf-vulns)
    (define-key map "l" 'helm-msf-loots)
    (define-key map "m" 'helm-msf-modules)
    (define-key map "x" 'helm-searchsploit)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "X" 'helm-h4x0r)
    (define-key map "H" 'helm-elite-history)
    (define-key map "I" 'elite-teamserver-infos)
    (define-key map "g" 'elite-mode)
    (define-key map (kbd "o e") 'msf>read-opts)
    (define-key map (kbd "o c") 'msf>clear-opts)
    (define-key map (kbd "b s") 'elite-save-all)
    map)
  "Keymap for Elite major mode")

(add-to-list 'auto-mode-alist '("*elite-mode*" . elite-mode))

(defun elite-reports-directory-with-teamserver ()
  "Return reports path with teamserver suffix."
  (if (and (getenv "MSFRPC_HOST")
           (getenv "MSFRPC_PORT"))
      (concat elite-reports-directory (getenv "MSFRPC_HOST") "-" (getenv "MSFRPC_PORT") "/")
    elite-reports-directory))

(defun elite-mode ()
  "The Elite mode"
  (interactive)
  (let ((bufname "*elite-mode*"))
    (get-buffer-create bufname)
    (switch-to-buffer-other-window bufname)
    (with-current-buffer bufname
      (erase-buffer)
      (insert "Emacs Elite Mode\n")
      (insert "================\n")
      (insert (concat "\n TeamServer: [" (getenv "MSFRPC_HOST") ":" (getenv "MSFRPC_PORT") "]\n"))
      (insert "----------------------\n")
      (insert "g - Refresh  |  I - TS Infos |  C - Console | H - History\n")
      (insert "\n")
      (insert "w - Workspaces  |  h - Hosts    |  s - Services\n")
      (insert "n - Notes       |  v - Vulns    |  p - Creds\n")
      (insert "j - Jobs        |  t - Threads  |  c - Consoles\n")
      (insert "S - Sessions    |  r - Routes   |  l - Loots\n")
      (insert "m - Modules\n")
      (insert "\n")
      (insert ">Options\n")
      (insert "o e - Edit options  |  o c - Clear options\n")
      (insert "Current:")
      (insert (msf>render-opts msf/user-opts))
      (insert "\n---------------\n")
      (insert "\n")
      (insert "\n Metasploit Local files\n")
      (insert "----------------------\n")
      (insert "f - Local files\n")
      (insert "\n Elite Stuff\n")
      (insert "----------\n")
      (insert "x - Search Sploit |  X - H4x0r Stuff\n")
      (insert "b s - Save All Buffers\n")
      (insert "\n")
      (insert "q - Quit\n")
      (kill-all-local-variables)
      (setq major-mode 'elite-mode)
      (setq mode-name "Elite")
      (use-local-map elite-mode-map)
      (run-hooks 'elite-mode-hook))))

(defun elite-teamserver-infos ()
  "Show TeamServer Infos notification."
  (interactive)
  (async-start
   (lambda ()
     (shell-command-to-string "msf-infos;echo;msf-status"))
   (lambda (result)
     (alert (concat "Elite mode started\n" result) :title "EliteStuff" :category 'pwnage))))

(defun elite-save-buffers ()
  "Save all buffers in elite-mode."
  (interactive)
  (dolist (b (buffer-list))
    (set-buffer b)
    (if (not (string-match "^ ?\\*" (buffer-name)))
        (write-file (concat (elite-reports-directory-with-teamserver) (buffer-name))))))

(defun elite-save-history ()
  "Write history in reports directory."
  (interactive)
  (with-temp-buffer
    (let ((file (concat (elite-reports-directory-with-teamserver) "history"))
          (coding-system-for-write 'utf-8))
      (print elite/modules-history (current-buffer))
      (when (file-writable-p file)
        (write-region (point-min) (point-max) file)))))

(defun elite-save-all ()
  "Save all files."
  (interactive)
  (elite-save-buffers)
  (elite-save-history))

(defun elite-notify-and-save-resources (module options command)
  "Notifications and save resources when launching modules."
  (add-to-list 'elite/modules-history `(,module ,command ,options))
  (alert (concat "Launch " module " with options:\n" (msf>render-opts options))
         :icon "kali-metasploit"
         :title "Metasploit - Launching module!"
         :category 'pwnage)
  (with-temp-buffer
    (insert (concat "use " module "\n" (msf>render-opts-cli options) "\n" command))
    (write-file (concat (elite-reports-directory-with-teamserver) (format-time-string "%s") "-" (msf>eshell-buffer-name module) ".rc"))))

(setq msf-module-run-function 'elite-notify-and-save-resources)

(push '("*elite-mode*" :stick t :height 35 :position top) popwin:special-display-config)

(run-at-time 0 60 'async-start
             (lambda ()
               (shell-command-to-string "msf-mode-line"))
             (lambda (result)
               (setq-default mode-line-format
                             (list result minor-mode-alist))))

(let ((file (concat (elite-reports-directory-with-teamserver) "history")))
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (setq elite/modules-history (read (current-buffer))))))

(provide 'elite-mode)

;;; elite-mode.el ends here
