;;; foreman-mode.el --- foreman-mode

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/foreman-mode
;; Keywords: foreman
;; Version: 0.0.1
;; Created: 17th Apr 2015
;; Package-Requires: ((s "1.9.0") (dash "2.10.0") (dash-functional "1.2.0") (f "0.17.2"))

;;; Commentary:
;;
;; Manage Procfile-based applications
;;

;;; Code:
(require 's)
(require 'f)
(require 'dash)
(require 'tabulated-list)
(require 'ansi-color)

(defcustom foreman:history-path "~/.emacs.d/foreman-history"
  "path for persistent proc history"
  :group 'foreman
  :type 'string)

(defcustom foreman:procfile "Procfile"
  "Procfile name"
  :group 'foreman
  :type 'string)

(defvar foreman-tasks '())
;; (setq foreman-tasks '())

(defvar foreman-current-id nil)

(defvar foreman-mode-map nil "Keymap for foreman mode.")

(setq foreman-mode-map (make-sparse-keymap))
(define-key foreman-mode-map "q" 'quit-window)
(define-key foreman-mode-map "s" 'foreman-start-proc)
(define-key foreman-mode-map "r" 'foreman-restart-proc)
(define-key foreman-mode-map (kbd "RET") 'foreman-view-buffer)
(define-key foreman-mode-map "k" 'foreman-kill-proc)
(define-key foreman-mode-map "d" 'foreman-kill-buffer)
(define-key foreman-mode-map "n" 'foreman-next-line)
(define-key foreman-mode-map "p" 'foreman-previous-line)

(define-derived-mode foreman-mode tabulated-list-mode "foreman-mode"
  "forman-mode to manage procfile-based applications"
  (setq mode-name "Foreman")
  (setq tabulated-list-format [("name" 12 t)
                               ("status" 12 t)
                               ("command" 12 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "name" nil))
  (tabulated-list-init-header))

(defun foreman ()
  (interactive)
  (load-procfile (find-procfile))
  (foreman-fill-buffer)
  (foreman-restore-cursor))

(defun foreman-start ()
  (interactive)
  (-each (load-procfile (find-procfile))
    'foreman-start-proc-internal)
  (foreman-fill-buffer))

(defun foreman-stop ()
  (interactive)
  (-each (load-procfile (find-procfile))
    (lambda (task-id)
      (cond ((assoc task-id foreman-tasks)
             (let ((buffer (foreman-get-in foreman-tasks task-id 'buffer)))
               (if buffer (kill-buffer buffer)))
             (setq foreman-tasks (delq (assoc task-id foreman-tasks) foreman-tasks))))))
  (message "all process killed"))

(defun foreman-clear ()
  (interactive)
  (setq foreman-tasks nil))

(defun foreman-restart ()
  (interactive)
  (foreman-stop)
  (foreman-start))

(defun foreman-next-line ()
  (interactive)
  (if (> (count-lines (point) (point-max)) 1)
      (progn 
        (forward-line 1)
        (foreman-view-buffer))))

(defun foreman-previous-line ()
  (interactive)
  (forward-line -1)
  (foreman-view-buffer))

(defun load-procfile (path)
  (let ((directory (f-parent path)))
    (with-temp-buffer
      (if (f-readable? path)
          (insert-file-contents path))
      (->> (s-lines (buffer-string))
           (-remove 's-blank?)
           (-remove (-partial 's-starts-with? "#"))
           (-map (-partial 's-split ":"))
           (-map (lambda (task)
                   (let ((key (format "%s:%s" directory (car task))))
                     (if (not (assoc key foreman-tasks))
                         (setq foreman-tasks
                               (cons `(,key . ((name . ,(s-trim (car task)))
                                               (directory . ,directory)
                                               (command . ,(s-trim (cadr task)))))
                                     foreman-tasks)))
                     key)))))))

(defun find-procfile ()
  (let ((dir (f-traverse-upwards
              (lambda (path)
                (f-exists? (f-expand foreman:procfile path)))
              ".")))
    (if dir (f-expand foreman:procfile dir))))

(defun foreman-make-task-buffer (task-name working-directory)
  (let ((buffer (generate-new-buffer task-name)))
    (with-current-buffer buffer
      (setq default-directory (f-slash working-directory))
      (set (make-local-variable 'window-point-insertion-type) t)
      (add-hook 'after-change-functions (lambda (start end length)
                                          (ansi-color-apply-on-region start end))))
    buffer))

(defun foreman-ensure-task-buffer (task-name working-directory buffer)
  (if (buffer-live-p buffer) buffer
    (foreman-make-task-buffer task-name working-directory)))

(defun foreman-start-proc ()
  (interactive)
  (let ((task-id (get-text-property (point) 'tabulated-list-id)))
    (foreman-start-proc-internal task-id)
    (revert-buffer)
    (pop-to-buffer (foreman-get-in foreman-tasks task-id 'buffer) nil t)
    (other-window -1)))

(defun foreman-start-proc-internal (task-id)
  (if (not (process-live-p (foreman-get-in foreman-tasks task-id 'process)))
      (let* ((task (cdr (assoc task-id foreman-tasks)))
             (command (cdr (assoc 'command task)))
             (directory (cdr (assoc 'directory task)))
             (name (format "*%s:%s*" (-last-item (f-split directory)) (cdr (assoc 'name task))))
             (buffer (foreman-ensure-task-buffer name directory (cdr (assoc 'buffer task))))
             (process (with-current-buffer buffer
                        (apply 'start-process-shell-command name buffer (s-split " +" command)))))
        (if (assoc 'buffer task)
            (setf (cdr (assoc 'buffer task)) buffer)
          (setq task (cons `(buffer . ,buffer) task)))
        (if (assoc 'process task)
            (setf (cdr (assoc 'process task)) process)
          (setq task (cons `(process . ,process) task)))
        (setf (cdr (assoc task-id foreman-tasks)) task))))

(defun foreman-kill-proc ()
  (interactive)
  (let* ((task-id (get-text-property (point) 'tabulated-list-id))
         (task (cdr (assoc task-id foreman-tasks)))
         (process (cdr (assoc 'process task))))
    (cond ((and (process-live-p process)
                (y-or-n-p (format "kill process %s? " (process-name process))))
           (kill-process process)
           (revert-buffer)))))

(defun foreman-kill-buffer ()
  (interactive)
  (let* ((task-id (get-text-property (point) 'tabulated-list-id))
         (buffer (foreman-get-in foreman-tasks task-id 'buffer)))
    (cond ((and (buffer-live-p buffer)
                (y-or-n-p (format "kill buffer %s? " (buffer-name buffer))))
           (kill-buffer buffer)
           (revert-buffer)))))

(defun foreman-error-buffer (msg)
  (let ((buffer (get-buffer-create "*foreman-error*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert msg))
    buffer))

(defun foreman-view-buffer ()
  (interactive)
  (let* ((task-id (get-text-property (point) 'tabulated-list-id))
         (buffer (foreman-get-in foreman-tasks task-id 'buffer)))
    (pop-to-buffer (if (buffer-live-p buffer) buffer
                     (foreman-error-buffer "application not running\n")) t t)
    (other-window -1)))

(defun foreman-get-in (alist &rest keys)
  (if keys
      (apply 'foreman-get-in (cdr (assoc (car keys) alist)) (cdr keys))
    alist))

(defun foreman-restart-proc ()
  (interactive)
  (let* ((task-id (get-text-property (point) 'tabulated-list-id))
         (task (cdr (assoc task-id foreman-tasks)))
         (buffer (cdr (assoc 'buffer task)))
         (command (cdr (assoc 'command task)))
         (directory (cdr (assoc 'directory task)))
         (name (format "*%s:%s*" (-last-item (f-split directory)) (cdr (assoc 'name task))))
         (process (cdr (assoc 'process task))))
    (if (y-or-n-p (format "restart process %s? " (process-name process)))
        (progn 
          (kill-process process)
          (setf (cdr (assoc 'process task))
                (with-current-buffer buffer
                  (apply 'start-process-shell-command name buffer (s-split " +" command))))
          (revert-buffer)))))

(defun foreman-fill-buffer ()
  (switch-to-buffer (get-buffer-create "*foreman*"))
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (foreman-mode)
  (setq tabulated-list-entries (foreman-task-tabulate))
  (tabulated-list-print t)
  (setq buffer-read-only t))

(defun foreman-task-tabulate ()
  (-map (lambda (task)
          (let* ((detail (cdr task))
                 (process (cdr (assoc 'process detail))))
            (list (car task)
                  (vconcat
                   (list (cdr (assoc 'name detail))
                         (if process (symbol-name (process-status process)) "")
                         (cdr (assoc 'command detail))))))) foreman-tasks))

(defun foreman-restore-cursor ()
  (while (and (< (point) (point-max))
              (not (string= foreman-current-id
                            (get-text-property (point) 'tabulated-list-id))))
    (next-line)))

(add-hook 'tabulated-list-revert-hook
          (lambda ()
            (interactive)
            (load-procfile (find-procfile))
            (setq foreman-current-id (get-text-property (point) 'tabulated-list-id))
            (foreman-fill-buffer)
            (foreman-restore-cursor)))

(provide 'foreman-mode)
;;; foreman-mode.el ends here
