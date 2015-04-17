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

(defcustom foreman:history-path "~/.emacs.d/foreman-history"
  "path for persistent proc history"
  :group 'foreman
  :type 'string)

(defcustom foreman:procfile "Procfile"
  "Procfile name"
  :group 'foreman
  :type 'string)

(defvar foreman-tasks '())

(defvar foreman-mode-map nil "Keymap for foreman mode.")

(setq foreman-mode-map (make-sparse-keymap))
(define-key foreman-mode-map "q" 'quit-window)
(define-key foreman-mode-map "s" 'foreman-start-proc)
(define-key foreman-mode-map "r" 'foreman-restart-proc)
(define-key foreman-mode-map "k" 'foreman-stop-proc)

(define-derived-mode foreman-mode tabulated-list-mode "foreman-mode"
  "forman-mode to manage procfile-based applications"
  (setq mode-name "Foreman")
  (setq tabulated-list-format [("Proc" 18 t)
                               ("command" 12 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Proc" nil))
  (tabulated-list-init-header))

(defun foreman ()
  (interactive)
  (load-procfile (find-procfile))
  (foreman-mode-fill-buffer
   "Foreman"))

(defun load-procfile (path)
  (let ((directory (f-parent path)))
    (with-temp-buffer
      (if (f-readable? path)
          (insert-file-contents path))
      (->> (s-lines (buffer-string))
           (-remove 's-blank?)
           (-map (-partial 's-split ":"))
           (-map (lambda (task)
                    (let ((key (format "%s:%s" directory (car task))))
                      (if (not (assoc key foreman-tasks))
                          (setq foreman-tasks
                                (cons `(,key . ((name . ,(s-trim (car task)))
                                                (directory . ,directory)
                                                (buffer . nil)
                                                (process . nil)
                                                (command . ,(s-trim (cadr task)))))
                                      foreman-tasks)))))))
      nil)))

(defun find-procfile ()
  (let ((dir (f-traverse-upwards
              (lambda (path)
                (f-exists? (f-expand foreman:procfile path)))
              ".")))
    (if dir (f-expand foreman:procfile dir))))

;; (defun foreman-history ()
;;   (interactive)
;;   (foreman-mode-fill-buffer
;;    "History"
;;    (load-procfile foreman:history-path)))

(defun foreman-find-task-buffer (task-name)
  (get-buffer (format "%s:%s")))

(defun foreman-make-task-buffer (task-name working-directory)
  (let ((buffer (generate-new-buffer task-name)))
    (with-current-buffer buffer
      (setq default-directory (f-slash working-directory)))
    buffer))

(defun foreman-ensure-task-buffer (task-name working-directory buffer)
  (if (buffer-live-p buffer) buffer
    (foreman-make-task-buffer task-name working-directory)))

(defun foreman-start-proc ()
  (interactive)
  (let* ((task-id (get-text-property (point) 'tabulated-list-id))
         (task (cdr (assoc task-id foreman-tasks)))
         (command (cdr (assoc 'command task)))
         (directory (cdr (assoc 'directory task)))
         (name (format "*%s:%s*" (-last-item (f-split directory)) (cdr (assoc 'name task))))
         (buffer (foreman-ensure-task-buffer name directory (cdr (assoc 'buffer task))))
         (process (with-current-buffer buffer
                    (apply 'start-process name buffer (s-split " +" command)))))
    (setf (cdr (assoc 'buffer task)) buffer)
    (setf (cdr (assoc 'process task)) process)
    (pop-to-buffer buffer)
    (message directory)
    (revert-buffer)))

(defun foreman-stop-proc ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (if (y-or-n-p (format "stop process %s? " (process-name process)))
        (progn 
          (restart-process process)
          (revert-buffer)))))

(defun foreman-restart-proc ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (if (y-or-n-p (format "restart process %s? " (process-name process)))
        (progn 
          (restart-process process)
          (revert-buffer)))))

(defun foreman-mode-fill-buffer (title)
  (switch-to-buffer (get-buffer-create "*foreman*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (foreman-mode)
  (setq tabulated-list-entries (forman-task-tabulate))
  (tabulated-list-print t)
  (setq buffer-read-only t))

(defun forman-task-tabulate ()
  (-map (lambda (task)
          (let ((detail (cdr task)))
            (list (car task)
                  (vconcat
                   (list (cdr (assoc 'name detail))
                         (cdr (assoc 'command detail))))))) foreman-tasks))


(provide 'foreman-mode)
;;; foreman-mode.el ends here


;; (setq foreman-tasks '())

;; (load-procfile "~/Procfile")

;; (assoc 'default-directory
;;        (buffer-local-variables
;;         (get-buffer "*Async Shell Command*")))

;; (get-buffer "buffer.org")

;; (start-process "zf" "*zf*" "ls" "-l")


;; (with-current-buffer "*zf*"
;;   (buffer-string))

;; (with-current-buffer "*Async Shell Command*"
;;   (setq default-directory "~"))
