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

(defcustom foreman:history-path "~/.emacs.d/foreman-history"
  "path for persistent proc history"
  :group 'foreman
  :type 'string)

(defcustom foreman:procfile "Procfile"
  "Procfile name"
  :group 'foreman
  :type 'string)

(defvar foreman-mode-map nil "Keymap for foreman mode.")

(if foreman-mode-map
    ()
  (setq foreman-mode-map (make-sparse-keymap))
  (define-key foreman-mode-map "n" 'foreman-mode-next-line)
  (define-key foreman-mode-map "p" 'foreman-mode-previous-line)
  (define-key foreman-mode-map "g" 'foreman-redraw)
  (define-key foreman-mode-map "q" 'quit-window)
  (define-key foreman-mode-map "s" 'foreman-start-proc)
  (define-key foreman-mode-map "r" 'foreman-restart-proc)
  (define-key foreman-mode-map "k" 'foreman-stop-proc))

(defun foreman-mode-next-line ()
  (interactive)
  (next-line 1))

(defun foreman-mode-previous-line ()
  (interactive)
  (previous-line 1))

(defun foreman-mode ()
  "Major mode for interacting with foreman processes."
  (interactive)
  (kill-all-local-variables)
  (use-local-map foreman-mode-map)
  (setq mode-name "Foreman")
  (setq major-mode 'foreman-mode))

(defun foreman ()
  (interactive)
  (foreman-mode-fill-buffer
   "Foreman"
   (load-procfile (find-procfile))))

(defun load-procfile (path)
  (with-temp-buffer
    (if (f-readable? path)
        (insert-file-contents path))
    (->> (s-lines (buffer-string))
        (-remove 's-blank?)
        (-map (-partial 's-split ":")))))

(defun find-procfile ()
  (let ((dir (f-traverse-upwards
           (lambda (path)
             (f-exists? (f-expand foreman:procfile path)))
           ".")))
    (if dir (f-expand foreman:procfile dir))))

(defun foreman-history ()
  (interactive)
  (foreman-mode-fill-buffer
   "History"
   (load-procfile foreman:history-path)))

(defun foreman-start ()
  ())

(load-procfile "~/Procfile")

(defun foreman-mode-fill-buffer (title content)
  (switch-to-buffer (get-buffer-create "*foreman*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert title "\n")
  (let ((tasks (s-join "\n" (-map (-partial 's-join ":") content))))
    (insert tasks)
    (goto-char (point-min))
    (setq buffer-read-only t)
    (foreman-mode)))

(shell-command-to-string "ps aux")

(provide 'foreman-mode)
;;; foreman-mode.el ends here
