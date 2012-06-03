;;; org-status.el --- 
;; 
;; Filename: org-status.el
;; Description: 
;; Author: Neil Smithline
;; Maintainer: 
;; Copyright (C) 2012, Neil Smithline, all rights reserved.;; Created: Sun May 27 09:24:41 2012 (-0400)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defgroup org-status nil
  "Settings for `org-status-update' commands."
  :group 'org)

(defcustom org-status-output-buffer "*Org Status Output*"
  "Status buffer from `org-status-update' commands."
  :type 'string
  :safe t
  :risky nil
  :group 'org-status)

(defcustom org-status-twitter-command "/usr/local/Cellar/ruby/1.9.3-p0/bin/t"
  "Full path to the installed `t' command on your system.

See https://github.com/sferik/t for instructions on installing `t'."
  :type '(file :must match t)
  :risky t
  :safe nil
  :group 'org-status)

(defun org-status-tweet ()
  "Do a tweet for the current headline."
  (beginning-of-line 1)
  (let* ((status-props      (org-agenda-get-some-entry-text
                             (point-marker) 9999))
         (status            (substring-no-properties status-props))
         (headline         (nth 4 (org-heading-components))))
    (message "Tweeting %s . . ." headline)
    (assert (<= (length status) 140) t)
    (get-buffer-create org-status-output-buffer)
    (let ((success (shell-command 
                    (format "%s update %s"
                            org-status-twitter-command
                            (shell-quote-argument status))
                    org-status-output-buffer
                    org-status-output-buffer))
          (output (substring 
                   (save-excursion
                     (set-buffer org-status-output-buffer)
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))
                   (1+ (length org-status-twitter-command)))))
      (if (zerop success)
          (progn
            (org-todo 'done)
            output)
        ;; Remove trailing newline and period for error messages.
        (let ((error-string (substring output 0 (- (length output) 2))))
          (error "Error (%s): %s: `%s'"
                 success error-string headline))))))

(defun org-status-updates ()
  "Loop through entries looking for status updates."
  (interactive)
  (let ((results (org-map-entries #'org-status-tweet
                                        "TWEET+TODO=\"POST\"")))
   (when results
     (with-output-to-temp-buffer org-status-output-buffer
      (set-buffer org-status-output-buffer)
      (print-elements-of-list results)))))

(unless :COMMENT-on-hold 

(defvar org-status-buffer nil
  "Non-nil if `org-status-updates` shoul be run when saving this buffer.")

(make-variable-buffer-local 'org-status-buffer)
(setq-default org-status-buffer nil)

(defhook set-org-status-buffer (org-mode-hook)
  (if (string-match "-status.org$" (or (buffer-file-name) ""))
      (setq org-status-buffer t)
    (setq org-status-buffer nil)))

(defhook org-auto-status-updates (write-file-functions)
  "If local variable `org-status-buffer`, run `org-status-updates' on it."
  (when org-status-buffer
    (org-status-updates))
  nil)
 )

(defhook set-org-status-buffer (org-mode-hook)
  (when (string-match "-status.org$" (or (buffer-file-name) ""))
    (defhook org-auto-status-updates (write-file-functions :local t)
      "If local variable `org-status-buffer`, run `org-status-updates' on it."
      (when org-status-buffer
        (org-status-updates))
      nil)))

(defun org-auto-status-updates-toggle (arg)
  "Toggle `org-status-buffer' in the local buffer.
If optional ARG is positive, set `org-status-buffer' to t, if
negative, set to nil."
  (interactive "p")
  (let ((a (when current-prefix-arg arg)))
    (cond ((null a)     (setq org-status-buffer (not org-status-buffer)))
          ((> a 0)      (setq org-status-buffer t))
          (t            (setq org-status-buffer nil))))
  (message "org-status-buffer is %s." org-status-buffer))

(provide 'org-status)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-status.el ends here
