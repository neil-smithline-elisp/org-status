;;; org-status.el --- Post tweets from Org Mode.
;; 
;; Author: Neil Smithline
;; Maintainer: Neil Smithline
;; Copyright (C) 2012, Neil Smithline, all rights reserved.
;; Created: Sun May 27 09:24:41 2012 (-0400)
;; Version: 1.0-pre1
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: org-mode, twitter, tweets
;; Compatibility: Wherever Org Mode runs
;; 
;; Features that might be required by this library:
;;
;;   defhook, custom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: Post tweets from Org Mode.
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

(defcustom org-status-output-buffer "*Org Status Output*"
  "Status buffer from `org-status-update' commands.")

(defvar org-status-types '(tweet fb fbpage)
  "List of status types.")

(defun org-status-get-status-function (type)
  "Return the status update function for string TYPE."
  (symbol-function (intern (concat "org-status-" type))))

(defun org-status-get-status-search-tag (type)
  "Return the search string to search for string TYPE."
  (upcase type))))

(defun org-status-tweet ()
  "Do a tweet for the current headline."
  (beginning-of-line 1)
  (let* ((status-props      (org-agenda-get-some-entry-text
                             (point-marker) 9999))
         (status            (substring-no-properties status-props))
         (headline          (buffer-substring (point)
                                              (progn (end-of-line 1)
                                                     (point)))))
    (assert (<= (length status) 140) t)
    (let ((success (shell-command
                    (format "t update '%s'" 
                            (replace-regexp-in-string
                             "\\([^\\]\\)'" "\\1\\\\'" status))
                    org-status-output-buffer
                    org-status-output-buffer))
          (output            (save-excursion
                               (set-buffer org-status-output-buffer)
                               (buffer-substring-no-properties (point-min)
                                                               (point-max)))))
      (if (zerop success)
          (progn
            (org-todo 'done)
            output)
        (error "Status update (%s) failed with return value %s."
               headline success)))))

(defun org-status-updates ()
  "Loop through agenda files looking for status updates."
  (interactive)
  (let ((results (org-map-entries #'org-status-tweet
                                        "TWEET+TODO=\"POST\"")))
   (when results
     (with-output-to-temp-buffer org-status-output-buffer
      (set-buffer org-status-output-buffer)
      (print-elements-of-list results)))))

(defvar org-status-buffer nil
  "Should `org-status-updates` be run when saving this buffer.")

(make-variable-buffer-local 'org-status-buffer)
(setq-default org-status-buffer nil)

(defhook set-org-status-buffer (org-mode-hook)
  (if (string-match "-status.org$" (or (buffer-file-name) ""))
      (setq org-status-buffer t)
    (setq org-status-buffer nil)))

;; If you wish, you can rewrite this `defhook' definition as a call to
;; `add-hook'. Something like:
;;          (add-hook 'write-file-functions
;;              (when org-status-buffer (org-status-updates)))
(defhook org-auto-status-updates (write-file-functions)
  "If local variable `org-status-buffer`, run `org-status-updates' on it."
  (when org-status-buffer
    (org-status-updates)))

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



(org-set-tags-command)
(org-set-property)

info:org#Matching tags and properties (see URL `info:org#Matching%20tags%20and%20properties')