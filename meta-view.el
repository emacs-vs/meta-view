;;; meta-view.el --- View metadata from .NET assemblies  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-24 14:01:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: View metadata from .NET assemblies
;; Keyword: assembly metadata source
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (csharp-mode "0.11.0") (meta-net "1.0.0") (ht "2.3") (f "0.20.0"))
;; URL: https://github.com/emacs-vs/meta-view

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; View metadata from .NET assemblies.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'csharp-mode)
(require 'ht)
(require 'f)
(require 'meta-net)

(defgroup meta-view nil
  "View metadata from .NET assemblies."
  :prefix "meta-view-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/meta-view"))

(defcustom meta-view-active-modes
  '(csharp-mode csharp-tree-sitter-mode)
  "Major modes that allow to view metadata source."
  :type 'list
  :group 'meta-view)

(defconst meta-view--templates-dir
  (concat (file-name-directory load-file-name) "templates/")
  "Templates path for package `meta-view'.")

(defconst meta-view--buffer-name "* %s [from metadata] *"
  "Buffer name to display metadata.")

(defvar meta-view--buffer nil
  "Singleton, it records the displayed buffer.")

(defvar-local meta-view--xmls nil
  "Cache records a list of assembly xml file path.")

;;
;; (@* "Util" )
;;

(defmacro meta-view--with-buffer (name &rest body)
  "Execute BODY inside the metadata displayed buffer with NAME."
  (declare (indent 0) (debug t))
  `(let ((buf-name (format meta-view--buffer-name name)))
     (with-current-buffer (get-buffer-create buf-name)
       (setq meta-view--buffer (current-buffer))
       (csharp-mode)
       (let (buffer-read-only) (progn ,@body))
       (setq buffer-read-only t))))

(defun meta-view--kill-display-buffer ()
  "Kill the metadata display buffer."
  (when (buffer-live-p meta-view--buffer)
    (kill-buffer meta-view--buffer)
    (setq meta-view--buffer nil)))

;;
;; (@* "Xmls" )
;;

(defun meta-view--get-xmls ()
  "Return a list of assembly xml files.

We use these path as search index for variable `meta-net-xml'."
  (let (xmls)
    (dolist (path (meta-net-csporj-files))
      (setq xmls (append (meta-net-csproj-xmls path))))
    xmls))

(defun meta-view--all-xmls (&optional refresh)
  "Return full list of assembly xml files.

If REFRESH is non-nil, refresh cache once."
  (when (or refresh (null meta-view--xmls))
    (setq meta-view--xmls (meta-view--get-xmls))
    (cl-delete-duplicates meta-view--xmls))
  meta-view--xmls)

;;
;; (@* "Core" )
;;

(defun meta-view--match-namespaces (namespaces)
  "Return non-nil, if buffer matches these NAMESPACES.

Argument NAMESPACES is a list of string."
  (let ((match t) ns (index 0) (len (length namespaces)) last-item)
    (while (and match (< index len))
      (setq ns (nth index namespaces)
            last-item (= index (1- len))
            index (1+ index)
            match (string-match-p (format "\\_<%s\\_>%s" ns (if last-item "[.;]" "[.]"))
                                  (buffer-string))))
    match))

(defun meta-view--find-type ())

;;;###autoload
(defun meta-view-at-point ()
  "View metadata at current point."
  (interactive)
  (meta-view (thing-at-point 'symbol)))

;;;###autoload
(defun meta-view (name)
  "View metadata by NAME."
  (interactive)
  (unless (memq major-mode meta-view-active-modes)
    (user-error "Invalid major-mode to view metadata, %s" major-mode))
  (unless (stringp name) (user-error "Invalid name to view metadata, %s" name))
  (unless meta-net-csproj-current (meta-net-read-project))
  (meta-view--kill-display-buffer)
  (dolist (xml (meta-view--all-xmls))
    (when-let* ((base (f-base xml)) (namespaces (split-string base "\\."))
                (match (meta-view--match-namespaces namespaces))
                (data (meta-net-xml-data xml)))
      (jcs-print xml)
      (ignore-errors (jcs-log-list (ht-keys data)))
      )
    ))

(provide 'meta-view)
;;; meta-view.el ends here
