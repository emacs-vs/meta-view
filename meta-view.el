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

(defvar meta-view-show-debug nil
  "Show the debug message from this package.")

;;
;; (@* "Util" )
;;

(defun meta-view-debug (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when meta-view-show-debug (apply 'message fmt args)))

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
    (dolist (path (meta-net-csproj-files))
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

(defun meta-view--match-name (name)
  "Return non-nil, if NAME exists inside the buffer.

The name should similar to namepsace syntax, `System.Collections.UI`, etc."
  (let* ((names (split-string name "\\."))
         (match t) keyword (index 0) (len (length names)))
    (save-excursion
      ;; Incremental search from the start of the buffer to eliminate
      ;; some of the possible candidates.
      (goto-char (point-min))
      (while (and match (< index len))
        (setq keyword (nth index names)
              index (1+ index)
              match (re-search-forward (format "%s[ \t\n]*[.;]" keyword) nil t))))
    match))

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
    (when-let* ((base (f-base xml))
                ;; We first compare namespaces
                (_match (meta-view--match-name base))
                (types (meta-net-xml-types xml)))
      (meta-view-debug "\f")
      (meta-view-debug "%s" xml)
      (dolist (type types)
        ;; Then we simply compare the type
        (when-let ((_match (meta-view--match-name type))
                   (methods (meta-net-type-methods xml type))
                   (fields (meta-net-type-fields xml type))
                   (properties (meta-net-type-properties xml type)))
          (jcs-print "Type:" type)
          ;;(jcs-log-list (ht-keys methods))
          (ignore-errors (jcs-log-list (ht-keys fields)))
          (ignore-errors (jcs-log-list (ht-keys properties)))
          )
        )
      )
    ))

(provide 'meta-view)
;;; meta-view.el ends here
