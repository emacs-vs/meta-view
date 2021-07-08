;;; meta-view.el --- View metadata from .NET assemblies  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-24 14:01:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: View metadata from .NET assemblies
;; Keyword: assembly metadata source
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (csharp-mode "0.11.0") (meta-net "1.0.0") (ht "2.3") (f "0.20.0"))
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

(defconst meta-view--buffer-name "%s : [from metadata]"
  "Buffer name to display metadata.")

(defvar meta-view--buffers nil
  "List of buffers being view.")

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

(defun meta-view--inside-comment-or-string-p ()
  "Return non-nil if it's inside comment or string."
  (or (nth 4 (syntax-ppss)) (nth 8 (syntax-ppss))))

(defun meta-view--contain-list-string-regexp (in-list in-str)
  "Return non-nil if IN-STR is listed in IN-LIST.

This function uses `string-match-p'."
  (cl-some (lambda (elm) (string-match-p (regexp-quote elm) in-str)) in-list))

(defun meta-view--get-string-from-file (path)
  "Return PATH file content."
  (if (file-exists-p path)
      (with-temp-buffer (insert-file-contents path) (buffer-string))
    ""))

(defun meta-view--add-buffer (buffer)
  "Add BUFFER to view list."
  (push buffer meta-view--buffers)
  (cl-delete-duplicates meta-view--buffers)
  (setq meta-view--buffers
        (cl-remove-if-not (lambda (buf) (buffer-live-p buf)) meta-view--buffers)))

(defmacro meta-view--with-buffer (name &rest body)
  "Execute BODY inside the metadata displayed buffer with NAME."
  (declare (indent 1) (debug t))
  `(let ((buf-name (format meta-view--buffer-name name)))
     (with-current-buffer (get-buffer-create buf-name)
       (meta-view--add-buffer (current-buffer))
       (delay-mode-hooks (funcall 'csharp-mode))
       (ignore-errors (font-lock-ensure))
       (let (buffer-read-only) (progn ,@body))
       (setq buffer-read-only t))))

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

(defun meta-view--choose-template (declare-type)
  "Return the path of the template by DECLARE-TYPE."
  (f-join meta-view--templates-dir
          (cl-case declare-type
            (class "class.cs")
            (enum "enum.cs")
            (interface "interface.cs"))))

(defun meta-view--find-declare-type (xml type)
  "Find the declaration type.

See function `meta-net--type-data-get' for arguments XML and TYPE."
  (let* ((methods (meta-net-type-methods xml type))
         (fields (meta-net-type-fields xml type))
         (events (meta-net-type-events xml type))
         (properties (meta-net-type-properties xml type))
         (methods-len (length (ht-keys methods)))
         (fields-len (length (ht-keys fields)))
         (events-len (length (ht-keys events)))
         (properties-len (length (ht-keys properties)))
         (no-methods (zerop methods-len))
         (no-fields (zerop fields-len))
         (no-events (zerop events-len))
         (no-properties (zerop properties-len)))
    (cond ((and no-methods (not no-fields) no-events no-properties)
           ;; If fields is the only tag that appears then it has higher chance
           ;; to be an enumerator
           'enum)
          ((and (not no-methods) no-fields no-events no-properties)
           ;; Interface only contains methods
           'interface)
          ((and (not no-methods) (not no-properties))
           ;; Class can have everything
           'class)
          ;; In case, return unknown
          (t
           (user-error "Detect unknown declaration: %s %s" type
                       (list methods-len fields-len events-len properties-len))
           'unknown))))

(defun meta-view--matching-data (tag-data target)
  "Return non-nil, if TARGET can be found in TAG-DATA.

Argument TAG-DTA is in hash-table and it stores all data in tag categoray.

Tag are `methods`, `fields`, `events` and `properties`."
  (let ((names (ht-keys tag-data)))
    (meta-view--contain-list-string-regexp names target)))

(defun meta-view--find-matching (xml type target)
  "Return non-nil, if TARGET can be found xml data.

See function `meta-net--type-data-get' for arguments XML and TYPE."
  (cl-some (lambda (tag-data)
             (meta-view--matching-data tag-data target))
           (list (meta-net-type-methods xml type)
                 (meta-net-type-fields xml type)
                 (meta-net-type-events xml type)
                 (meta-net-type-properties xml type))))

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
              match (re-search-forward (format "%s[ \t\n]*[.;]" keyword) nil t))
        (when (meta-view--inside-comment-or-string-p)
          (setq match t))))
    (integerp match)))

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
  (when (meta-view--inside-comment-or-string-p)
    (user-error "View under comment or string is not allow"))
  (unless (stringp name) (user-error "Invalid name to view metadata, %s" name))
  (unless meta-net-csproj-current (meta-net-read-project))
  (let* ((xmls (meta-view--all-xmls))  ; Get the list of xml files from current project
         (xmls-len (length xmls))      ; length of the xmls
         (xml-index 0)                 ; index search through all `xmls`
         xml            ; current xml path as key
         break          ; flag to stop
         type           ; xml assembly type
         comp-name      ; name of the type, the last component from the type
         splits         ; temporary list to chop namespace, use to produce `comp-name`
         decalre-type   ; guess the declaration type
         template       ; chosen template path
         template-str)  ; template string, load it from `template`
    (while (and (not break) (< xml-index xmls-len))
      (setq xml (nth xml-index xmls)
            xml-index (1+ xml-index))
      (let* ((types (meta-net-xml-types xml))
             (types-len (length types))
             (type-index 0))
        (while (and (not break) (< type-index types-len))
          (setq type (nth type-index types)
                type-index (1+ type-index)
                splits (split-string type "\\.")
                comp-name (nth (1- (length splits)) splits))
          ;; Check if all namespaces exists in the buffer,
          (when (meta-view--match-name type)
            (meta-view-debug "\f")
            (meta-view-debug "%s" xml)
            (meta-view-debug "%s" type)
            (when (or (string= name comp-name)                   ; Viewing type data?
                      (meta-view--find-matching xml type name))  ; Viewing data under the type
              (meta-view-debug "found!")
              (setq break t
                    decalre-type (meta-view--find-declare-type xml type)
                    template (meta-view--choose-template decalre-type)
                    template-str (meta-view--get-string-from-file template))
              (meta-view--with-buffer comp-name
                (insert template-str)

                )
              ;; TODO: ..
              (jcs-print "Type:" comp-name)
              (jcs-print "Declare:" decalre-type)
              )))))))

(provide 'meta-view)
;;; meta-view.el ends here
