;;; meta-view.el --- View metadata from .NET assemblies  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-24 14:01:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: View metadata from .NET assemblies
;; Keyword: assembly metadata source
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (csharp-mode "0.11.0") (meta-net "1.0.0"))
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

(require 'csharp-mode)
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

(defconst meta-view--buffer-name "* %s [from metadata] *"
  "Buffer name to display metadata.")

(defvar meta-view--buffer nil
  "Singleton, it records the displayed buffer.")

(defmacro meta-view--with-buffer (name &rest body)
  "Execute BODY inside the metadata displayed buffer with NAME."
  (declare (indent 0) (debug t))
  `(let ((buf-name (format meta-view--buffer-name name)))
     (with-current-buffer (get-buffer-create buf-name) (progn ,@body))))

(defun meta-view--kill-display-buffer ()
  "Kill the metadata display buffer."
  (when (buffer-live-p meta-view--buffer)
    (kill-buffer meta-view--buffer)
    (setq meta-view--buffer nil)))

;;;###autoload
(defun meta-view-at-point ()
  "View metadata at current point."
  (interactive)
  (meta-view (thing-at-point 'symbol)))

;;;###autoload
(defun meta-view (&optional name)
  "View "
  (interactive)
  (unless (memq major-mode meta-view-active-modes)
    (user-error "Invalid major-mode to view metadata, %s" major-mode))
  (unless (stringp name) (user-error "Invalid name to view metadata, %s" name))
  (meta-net-read-project)  ; read it
  (meta-view--kill-display-buffer)
  )

(provide 'meta-view)
;;; meta-view.el ends here
