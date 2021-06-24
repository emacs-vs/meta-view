;;; meta-view.el --- View metadata from .NET assemblies  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-24 14:01:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: View metadata from .NET assemblies
;; Keyword: assembly metadata source
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs-elpa/meta-view

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

(defgroup meta-view nil
  "View metadata from .NET assemblies."
  :prefix "meta-view-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/meta-view"))

(defconst meta-view--tag-property "P:"
  "Tag represent property declaration.")

(defconst meta-view--tag-method "M:"
  "Tag represent method/function declaration.")

(defconst meta-view--tag-type "T:"
  "Tag represent type declaration.")

(defconst meta-view--tag-enum "F:"
  "Tag represent enum item.")

(provide 'meta-view)
;;; meta-view.el ends here
