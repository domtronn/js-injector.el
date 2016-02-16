;;; js-injector-lib.el --- Common library funtions for js-injector modules

;; Copyright (C) 2014  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: convenience, abbrev, tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module has functions common functions that are used between
;; the two modules of the js-injector

;;; Code:

;;; Utility definitions
(defun js-injector-replace-region (start end replacement)
  "Replace a region from START to END with REPLACEMENT string."
  (goto-char start)
  (delete-region start end)
  (insert replacement))

(defun js-injector--get-quote-char ()
  "Get the majority quote character used in a file."
  (if (> (count-matches "\"" (point-min) (point-max))
         (count-matches "'" (point-min) (point-max)))
      "\"" "'"))

(defun js-injector--parse-version (s)
  "Parse a semver version number S scheme seen in node package.json.
e.g.  `>=0.10.3 ~0.12` etc"
  (when s
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (search-forward-regexp ">\\|>=\\|^\\|~\\|v")
      (search-forward-regexp "\\([0-9]+\\(?:\.[0-9]+\\)+\\)")
      (match-string 0))))

(defun js-injector-relativise (files containing-dir)
  "Maps all file paths in FILES to be relative to CONTAINING-DIR."
  (cl-loop for file-alist in files
           for file = (car file-alist)
           for locations = (cdr file-alist)
    collect
		(cons file (--map (file-relative-name it containing-dir) locations))))

;;; 3rd Party helper functions
;; projectile
(defun js-injector--get-projectile-files-alist ()
  (--map (list (file-name-nondirectory it) (format "%s%s" (projectile-project-root) it))
         (projectile-current-project-files)))
(defun js-injector--get-projectile-relative-requirejs-alist ()
  (list (cons (projectile-project-name) (js-injector-relativise (js-injector--get-projectile-files-alist) (buffer-file-name)))))
(defun js-injector--get-projectile-relative-requirejs-config ()
  (list (cons (projectile-project-name) "//-projectile/?")))

;; jpop
(defun js-injector--get-jpop-files-alist () jpop-file-alist)
(defun js-injector--get-jpop-relative-requirejs-alist ()
  (list (cons jpop-id (js-injector-relativise jpop-file-alist (buffer-file-name)))))
(defun js-injector--get-jpop-requirejs-alist ()
  (let ((lib-ids (--map (plist-get it :id) (append (plist-get jpop-project-plist :libs) nil))))
    (--filter (-contains? lib-ids (car it)) jpop-project-alist)))
(defun js-injector--get-jpop-relative-requirejs-config () (list (cons jpop-id "//-jpop/?")))
(defun js-injector--get-jpop-requirejs-config ()
  (let ((configs (append (plist-get jpop-project-plist :libs) nil)))
    (mapcar (lambda (config) (cons (plist-get config :id) (plist-get config :dir))) configs)))

(provide 'js-injector-lib)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (nameless-mode 1)
;; eval: (flycheck-mode 0)
;; End:
;;; js-injector-lib.el ends here
