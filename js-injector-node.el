;;; js-injector-node.el --- Inject paths to JS classes for node projects

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

;; This module has functions specific to dependency injection/requiring files for node

;;; Code:

(require 'dash)

;;; Group Definitions
(defvar js-inject-use-dev-dependencies nil)
(defcustom js-injector-node-lib-alist
	'(("ramda" . "R")
		("lodash" . "_")
		("supertest" . "request")
		("q" . "Q"))
	"Alist of node libraries and their 'nice' require names."
	:group 'js-injector
	:type '(alist :key-type string :value-type string))

;; Main function definitions
(defun get-node-modules (&optional get-dev-dependencies)
  "Get a list of node packages defined in package.json."
  (let ((package-json (locate-dominating-file
                       (file-name-directory (buffer-file-name))
                       "package.json")))
    (when package-json
      (let* ((json-object-type 'hash-table)
             (json-contents (with-temp-buffer
                              (insert-file-contents
                               (format "%s/package.json" package-json))
                              (buffer-string)))
             (json-hash (json-read-from-string json-contents))
             (result (list)))
        (mapc
         (lambda (arg)
           (maphash
            (lambda (key value) (setq result (-distinct (append result (list key)))))
            (gethash arg json-hash)))
         (-non-nil (list
                    "dependencies"
                    (when get-dev-dependencies "devDependencies"))))
        result))))

(defun require-node-module ()
	"Get node modules based on `js-injector-use-dev-dependncies`."
	(interactive)
  (require--node-module js-inject-use-dev-dependencies))

(defun require-node-module-dev ()
	"Get node modules override for dev dependencies."
	(interactive)
  (require--node-module t))

(defun require--node-module (&optional get-dev-dependencies)
  "Require a node modules defined in package.json at point.
This will search up from the current directory to find the package.json and
pull out the dependencies - by default this will just use DEPENDENCIES, but can
also use DEVDEPENDENCIES when GET-DEV-DEPENDENCIES is present
- and then prompt the user for the module they want
to include."
  (save-excursion
    (let* ((popup-point (point))
           (node-modules (get-node-modules get-dev-dependencies)))
      (if node-modules
          (let ((result (completing-read "Require Node Module:" node-modules))
                (qc (get-quote-char)))
            (list (nice-node-name result)
									(format "%s%s%s" qc result qc)))
        (message "No node modules found in current project")))))

(defun require-relative-module ()
  "Require a module relative to the current file from a project."
  (interactive)
  (let* ((qc (get-quote-char))
         (modules (--filter (string-match "\.js[on]\\{0,2\\}$" (cadr it))
                            (--map (cons (file-name-sans-extension (car it)) (cdr it)) projectable-file-alist)))
         (module (or m (completing-read "Require: " modules)))
         (relative-modules (--map (file-relative-name it (file-name-directory (buffer-file-name))) (cdr (assoc module modules))))
         (relative-module (if (> (length relative-modules) 1)
                              (ido-completing-read "Module: " relative-modules)
                            (car relative-modules)))
         (result (file-name-sans-extension (if (string-match "^[a-zA-Z]" relative-module) (concat "./" relative-module) relative-module))))
    (list (sanitise module)
					(format "%s%s%s" qc result qc ))))

(defun nice-node-name (package)
  "Return the sanitised nice node name for PACKAGE."
	(sanitise (if (assoc package js-injector-node-lib-alist)
								(cdr (assoc package js-injector-node-lib-alist))
							package)))

(defun sanitise (s)
  "Return a sanitised string S."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward "-\\(.\\)" nil t)
      (replace-match (capitalize (match-string 1))))
    (buffer-string)))

(defun get-quote-char ()
  "Get the majority quote character used in a file."
  (if (> (count-matches "\"" (point-min) (point-max))
         (count-matches "'" (point-min) (point-max)))
      "\"" "'"))

(provide 'js-injector-node)
;;; js-injector-node.el ends here
