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
(require 's)

(require 'js-injector-lib)

;;; Group Definitions
(defvar js-injector-node-use-dev t
	"Whether to read the dev dependencies for requiring node modules.")
(defvar js-injector-node-executable (executable-find "node")
	"Executable path for `node`.")

(defcustom js-injector-node-lib-alist
	'(("ramda" . "R")
		("lodash" . "_")
		("react" . "React")
		("supertest" . "request")
		("q" . "Q"))
	"Alist of node libraries and their 'nice' require names."
	:group 'js-injector
	:type '(alist :key-type string :value-type string))

;;; Get definitions
;;  Functions to get various bits of information required for dependencies
(defun js-injector-node-get-node-modules (&optional get-dev-dependencies)
  "Get a list of node packages defined in `package.json`.
When called with GET-DEV-DEPENDENCIES, this function will return
a distinct list of both the dev and production dependencies."
  (let ((package-dir
				 (locate-dominating-file default-directory "package.json")))
		
		(unless package-dir
			(error "Could not find a package.json in the current project"))
		
		(let* ((json-object-type 'alist)
					 (json-alist (json-read-file (format "%s/package.json" package-dir))))
			
			(-distinct
			 (-flatten-n 1
				(--map
				 (-map 'car (cdr (assoc it json-alist)))
				 (append '(dependencies)
								 (when get-dev-dependencies '(devDependencies)))))))))

(defun js-injector-node-get-node-module-alist ()
  "Get a list of node modules associated with their nice names."
	(let ((node-modules (js-injector-node-get-node-modules)))
		(--map (cons it (list (symbol-name it))) node-modules)))

(defun js-injector-node--nice-name (name)
  "Return the nice node NAME defined in `js-injector-node-lib-alist`."
	(or (cdr (assoc name js-injector-node-lib-alist)) name))

(defun js-injector-node--var-decl? ()
  "Check whether you're in a variable declaration.
If you are, return the variable name currently being defined."
	(let* ((line (buffer-substring-no-properties
								(line-beginning-position) (line-end-position)))
				 (full-match (s-match "\\(var\\|let\\|const\\)\\s-+\\([a-z0-9_]*\\)" line))
				 (partial-match (s-match "\\(var\\|let\\|const\\)" line)))
		(cond
		 (full-match (caddr full-match))
		 (partial-match t))))

;;; Version calculation definitions
;;  Functions to calculate the version of node in use in a project

(defun js-injector-node-version>4 ()
  "Guess the node version being used in the project.

This will try to read it from the `package.json` engine field,
  otherwise fall back to reading from `.node-version`, or finally
  executing `node --version`."
	(let ((node-version (or (js-injector-node--package-version)
													(js-injector-node--dot-version)
													(js-injector-node--version))))
		
		(version<= "4" node-version)))

(defun js-injector-node--package-version ()
	"Read the node version from the `package.json` file."
  (let ((package-dir (locate-dominating-file default-directory "package.json")))
		(unless package-dir (error "Project does not contain a `package.json`"))
		(let* ((json-object-type 'alist)
					 (json-alist (json-read-file (format "%s/package.json" package-dir)))
					 (engines (cdr (assoc 'engines json-alist))))
			(js-injector--parse-version (cdr (assoc 'node engines))))))

(defun js-injector-node--dot-version ()
  "Read the node version from the `.node-version` file."
	(let ((node-version-dir (locate-dominating-file default-directory ".node-version")))
		(unless node-version-dir (error "Project does not contain a `.node-version`"))
		(with-temp-buffer
			(insert-file-contents (format "%s/.node-version" node-version-dir))
			(s-trim (s-collapse-whitespace (buffer-string))))))

(defun js-injector-node--version ()
  "Find the node version by running `node --version`."
	(unless js-injector-node-executable (error "You do not have `node` executable available"))
	(js-injector--parse-version
	 (shell-command-to-string (format "%s --version" js-injector-node-executable))))

;;; Navigation definitions
;;  Functions to navigate around the requirejs file

(defun js-injector-node--goto-first-import ()
  "Navigate to the first import/require in the file."
	(goto-char (point-min))
	(search-forward-regexp "['\"]use strict[\"'].*?[;]\\{0,1\\}" nil t)
	(skip-chars-forward " \n\t")
	(search-forward-regexp "require(\\|import" nil t)
	(beginning-of-line))
			
;;; Interactive Injector functions

(defun js-injector-node-import (module &optional prompt-name pos)
	"Import MODULE as a dependency relative to current file.
This function will look for MODULE in a dependency list relative
to the current file and add it as an import/require statement at
the top of the file.

If PROMPT-NAME is non-nil, this function will prompt the user for
the name they would like to import the module as.

If POS is non-nil, inject the dependency at position."
  (let* ((dependency-alist (append
														(js-injector-get-relative-dependency-alist)
														(ignore-errors (js-injector-node-get-node-module-alist))))
				 (dependency-match (assoc-string module dependency-alist t))
				 (dependencies (cdr dependency-match))
				 
				 (import-module (js-injector--read-dependencies module dependencies))
				 (import-name (when prompt-name (read-string "Import as: "))))

		(unless import-module
			(error "No module named '%s'" module))

		(js-injector-node--inject-module (file-name-sans-extension import-module)
										 (or import-name (js-injector-node--nice-name module))
										 pos)))

(defun js-injector-node--inject-module (module module-name &optional pos)
	"Inject MODULE into the node file as MODULE-NAME.
If POS is non-nil, goto position before injecting module."
	(if pos
			(progn (goto-char pos)
						 (delete-region (line-beginning-position) (1+ (line-end-position))))
		(js-injector-node--goto-first-import))
	(let ((qc (js-injector--get-quote-char)))
		(insert
		 (format
			(if (js-injector-node-version>4) "import %s from %s%s%s;\n" "var %s = require(%s%s%s);\n")
			module-name qc module qc))))

;;;###autoload
(defun js-injector-node-import-module-at-point (&optional pfx)
	"Import the module at point.
When called with a PFX argument, this will prompt the user for
what name they want to import the file as."
  (interactive "P")
	(let* ((module (word-at-point))
				 (var-decl (js-injector-node--var-decl?))
				 (pos (and var-decl (line-beginning-position))))
		
		(if (and var-decl (equal "" var-decl))
				(js-injector-node-import-module pfx)
			(save-excursion (js-injector-node-import module pfx pos)))))

;;;###autoload
(defun js-injector-node-import-module (&optional pfx)
	"Import a module in the project.
When called with a PFX argument, this will prompt the user for
what name they want to import the file as."
  (interactive "P")
	(let* ((modules (-map 'car (append
															(js-injector-get-relative-dependency-alist)
															(ignore-errors (js-injector-node-get-node-module-alist)))))
				 (module (completing-read "Import module: " modules))
				 (var-decl (js-injector-node--var-decl?))
				 (pos (and var-decl (line-beginning-position))))
		
			(save-excursion (js-injector-node-import module pfx pos))))

(provide 'js-injector-node)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (nameless-mode 1)
;; End:
;;; js-injector-node.el ends here
