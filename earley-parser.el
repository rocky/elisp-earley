;;; early-parser.el --- An Earley algorithm pareser

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Version: 1.0.0
;; Package-Type: multi
;; Package-Requires: ((load-relative "1.2") (test-simple  "1.2.0") (cl-lib "0.5") (emacs "24"))
;; URL: http://github.com/rocky/elisp-earley
;; Keywords: Earley, parser

;; Copyright (C) 2018 Rocky Bernstein

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Jay Earley's context-free grammar parser for Emacs lisp
;;
;; Quick start: https://github.com/rocky/elisp-earley
;;

;;; Code:

;; Press C-x C-e at the end of the next line configure the program in
;; for building via "make" to get set up.
;; (compile (format "EMACSLOADPATH=:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc"))))

(require 'load-relative)

(defgroup earley nil
  "Earley Algorithm parser"
  :group 'processes
  :group 'tools
  :version "24.3")

;; FIXME: extend require-relative for "autoload".
(defun earley:load-features()
  (progn
    (require-relative-list
     '(
       "./earley-parser/parser"
       ) "earley-parser:")
    (earley:loaded-features)
    )
  )

(defun earley-feature-starts-with(feature prefix)
  "earley-strings-starts-with on stringified FEATURE and PREFIX."
  (declare (indent 1))
  (string-prefix-p (symbol-name feature) prefix)
  )

(defun earley:loaded-features()
  "Return a list of loaded debugger features. These are the features
that start with 'earley-' and 'earley:'"

  (delq nil
	(mapcar (lambda (x) (and (string-match-p "^\\(earley:\\|earley-\\)" (symbol-name x)) x))
		features)))

(defun earley:unload-features()
  "Remove all features loaded from this package. Used in
`earley:reload-features'. See that."
  (let ((removal-set (earley:loaded-features)))
	(dolist (feature removal-set)
	  (unload-feature feature t))
	removal-set)) ; return removed set

(defun earley:reload-features()
  "Reload all features loaded from this package. Useful if have
changed some code or want to reload another version, say a newer
development version and you already have this package loaded."
  (interactive "")
  (earley:unload-features)
  (earley:load-features)
  )

;; Load everything.
(earley:load-features)


;;; Autoloads-related code

;; This section is needed because package.el doesn't recurse into subdirectories
;; when looking for autoload-able forms.  As a workaround, we statically
;; generate our own autoloads, and force Emacs to read them by adding an extra
;; autoloded form.

;;;###autoload
(defconst earley--recursive-autoloads-file-name "earley-recursive-autoloads.el"
  "Where to store autoloads for subdirectory contents.")

;;;###autoload
(defconst earley--recursive-autoloads-base-directory
  (file-name-directory
   (if load-in-progress load-file-name
     buffer-file-name)))

;;;###autoload
(with-demoted-errors "Error in Earley's autoloads: %s"
  (load (expand-file-name earley--recursive-autoloads-file-name
                          earley--recursive-autoloads-base-directory)
        t t))

(defun earley--rebuild-recursive-autoloads ()
  "Update earley-parsers's recursive autoloads.
This is needed because the package.el infrastructure doesn't
process autoloads in subdirectories; instead we create an
additional autoloads file of our own, and we load it from an
autoloaded form.  Maintainers should run this after adding
autoloaded functions, and commit the resulting changes."
  (interactive)
  (let ((generated-autoload-file
         (expand-file-name earley--recursive-autoloads-file-name
                           earley--recursive-autoloads-base-directory)))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file))
    (dolist (name (with-no-warnings
                    (directory-files-recursively
                     earley--recursive-autoloads-base-directory "" t)))
      (when (file-directory-p name)
        (update-directory-autoloads name)))))

(provide-me)

;;; earley-parser.el ends here
