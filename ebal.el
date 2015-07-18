;;; ebal.el --- Emacs interface to Cabal -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/ebal
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (f "1.6") (mmt "0.1.0"))
;; Keywords: convenience, cabal, haskell
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is Emacs interface to Cabal that eliminates boilerplate.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'mmt)

;; TODO: this is just a batch of hacks, write great package now

(defvar ebal-cabal-operations
  '((build          . "cabal build")
    (check          . "cabal check")
    (clean          . "cabal clean")
    (configure      . "cabal configure --enable-tests --enable-benchmarks")
    (init           . "cabal init")
    (install        . "cabal update ; \
cabal install --only-dependencies --enable-tests --enable-benchmarks")
    (run            . "cabal run")
    (sandbox-delete . "cabal sandbox delete")
    (sandbox-init   . "cabal sandbox init")
    (sdist          . "cabal sdist")
    (test           . "cabal test --test-option=\"--maximum-test-size=50\""))
  "Collection of operations supported by `ebal-cabal-action'.")

(defun ebal-find-file (regexp)
  "Find file whose name satisfies REGEXP traversing upwards.
Return absolute path to directory containing that file or NIL on
failure."
  (f-traverse-upwards
   (lambda (path)
     (directory-files path t regexp t))
   (expand-file-name default-directory)))

(defun ebal-cabal-action (command)
  "Perform a Cabal command COMMAND.
COMMAND can be one of the operations listed in
`ebal-cabal-operations'.  Completing read is used if the command is
called interactively."
  (interactive
   (list
    (intern
     (completing-read
      "Cabal operation: "
      (mapcar (lambda (x) (symbol-name (car x)))
              ebal-cabal-operations)
      nil
      t))))
  (let ((dir (ebal-find-file "\\`.+\\.cabal\\'")))
    (if dir
        (compile
         (format "cd %s ; %s"
                 dir
                 (cdr (assoc command ebal-cabal-operations))))
      (message "Please create ‘.cabal’ file for the project."))))

(provide 'ebal)

;;; ebal.el ends here
