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

;; Settings & Variables

(defgroup ebal nil
  "Emacs interface to Cabal."
  :group 'programming
  :tag "Ebal"
  :prefix "ebal-")

(defvar ebal--command-alist nil
  "Alist that maps names of commands to Cabal commands.

This variable is modified by `ebal-define-command' when some
command is defined.")

(defvar ebal--active-command nil
  "Name of currently performed command (symbol) or NIL.

NIL value means that no command is performed right now.  This is
set by Ebal before running of “before” command hooks and reset
after running “after” command hook.

This variable is mainly useful when you want to add
‘ebal-before-command-hook’ or ‘ebal-after-command-hook’ and you
need to test which command is currently performed.")

(defvar ebal--last-directory nil
  "Path to project's directory last time `ebal-execute' was called.

This is mainly used to check when we need to reload/re-parse
project-local settings that user might have.")

(defcustom ebal-cabal-executable nil
  "Path to cabal executable.

If it's not NIL, this value is used in invocation of Cabal
commands instead of standard \"cabal\" string.  Set this variable
if your Cabal is in a strange place where OS cannot find it.

Note that the path is quoted with `shell-quote-argument' before
being used to compose command line."
  :tag "Path to Cabal Executable"
  :type '(choice (file :must-match t)
                 (const :tag "Use Default" nil)))

(defcustom ebal-global-options nil
  "Alist that maps names of commands to their default options.

Names of commands are symbols and options are strings.  If option
string is missing for some command or it's NIL, this results in
empty string.

Note that this is global collection of options.  If you want to
specify option to be used only with a specific command and in a
specific project, see `ebal-project-options' and corresponding
setup instructions."
  :tag "Global Options for Ebal Commands"
  :type '(alist :key-type symbol
                :value-type (string :tag "Command Line Options")))

(defcustom ebal-project-options nil
  "Alist that maps names of commands to their default options.

Names of commands are symbols and options are strings.  If option
string is missing for some command or it's NIL, this results in
empty string.

This variable represents user's preferences for current project.
Value of the variable is read from \"*.ebal\" file that may be
present in project's root directory (the same directory that
contains \"*.cabal\" file)."
  :tag "Project Specific Options"
  :type '(alist :key-type symbol
                :value-type (string :tag "Command Line Options")))

(defcustom ebal-command-dependency t
  "Whether or not perform dependencies of commands.

Since typical usage of Cabal involves a lot of boilerplate, Ebal
allows to perform some ritual actions for user, so he/she has no
chances to forget something.

NIL value of this variable tells Ebal be not so smart and do only
what's explicitly asked."
  :tag "Allow Command Dependencies"
  :type 'boolean)

(defcustom ebal-sandboxing 'ask
  "This determines Ebal's policy towards sandboxing.

The following values are recognized:

NIL — don't create sandboxes unless user explicitly runs command
to create one.

ask — ask if user wants to create a sandbox (so it's harder to
forget to create it), this is often preferable because most
Haskell developers want sandboxes everywhere nowadays (default).

always — create sandboxes silently when they are missing and they
should be created.  With this option every your project is
sandboxed without any effort on your side.

All other values of this variabe produce the same effect as
`always'."
  :tag "Sandboxing Policy"
  :type '(choice (const :tag "User creates sandboxes manually" nil)
                 (const :tag "Ask whether to create one" ask)
                 (const :tag "Silently create sandboxes" always)))

(defcustom ebal-bury-on-success nil
  "Wheter to bury compilation bury on success.

If this variable is bound to non-NIL value, restore window
configuration after successful execution of Ebal command."
  :tag "Bury *Compilation* buffer on success"
  :type 'boolean)

(defcustom ebal-completing-read-function #'ebal-built-in-completing-read
  "Function to be called when requesting input from the user."
  :tag "Completing Function"
  :type '(radio (function-item ebal-built-in-completing-read)
                (function-item ebal-ido-completing-read)))

(defcustom ebal-select-command-function #'ebal-command-popup
  "Function to call to select Ebal command.

This is what `ebal-execute' uses.  Default is Ebal custom popup
buffer, but you can use IDO-powered variant if you like or plain
`ebal-command-completing-read'."
  :tag "How to Select Command"
  :type '(radio (function-item ebal-command-popup)
                (function-item ebal-command-ido)
                (function-item ebal-command-completing-read)))

;; TODO: variables for all values that should be extracted:
;; * `ebal-project-name'
;; * `ebal-project-version'
;; * `ebal-project-targets'

;; TODO: variables for hooks

;; TODO: implement algorithm for locating and extracting all the data
;; without unnecessary reparsing (`ebal-prepare')

;; TODO: low-level construction of individual commands and their execution
;; via `compile' (variable to store functions to ask arguments)

;; TODO: write `ebal-define-command'

;; TODO: `ebal-execute'

;; TODO: write all the supported commands

;; TODO: UI — various versions of completing read, IDO (for arguments)

;; TODO: UI — various ways to select commands (including popup)

;; TODO: UI — implement setup wizard in Emacs Lisp (`ebal-init')

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
