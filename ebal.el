;;; ebal.el --- Emacs interface to Cabal -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/ebal
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (f "1.6") (ido-completing-read+ "3.6"))
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
(require 'compile)
(require 'f)
(require 'mmt)

;; Settings & Variables

(defgroup ebal nil
  "Emacs interface to Cabal."
  :group 'programming
  :tag "Ebal"
  :prefix "ebal-")

(defvar ebal--command-alist nil
  "Alist that maps names of commands to functions that perform them.

This variable is modified by `ebal--define-command' when some
command is defined.  Do not modify this manually, unless you know
what you're doing.")

(defvar ebal--actual-command nil
  "Name of currently performed command (symbol) or NIL.

NIL value means that no command is performed right now.  This is
set by `ebal--perform-command' before running of “before” command
hooks and reset after running “after” command hook.

This variable is mainly useful when you want to add
‘ebal-before-command-hook’ or ‘ebal-after-command-hook’ and you
need to test which command is currently performed.")

(defvar ebal--last-directory nil
  "Path to project's directory last time `ebal--prepare' was called.

This is mainly used to check when we need to reload/re-parse
project-local settings that user might have.")

(defvar ebal--cabal-mod-time nil
  "Time of last modification of \"*.cabal\" file.

This is usually set by `ebal-prepare'.")

(defvar ebal--ebal-mod-time nil
  "Time of last modification of \"*.ebal\" file.

This is usually set by `ebal-prepare'.")

(defvar ebal--project-name nil
  "Name of current project extracted from \"*.cabal\" file.

This is usually set by `ebal--parse-cabal-file'.")

(defvar ebal--project-version nil
  "Version of current project extracted from \"*.cabal\" file.

This is usually set by `ebal--parse-cabal-file'.")

(defvar ebal--project-targets nil
  "List of build targets (strings) extracted from \"*.cabal\" file.

This is usually set by `ebal--parse-cabal-file'.")

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

;;;###autoload
(defcustom ebal-global-option-alist nil
  "Alist that maps names of commands to their default options.

Names of commands are symbols and options are strings.  If option
string is missing for some command or it's NIL, this results in
empty string.

Note that this is global collection of options.  If you want to
specify option to be used only with a specific command and in a
specific project, see `ebal-project-option-alist' and
corresponding setup instructions."
  :tag "Global Options for Ebal Commands"
  :type '(alist :key-type symbol
                :value-type (string :tag "Command Line Options")))

(defcustom ebal-project-option-alist nil
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

(defcustom ebal-completing-read-function #'ebal-built-in-completing-read
  "Function to be called when requesting input from the user."
  :tag "Completing Function"
  :type '(radio (function-item ebal-built-in-completing-read)
                (function-item ebal-ido-completing-read)))

(defcustom ebal-select-command-function #'ebal-command-popup
  "Function to call to select Ebal command.

This is what `ebal-execute' uses.  Default is Ebal custom popup
buffer, but you can use IDO-powered variant if you like or plain
`ebal-command-completing-read'.

The function is called with no arguments, it should return symbol
specifying chosen command."
  :tag "How to Select Command"
  :type '(radio (function-item ebal-command-completing-read)
                (function-item ebal-command-ido)
                (function-item ebal-command-popup)))

(defcustom ebal-before-init-hook nil
  "Hook to run before execution of `ebal-init' function."
  :tag "Before Init Hook"
  :type 'hook)

(defcustom ebal-after-init-hook nil
  "Hook to run after execution of `ebal-init' function."
  :tag "After Init Hook"
  :type 'hook)

(defcustom ebal-before-command-hook nil
  "Hook to run before execution of particular command.

You can check name of the command in `ebal--actual-command'."
  :tag "Before Command Hook"
  :type 'hook)

(defcustom ebal-after-command-hook nil
  "Hook to run after execution of particular command."
  :tag "After Command Hook"
  :type 'hook)

;; Various utilities.

(defun ebal--all-matches (regexp)
  "Return list of all stirngs matching REGEXP in current buffer."
  (let (matches)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (push (match-string-no-properties 1) matches))
    (reverse matches)))

(defun ebal--parse-cabal-file (filename)
  "Parse \"*.cabal\" file with name FILENAME and set some variables.

The following variables are set:

* `ebal--project-name'
* `ebal--project-version'
* `ebal--project-targets'

This is used by `ebal--prepare'."
  (with-temp-buffer
    (insert-file-contents filename)
    ;; project name
    (setq ebal--project-name
          (car (ebal--all-matches
                "^[[:blank:]]*name:[[:blank:]]*\\(\\w+\\)")))
    ;; project version
    (setq ebal--project-version
          (car (ebal--all-matches
                "^[[:blank:]]*version:[[:blank:]]*\\([[:digit:]\\.]+\\)")))
    ;; project targets
    (setq
     ebal--project-targets
     (append
      ;; library
      (mapcar (lambda (_) (format "lib:%s" ebal--project-name))
              (ebal--all-matches
               "^[[:blank:]]*library[[:blank:]]*"))
      ;; executable
      (mapcar (lambda (x) (format "exe:%s" x))
              (ebal--all-matches
               "^[[:blank:]]*executable[[:blank:]]*\\(\\w+\\)"))
      ;; test suites
      (ebal--all-matches
       "^[[:blank:]]*test-suite[[:blank:]]*\\(\\w+\\)")))))

(defun ebal--parse-ebal-file (filename)
  "Parse \"*.ebal\" file with name FILENAME and set some variables.

The following variable is set:

* `ebal--project-option-alist'

This is used by `ebal--prepare.'"
  (setq ebal-project-option-alist
        (with-temp-buffer
          (insert-file-contents filename)
          (read (buffer-string)))))

(defun ebal--find-dir-of-file (regexp)
  "Find file whose name satisfies REGEXP traversing upwards.

Return absolute path to directory containing that file or NIL on
failure.  Returned path is guaranteed to have trailing slash."
  (let ((dir (f-traverse-upwards
              (lambda (path)
                (directory-files path t regexp t))
              (f-full default-directory))))
    (when dir
      (f-slash dir))))

(defun ebal--mod-time (filename)
  "Return time of last modification of file FILENAME."
  (nth 5 (file-attributes filename 'integer)))

(defun ebal--cabal-available-p ()
  "Return non-NIL if location of Cabal executable known, and NIL otherwise."
  (or (executable-find "cabal")
      (and ebal-cabal-executable
           (f-file? ebal-cabal-executable))))

(defun ebal--sandbox-exists-p (dir)
  "Return non-NIL value if sandbox exists in DIR."
  (f-dir? (f-expand ".cabal-sandbox" dir)))

(defun ebal--implicit-sandbox-p ()
  "Return non-NIL value if sandbox should be implicitly created."
  (if (eql ebal-sandboxing 'ask)
      (yes-or-no-p "Create sandbox here?")
    ebal-sandboxing))

(defun ebal--installed-packages (dir)
  "Call Cabal as if from directory DIR and return list of installed packages.

This uses variation \"cabal list\" internally."
  (let ((default-directory dir))
    (with-temp-buffer
      (shell-command "cabal list --installed --simple-output"
                     (current-buffer))
      (ebal--all-matches "^\\(.+\\)[[:blank:]]"))))

;; Preparation.

(defun ebal--prepare ()
  "Locate, read, and parse configuration files and set various variables.

This commands searches for first \"*.cabal\" files traversing
directories upwards beginning with `default-directory'.  When
Cabal files is found, the following variables are set:

* `ebal--project-name'
* `ebal--project-version'
* `ebal--project-targets'

If \"*.ebal\" file is present, `ebal--project-option-alist' is
set.

At the end, `ebal--last-directory' is set.  Note that this
function is smart enough to not reparse all the stuff every time.
It can detect when we are in different project or when some files
have been changed since its last invocation.

Returned value is T on success and NIL on failure (when no
\"*.cabal\" files is found)."
  (let* ((project-directory
          (ebal--find-dir-of-file "^.+\.cabal$"))
         (cabal-file
          (car (and project-directory
                    (f-glob "*.cabal" project-directory))))
         (ebal-file
          (let ((ebal-pretender (f-swap-ext cabal-file "ebal")))
            (when (f-file? ebal-pretender)
              ebal-pretender))))
    (when cabal-file
      (if (or (not ebal--last-directory)
              (not (f-same? ebal--last-directory
                            project-directory)))
          (progn
            ;; We are in different directory (or it's the first invocation).
            ;; This means we should unconditionally parse everything without
            ;; checking of date of last modification.
            (ebal--parse-cabal-file cabal-file)
            (setq ebal--cabal-mod-time (ebal--mod-time cabal-file))
            (when ebal-file
              (ebal--parse-ebal-file ebal-file)
              (setq ebal--ebal-mod-time (ebal--mod-time ebal-file)))
            ;; Set last directory for future checks.
            (setq ebal--last-directory project-directory)
            t) ;; Return T on success.
        ;; We are in already visited directory, so we don't need to reset
        ;; `ebal--last-directory' this time. We need to reread/re-parse
        ;; *.cabal and *.ebal files if they have been modified though.
        (when (time-less-p ebal--cabal-mod-time
                           (ebal--mod-time cabal-file))
          (ebal--parse-cabal-file cabal-file)
          (setq ebal--cabal-mod-time (ebal--mod-time cabal-file)))
        (when (and ebal-file
                   (time-less-p ebal--ebal-mod-time
                                (ebal--mod-time ebal-file)))
          (ebal--parse-ebal-file ebal-file)
          (setq ebal--ebal-mod-time (ebal--mod-time ebal-file)))
        t))))

;; Low-level construction of individual commands and their execution via
;; `compile'.

(defun ebal--perform-command (command &optional arg)
  "Perform Cabal command COMMAND.

This function should be called in “prepared” environment, where
`ebal--actual-command' is bound to name of executing command.

If argument ARG is given, it will quoted and added to command
line.

This is low-level operation, it doesn't run `ebal--prepare', thus
it cannot be used on its own by user."
  (run-hooks ebal-before-command-hook)
  (let ((default-directory ebal--last-directory)
        (compilation-buffer-name-function
         (lambda (_major-mode)
           (format "*%s-cabal*"
                   (downcase
                    (replace-regexp-in-string
                     "[[:space:]]"
                     "-"
                     ebal--project-name))))))
    (compile
     (mapconcat
      #'identity
      (remove
       nil
       (list
        (shell-quote-argument (or ebal-cabal-executable "cabal"))
        command
        (cdr (assq ebal--actual-command ebal-global-option-alist))
        (cdr (assq ebal--actual-command ebal-project-option-alist))
        arg))
      " ")))
  (run-hooks ebal-after-command-hook))

(defmacro ebal--define-command (name global-options doc-string &rest body)
  "Define new Ebal command named NAME.

GLOBAL-OPTIONS should be a string (or NIL) that contains all the
command line options that should be always used with this
command.

DOC-STRING is description of the command, BODY is an implicit
PROGN.

Note that `ebal--actual-command' is let-bound to name of actual
command inside of BODY.  Also, inside the BODY, `non-direct-call'
is bound to truly value if this command is called directly by
user.  Some commands can check ARG variable that's bound to
argument when actual command is called as dependency."
  (declare (indent 2))
  (let ((function-name
         (intern (concat "ebal--command-"
                         (symbol-name name)))))
    `(progn
       (defun ,function-name (&optional arg)
         ,doc-string
         (ignore arg)
         (let ((ebal--actual-command ',name))
           ,@body))
       (push (cons ',name (function ,function-name))
             ebal--command-alist)
       (cl-pushnew (cons ',name ,global-options)
                   ebal-global-option-alist
                   :key #'car))))

;;;###autoload
(defun ebal-execute (command)
  "Perform cabal command COMMAND.

When called interactively, propose to choose command with
`ebal-select-command-function'."
  (interactive
   (list
    (intern
     (funcall
      ebal-select-command-function
      "Choose command: "
      (mapcar (lambda (x) (symbol-name (car x)))
              ebal--command-alist)
      nil
      t))))
  (when command
    (if (ebal--cabal-available-p)
        (if (ebal--prepare)
            (let ((fnc (cdr (assq command ebal--command-alist))))
              (when fnc
                (funcall fnc)))
          (message "Cannot locate ‘.cabal’ file."))
      (message "Cannot local Cabal executable on this system."))))

;; Definitions of all supported commands.

(ebal--define-command build nil
  "Build Cabal target ARG."
  (let ((target
         (or arg
             (funcall ebal-completing-read-function
                      "Build target: "
                      (cons "default" ebal--project-targets)
                      nil
                      t))))
    (apply #'ebal--perform-command
           "build"
           (unless (string= target "default")
             target))))

(ebal--define-command configure "--enable-tests --enable-benchmarks"
  "Configure how package is built."
  (ebal--perform-command "configure"))

(ebal--define-command sdist nil
  "Generate a source distribution file (.tag.gz)."
  (ebal--perform-command "sdist"))

(ebal--define-command bench nil
  "Run all/specific benchmarks."
  ;; TODO Improve this so specific benchmarks can be run.
  (ebal--perform-command "bench"))

(ebal--define-command freeze "--enable-tests --enable-benchmarks"
  "Calculate a valid set of dependencies and their exact
versions.  If successful, save the result to the file
\"cabal.config\"."
  (ebal--perform-command "freeze"))

(ebal--define-command fetch nil
  "Download packages for later installation."
  (let ((packages
         (or arg
             (funcall ebal-completing-read-function
                      "Packages to fetch: "))))
    (ebal--perform-command "fetch" packages)))

(ebal--define-command haddock nil
  "Generate Haddock HTML documentation.

Requires the program `haddock'."
  (ebal--perform-command "haddock"))

(ebal--define-command install
    "--only-dependencies --enable-tests --enable-benchmarks"
  "Install necessary packages."
  (if (and (not (ebal--sandbox-exists-p ebal--last-directory))
           (ebal--implicit-sandbox-p))
      (funcall (cdr (assq 'sandbox-init ebal--command-alist)))
    (ebal--perform-command "install")))

(ebal--define-command check nil
  "Check the package for common mistakes."
  (ebal--perform-command "check" nil))

(ebal--define-command list "--installed --simple-output"
  "List packages matching a search string."
  (ebal--perform-command "list" nil))

(ebal--define-command sandbox-init nil
  "Initialize a sandbox in the current directory.  An existing
package database will not be modified, but settings (such as the
location of the database) can be modified this way."
  (ebal--perform-command "sandbox init"))

(ebal--define-command info nil
  "Display detailed information about a particular package."
  (let ((package
         (or arg
             (funcall ebal-completing-read-function
                      "Show info about package: "
                      (ebal--installed-packages ebal--last-directory)
                      nil
                      t))))
    (ebal--perform-command "info" package)))

(ebal--define-command test nil
  "Run all/specific tests in the test suite."
  ;; TODO allow to run specific test components
  (ebal--perform-command "test"))

(ebal--define-command update nil
  "Update list of known packages."
  (ebal--perform-command "update"))

(ebal--define-command sandbox-delete nil
  "Remove the sandbox deleting all the packages installed inside."
  (when (yes-or-no-p "Really delete the sandbox and all packages?")
    (ebal--perform-command "sandbox delete")))

(ebal--define-command clean nil
  "Clean up after a build."
  (ebal--perform-command "clean"))

;; User interface.

(defalias 'ebal-built-in-completing-read 'completing-read)
(defalias 'ebal-ido-completing-read      'ido-completing-read+)
(defalias 'ebal-command-completing-read  'completing-read)
(defalias 'ebal-command-ido              'ido-completing-read+)
(defalias 'ebal-command-popup            'ebal-command-ido) ;; FIXME

;; NOTE ↑ We need to finish all this stuff, implement full functionality and
;; also refactor it a bit. This needs to be well-tested before we can
;; continue to popups and other nice (and easier) things.

;; TODO: UI — implement setup wizard in Emacs Lisp (`ebal-init')

(provide 'ebal)

;;; ebal.el ends here
