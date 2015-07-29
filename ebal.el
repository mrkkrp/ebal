;;; ebal.el --- Emacs interface to Cabal -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/ebal
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (f "1.6") (ido-completing-read+ "3.6"))
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

;; This is Emacs interface to Cabal. Currently, it provides fast and easy
;; access to most Cabal commands:
;;
;; * cabal init
;; * cabal build
;; * cabal configure
;; * cabal sdist
;; * cabal bench
;; * cabal freeze
;; * cabal fetch
;; * cabal install
;; * cabal check
;; * cabal list
;; * cabal sandbox init
;; * cabal info
;; * cabal test
;; * cabal update
;; * cabal sandbox delete
;; * cabal clean

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'compile)
(require 'f)

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

(defvar ebal--pre-commands nil
  "List of command lines to execute before next Cabal command.")

(defvar ebal--post-commands nil
  "List of command lines to execute after next Cabal command.")

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

(defvar ebal--init-aborted nil
  "Whether current initialization has been aborted or not.")

(defcustom ebal-cabal-executable nil
  "Path to Cabal executable.

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

Names of commands are symbols and options are lists of strings.

Note that this is global collection of options.  If you want to
specify option to be used only with a specific command and in a
specific project, see `ebal-project-option-alist' and
corresponding setup instructions."
  :tag "Global Options for Ebal Commands"
  :type '(alist :key-type symbol
                :value-type
                (repeat :tag "Options" string)))

(defcustom ebal-project-option-alist nil
  "Alist that maps names of commands to their default options.

Names of commands are symbols and options are lists of strings.

This variable represents user's preferences for current project.
Value of the variable is read from \"*.ebal\" file that may be
present in project's root directory (the same directory that
contains \"*.cabal\" file).

Don't set this variable manually, instead create
\"project-name.ebal\" file and put desired value (a Lisp Object)
into it unquoted."
  :tag "Project Specific Options"
  :type '(alist :key-type symbol
                :value-type
                (repeat :tag "Options" string)))

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

The function is called with arguments like those that
`completing-read' takes."
  :tag "How to Select Command"
  :type '(radio (function-item ebal-command-completing-read)
                (function-item ebal-command-ido)
                (function-item ebal-command-popup)))

;;;###autoload
(defcustom ebal-popup-key-alist nil
  "Alist that maps names of commands to keys used in Ebal popup.

This is used by `ebal-command-popup'."
  :tag "Keys Used in Ebal Popup"
  :type '(alist :key-type character
                :value-type (symbol :tag "Command Name")))

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
                "^[[:blank:]]*name:[[:blank:]]+\\(\\w+\\)")))
    ;; project version
    (setq ebal--project-version
          (car (ebal--all-matches
                "^[[:blank:]]*version:[[:blank:]]+\\([[:digit:]\\.]+\\)")))
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
               "^[[:blank:]]*executable[[:blank:]]+\\(\\w+\\)"))
      ;; test suites
      (ebal--all-matches
       "^[[:blank:]]*test-suite[[:blank:]]+\\(\\w+\\)")
      ;; benchmarks
      (ebal--all-matches
       "^[[:blank:]]*benchmark[[:blank:]]+\\(\\w+\\)")))))

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

(defun ebal--cabal-executable ()
  "Return path to Cabal program is it's available, and NIL otherwise."
  (cond ((executable-find "cabal")
         "cabal")
        ((and ebal-cabal-executable
              (f-file? ebal-cabal-executable))
         ebal-cabal-executable)))

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
                    (f-glob "*.cabal" project-directory)))))
    (when cabal-file
      (let* ((ebal-pretender (f-swap-ext cabal-file "ebal"))
             (ebal-file
              (when (f-file? ebal-pretender)
                ebal-pretender)))
        (if (or (not ebal--last-directory)
                (not (f-same? ebal--last-directory
                              project-directory)))
            (progn
              ;; We are in different directory (or it's the first
              ;; invocation).  This means we should unconditionally parse
              ;; everything without checking of date of last modification.
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
                     (or (not ebal--ebal-mod-time)
                         (time-less-p ebal--ebal-mod-time
                                      (ebal--mod-time ebal-file))))
            (ebal--parse-ebal-file ebal-file)
            (setq ebal--ebal-mod-time (ebal--mod-time ebal-file)))
          t)))))

;; Low-level construction of individual commands and their execution via
;; `compile'.

(defun ebal--format-command (command &rest args)
  "Generate textual representation of command line.

COMMAND is the name of command and ARGS are arguments.  Result is
expected to be used as argument of `compile'."
  (mapconcat
   #'identity
   (append
    (list (shell-quote-argument (ebal--cabal-executable))
          command)
    (mapcar #'shell-quote-argument
            (remove nil args)))
   " "))

(defun ebal--register-command (kind command &rest args)
  "Register command to run before next call to Cabal.

KIND tells when to perform the command, meaningful values are:

before — execute the command before next Cabal command;

after — execute the command after next Cabal command.

All other values of KIND have effect of ‘before’.

COMMAND is the name of command and ARGS are arguments.  Return
the formatted command."
  (let ((cmd (apply #'ebal--format-command command args)))
    (if (eq kind 'after)
        (push cmd ebal--post-commands)
      (push cmd ebal--pre-commands))
    cmd))

(defun ebal--call-cabal (dir command &rest args)
  "Call Cabal as if from DIR performing COMMAND with arguments ARGS.

Arguments are quoted if necessary and NIL arguments are ignored.
This uses `compile' internally."
  (let ((default-directory dir)
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
      (append
       (reverse ebal--pre-commands)
       (list (apply #'ebal--format-command command args))
       (reverse ebal--post-commands))
      " && "))
    (setq ebal--pre-commands  nil
          ebal--post-commands nil)
    nil))

(defun ebal--perform-command (command &optional arg)
  "Perform Cabal command COMMAND.

This function should be called in “prepared” environment, where
`ebal--actual-command' is bound to name of executing command.

If argument ARG is given, it will quoted and added to command
line.

This is low-level operation, it doesn't run `ebal--prepare', thus
it cannot be used on its own by user."
  (run-hooks ebal-before-command-hook)
  (apply #'ebal--call-cabal
         ebal--last-directory
         command
         (append
          (cdr (assq ebal--actual-command
                     ebal-global-option-alist))
          (cdr (assq ebal--actual-command
                     ebal-project-option-alist))
          (when arg (list arg))))
  (run-hooks ebal-after-command-hook))

(defmacro ebal--define-command (name kbd global-options doc-string &rest body)
  "Define new Ebal command named NAME.

KBD is keybinding (a character) that is used by
`ebal-command-popup' function.

GLOBAL-OPTIONS should be list of strings that contains all the
command line options that should be always used with this
command.

DOC-STRING is description of the command, BODY is an implicit
PROGN.

Note that `ebal--actual-command' is let-bound to name of actual
command inside of BODY.  Also, inside the BODY, `non-direct-call'
is bound to truly value if this command is called directly by
user.  Some commands can check ARG variable that's bound to
argument when actual command is called as dependency."
  (declare (indent 3))
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
       (cl-pushnew (cons ,kbd ',name)
                   ebal-popup-key-alist
                   :key #'car)
       (cl-pushnew (cons ',name ',global-options)
                   ebal-global-option-alist
                   :key #'car))))

;;;###autoload
(defun ebal-execute (&optional command)
  "Perform cabal command COMMAND.

When called interactively or when COMMAND is NIL, propose to
choose command with `ebal-select-command-function'."
  (interactive)
  (if (ebal--cabal-executable)
      (if (ebal--prepare)
          (let* ((command
                  (or command
                      (intern
                       (funcall
                        ebal-select-command-function
                        "Choose command: "
                        (mapcar (lambda (x) (symbol-name (car x)))
                                ebal--command-alist)
                        nil
                        t))))
                 (fnc (cdr (assq command ebal--command-alist))))
            (when fnc
              (funcall fnc)))
        (message "Cannot locate ‘.cabal’ file."))
    (message "Cannot local Cabal executable on this system.")))

;; Definitions of all supported commands.

(ebal--define-command build ?b nil
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
             (list target)))))

(ebal--define-command configure ?c
                      ("--enable-tests"
                       "--enable-benchmarks")
  "Configure how package is built."
  (ebal--perform-command "configure"))

(ebal--define-command sdist ?d nil
  "Generate a source distribution file (.tag.gz)."
  (ebal--perform-command "sdist"))

(ebal--define-command bench ?e nil
  "Run all/specific benchmarks."
  ;; TODO Improve this so specific benchmarks can be run.
  (ebal--perform-command "bench"))

(ebal--define-command freeze ?f
                      ("--enable-tests"
                       "--enable-benchmarks")
  "Calculate a valid set of dependencies and their exact
versions.  If successful, save the result to the file
\"cabal.config\"."
  (ebal--perform-command "freeze"))

(ebal--define-command fetch ?g nil
  "Download packages for later installation."
  (let ((packages
         (or arg
             (funcall ebal-completing-read-function
                      "Packages to fetch: "
                      (ebal--installed-packages ebal--last-directory)
                      nil
                      t))))
    (ebal--perform-command "fetch" packages)))

(ebal--define-command haddock ?h nil
  "Generate Haddock HTML documentation.

Requires the program `haddock'."
  (ebal--perform-command "haddock"))

(ebal--define-command install ?i
                      ("--only-dependencies"
                       "--enable-tests"
                       "--enable-benchmarks")
  "Install necessary packages."
  (when (and (not (ebal--sandbox-exists-p ebal--last-directory))
             (ebal--implicit-sandbox-p))
    (ebal--register-command 'before "sandbox init"))
  (ebal--perform-command "install"))

(ebal--define-command check ?k nil
  "Check the package for common mistakes."
  (ebal--perform-command "check" nil))

(ebal--define-command list ?l
                      ("--installed"
                       "--simple-output")
  "List packages matching a search string."
  (ebal--perform-command "list" nil))

(ebal--define-command sandbox-init ?n nil
  "Initialize a sandbox in the current directory.  An existing
package database will not be modified, but settings (such as the
location of the database) can be modified this way."
  (if (ebal--sandbox-exists-p ebal--last-directory)
      (message "Sandbox already exists")
    (ebal--perform-command "sandbox init")))

(ebal--define-command info ?o nil
  "Display detailed information about a particular package."
  (let ((package
         (or arg
             (funcall ebal-completing-read-function
                      "Show info about package: "
                      (ebal--installed-packages ebal--last-directory)
                      nil
                      t))))
    (ebal--perform-command "info" package)))

(ebal--define-command test ?t nil
  "Run all/specific tests in the test suite."
  ;; TODO allow to run specific test components
  (ebal--perform-command "test"))

(ebal--define-command update ?u nil
  "Update list of known packages."
  (ebal--perform-command "update"))

(ebal--define-command sandbox-delete ?x nil
  "Remove the sandbox deleting all the packages installed inside."
  (if (ebal--sandbox-exists-p ebal--last-directory)
      (when (yes-or-no-p "Really delete the sandbox and all packages?")
        (ebal--perform-command "sandbox delete"))
    (message "There is no sandbox to delete")))

(ebal--define-command clean ?z nil
  "Clean up after a build."
  (ebal--perform-command "clean"))

;; User interface.

(defalias 'ebal-built-in-completing-read 'completing-read)
(defalias 'ebal-ido-completing-read      'ido-completing-read+)
(defalias 'ebal-command-completing-read  'completing-read)
(defalias 'ebal-command-ido              'ido-completing-read+)

;; Ebal command popup.

(defun ebal-command-popup
    (prompt collection &optional predicate
            _require-match _initial-input _hist _def _inherit-input-method)
  "Show a popup displaying PROMPT and COLLECTION of buttons.

PREDICATE is used to filter COLLECTION.  Other arguments are
taken for compatibility and have no effect."
  (let* ((collection (cl-remove-if-not predicate collection))
         (col (reverse
               (cl-remove-if-not
                (lambda (x)
                  (member (cdr x) collection))
                (mapcar
                 (lambda (x)
                   (cl-destructuring-bind (key . symbol) x
                     (cons key (symbol-name symbol))))
                 ebal-popup-key-alist))))
         (col-width (+ (cl-reduce
                        #'max
                        (mapcar
                         (lambda (x) (length (cdr x)))
                         col))
                       5))
         (i 0))
    (when collection
      (let ((buffer (get-buffer-create "*Cabal Commands*")))
        (with-current-buffer buffer
          (with-current-buffer-window
           ;; buffer or name
           buffer
           ;; action (for `display-buffer')
           (cons 'display-buffer-below-selected
                 '((window-height . fit-window-to-buffer)
                   (preserve-size . (nil . t))))
           ;; quit-function
           (lambda (window _value)
             (with-selected-window window
               (unwind-protect
                   (cdr
                    (assq
                     (read-char-choice
                      prompt
                      (mapcar #'car ebal-popup-key-alist))
                     col))
                 (when (window-live-p window)
                   (quit-restore-window window 'kill)))))
           ;; Here we generate the menu.
           (setq cursor-type nil)
           ;; print stuff from collection
           (insert
            (propertize (or ebal--project-name "Unknown")
                        'face 'font-lock-function-name-face))
           (when ebal--project-version
             (insert
              (propertize (format " %s" ebal--project-version)
                          'face 'font-lock-doc-face)))
           (insert "\n\n")
           (insert (propertize "Commands:\n" 'face 'bold))
           (dolist (item col)
             (cl-destructuring-bind (key . command) item
               (insert (propertize (string key)
                                   'face 'font-lock-keyword-face))
               (insert (format " %s" command)))
             (setq i (1+ i))
             (let ((j (mod i 4)))
               (if (zerop j)
                   (insert "\n")
                 (insert " \t")
                 (set-text-properties
                  (1- (point)) (point)
                  `(display (space :align-to ,(* j col-width)))))))))))))

;; Wizard that helps to create new Cabal projects.

(defun ebal--init-query (prompt &optional collection require-match)
  "Read users' input using `ebal-completing-read-function'.

PROMPT is the prompt to show and COLLECTION represents valid
choices.  If REQUIRE-MATCH is not NIL, don't let user input
something different from items in COLLECTION.

COLLECTION is allowed to be a string, in this case it's
automatically wrapped to make it one-element list.

If COLLECTION contains \"none\", and user selects it, interpret
it as NIL.  If user aborts entering of the input, return NIL and
most importantly set `ebal--init-aborted' to t.

Finally, if COLLECTION is nil, plain `read-string' is used.

If `ebal--init-aborted' is non-NIL, don't even try to read users'
input, immediately return NIL.  Thus, before reading of series of
inputs, `ebal--init-aborted' should be set to NIL."
  (unless ebal--init-aborted
    (let* ((collection
            (if (listp collection)
                collection
              (list collection)))
           (result
            (if collection
                (funcall ebal-completing-read-function
                         prompt
                         collection
                         nil
                         require-match)
              (read-string prompt))))
      (if result
          (unless (and (string= result "none")
                       (member result collection))
            result)
        (setq ebal--init-aborted t)
        nil))))

(defun ebal--form-arg (option value)
  "Return argument that supplies OPTION with VALUE."
  (when (and option value)
    (format "%s=%s" option value)))

;;;###autoload
(defun ebal-init ()
  "Create a .cabal, Setup.hs, and optionally a LICENSE file interactively."
  (interactive)
  (if (ebal--prepare)
      (message "The directory is already Cabalized, it seems")
    (run-hooks ebal-before-init-hook)
    (setq ebal--init-aborted nil)
    (let* ((ebal--project-name
            (ebal--init-query "Package name: " "cabal"))
           (ebal--project-version
            (ebal--init-query "Initial version: " "0.1.0"))
           (license
            (ebal--init-query
             "License: "
             '("none" "GPL-2" "GPL-3" "LGPL-2.1" "LGPL-3" "AGPL-3"
               "BSD2" "BSD3" "MIT" "ISC" "MPL-2.0" "Apache-2.0"
               "PublicDomain" "AllRightsReserved")
             t))
           (author (ebal--init-query "Author name: " user-full-name))
           (email (ebal--init-query "Maintainer email: " user-mail-address))
           (homepage (ebal--init-query "Project homepage URL: "))
           (synopsis (ebal--init-query "Synopsis: "))
           (category
            (ebal--init-query
             "Category: "
             '("none" "Codec" "Concurrency" "Control" "Data" "Database"
               "Development" "Distribution" "Game" "Graphics" "Language"
               "Math" "Network" "Sound" "System" "Testing" "Text" "Web")
             t))
           (type
            (ebal--init-query
             "What does the package build: "
             '("Library" "Executable")
             t))
           (main-is
            (when (string= type "Executable")
              (ebal--init-query
               "What is the main module of the executable: "
               '("Main.hs" "Main.lhs"))))
           (language
            (ebal--init-query
             "What base language is the package written in: "
             '("Haskell2010" "Haskell98")
             t))
           (source-dir
            (ebal--init-query
             "Source directory: "
             '("src" "none")))
           (include-comments
            (y-or-n-p "Include documentation on what each field means? ")))
      (unless ebal--init-aborted
        (when (ebal--implicit-sandbox-p)
          (ebal--register-command 'after "sandbox init"))
        (ebal--call-cabal
         default-directory
         "init"
         "--non-interactive"
         (ebal--form-arg "--package-name" ebal--project-name)
         (ebal--form-arg "--version"      ebal--project-version)
         (ebal--form-arg "--license"      license)
         (ebal--form-arg "--author"       author)
         (ebal--form-arg "--email"        email)
         (ebal--form-arg "--homepage"     homepage)
         (ebal--form-arg "--synopsis"     synopsis)
         (ebal--form-arg "--category"     category)
         (cl-case type
           ("Library"    "--is-library")
           ("Executable" "--is-executable"))
         (ebal--form-arg "--main-is"      main-is)
         (ebal--form-arg "--language"     language)
         (ebal--form-arg "--source-dir"   source-dir)
         (unless include-comments
           "--no-comments"))))
    (run-hooks ebal-after-init-hook)))

(provide 'ebal)

;;; ebal.el ends here
