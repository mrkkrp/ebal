;;; ebal.el --- Emacs interface to Cabal and Stack -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2017 Mark Karpov <markkarpov92@gmail.com>
;;
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; URL: https://github.com/mrkkrp/ebal
;; Version: 0.3.1
;; Package-Requires: ((emacs "24.4") (f "0.18.0"))
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

;; This is an Emacs interface to Cabal and Stack.  Currently, it provides
;; fast and easy access to most Cabal commands (†—commands available in
;; Stack mode):
;;
;; * init (in stack mode this is done for you automatically)
;; * build †
;; * configure
;; * sdist †
;; * bench †
;; * freeze
;; * fetch
;; * haddock †
;; * install †
;; * check
;; * list
;; * sandbox init
;; * info
;; * test †
;; * update †
;; * sandbox delete
;; * clean †

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings & Variables

(defgroup ebal nil
  "Emacs interface to Cabal and Stack."
  :group  'programming
  :tag    "Ebal"
  :prefix "ebal-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/ebal"))

(defface ebal-project-name
  '((t (:inherit font-lock-function-name-face)))
  "Face used to display name of current project.")

(defface ebal-project-version
  '((t (:inherit font-lock-doc-face)))
  "Face used to display version of current project.")

(defface ebal-header
  '((t (:inherit bold)))
  "Face used to display “Commands:” header.")

(defface ebal-key
  '((t (:inherit font-lock-keyword-face)))
  "Face used to display key bindings for commands.")

(defvar ebal--cabal-command-alist nil
  "Alist that maps names of commands to functions that perform them.

This variable is modified by `ebal--define-command' when some
command is defined.  Do not modify this manually, unless you know
what you're doing.")

(defvar ebal--stack-command-alist nil
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
  "List of command lines to execute before next command.")

(defvar ebal--post-commands nil
  "List of command lines to execute after next command.")

(defvar ebal--last-directory nil
  "Path to project's directory last time `ebal--prepare' was called.

This is mainly used to check when we need to reload/re-parse
project-local settings that user might have.")

(defvar ebal--cabal-mod-time nil
  "Time of last modification of \"*.cabal\" file.

This is usually set by `ebal--prepare'.")

(defvar ebal--ebal-mod-time nil
  "Time of last modification of \"*.ebal\" file.

This is usually set by `ebal--prepare'.")

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

(defvar ebal--init-template-selected nil
  "Whether template was selected during initialization.")

(defcustom ebal-operation-mode 'cabal
  "Mode of operation for Ebal package.

The following values are recognized:

  cabal—Ebal works as interface for Cabal
  stack—Ebal works as interface for Stack

All other values of this variable produce the same effect as
`cabal'."
  :tag "Mode of Operation"
  :type '(choice (const :tag "Interface to Cabal" cabal)
                 (const :tag "Interface to Stack" stack)))

(defcustom ebal-cabal-executable nil
  "Path to Cabal executable.

If it's not NIL, this value is used in invocation of Cabal
commands instead of the standard \"cabal\" string.  Set this
variable if your Cabal is in a strange place where OS cannot find
it.

Note that the path is quoted with `shell-quote-argument' before
being used to compose command line."
  :tag "Path to Cabal Executable"
  :type '(choice (file :must-match t)
                 (const :tag "Use Default" nil)))

(defcustom ebal-stack-executable nil
  "Path to Stack executable.

If it's not NIL, this value is used in invocation of Stack
commands instead of the standard \"stack\" string.  Set this
variable if your Stack is in a strange place where OS cannot find
it.

Note that the path is quoted with `shell-quote-argument' before
being used to compose command line."
  :tag "Path to Stack Executable"
  :type '(choice (file :must-match t)
                 (const :tag "Use Default" nil)))

;;;###autoload
(defcustom ebal-global-option-alist nil
  "Alist that maps names of commands to their default options.

Names of commands are symbols and options are lists of strings.

Note that this is a global collection of options.  If you want to
specify an option to be used only with a specific command and in
a specific project, see `ebal-project-option-alist' and
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

The following values are recognized (Cabal mode only):

  NIL—don't create sandboxes unless user explicitly runs command
  to create one.

  ask—ask if user wants to create a sandbox (so it's harder to
  forget to create it), this is often preferable because most
  Haskell developers want sandboxes everywhere
  nowadays (default).

  always—create sandboxes silently when they are missing and they
  should be created.  With this option every your project is
  sandboxed without any effort on your side.

All other values of this variable produce the same effect as
`always'."
  :tag "Sandboxing Policy"
  :type '(choice (const :tag "User creates sandboxes manually" nil)
                 (const :tag "Ask whether to create one" ask)
                 (const :tag "Silently create sandboxes" always)))

(defcustom ebal-completing-read-function #'ebal-built-in-completing-read
  "Function to be called when requesting input from the user."
  :tag "Completing Function"
  :type '(radio (function-item ebal-built-in-completing-read)))

(defcustom ebal-select-command-function #'ebal-command-popup
  "Function to call to select Ebal command.

This is what `ebal-execute' uses.  Default is Ebal custom popup
buffer, but you can use plain `ebal-command-completing-read' if
you like.

The function is called with arguments like those that
`completing-read' takes."
  :tag "How to Select Command"
  :type '(radio (function-item ebal-command-completing-read)
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

Name of the command is available in `ebal--actual-command'."
  :tag "Before Command Hook"
  :type 'hook)

(defcustom ebal-after-command-hook nil
  "Hook to run after execution of particular command.

Name of the command is available in `ebal--actual-command'."
  :tag "After Command Hook"
  :type 'hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various utilities

(defun ebal--all-matches (regexp)
  "Return list of all strings matching REGEXP in current buffer."
  (let (matches)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (push (match-string-no-properties 1) matches))
    (reverse matches)))

(defun ebal--parse-cabal-file (filename)
  "Parse \"*.cabal\" file with name FILENAME and set some variables.

The following variables are set:

  `ebal--project-name'
  `ebal--project-version'
  `ebal--project-targets'

This is used by `ebal--prepare'."
  (with-temp-buffer
    (insert-file-contents filename)
    ;; project name
    (setq ebal--project-name
          (car (ebal--all-matches
                "^[[:blank:]]*name:[[:blank:]]+\\([[:word:]-]+\\)")))
    ;; project version
    (setq ebal--project-version
          (car (ebal--all-matches
                "^[[:blank:]]*version:[[:blank:]]+\\([[:digit:]\\.]+\\)")))
    ;; project targets
    (setq
     ebal--project-targets
     (append
      ;; library
      (mapcar (lambda (_) (format "%s:lib" ebal--project-name))
              (ebal--all-matches
               "^[[:blank:]]*library[[:blank:]]*"))
      ;; executables
      (mapcar (lambda (x) (format "%s:exe:%s" ebal--project-name x))
              (ebal--all-matches
               "^[[:blank:]]*executable[[:blank:]]+\\([[:word:]-]+\\)"))
      ;; test suites
      (mapcar (lambda (x) (format "%s:test:%s" ebal--project-name x))
              (ebal--all-matches
               "^[[:blank:]]*test-suite[[:blank:]]+\\([[:word:]-]+\\)"))
      ;; benchmarks
      (mapcar (lambda (x) (format "%s:bench:%s" ebal--project-name x))
              (ebal--all-matches
               "^[[:blank:]]*benchmark[[:blank:]]+\\([[:word:]-]+\\)"))))))

(defun ebal--parse-ebal-file (filename)
  "Parse \"*.ebal\" file with name FILENAME and set some variables.

The following variable is set:

  `ebal-project-option-alist'

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

(defun ebal--cabal-mode-p ()
  "Return non-NIL value if current mode is Cabal mode."
  (not (eq ebal-operation-mode 'stack)))

(defun ebal--stack-mode-p ()
  "Return non-NIL value if current mode is Stack mode."
  (eq ebal-operation-mode 'stack))

(defun ebal--mod-time (filename)
  "Return time of last modification of file FILENAME."
  (nth 5 (file-attributes filename 'integer)))

(defun ebal--target-executable ()
  "Return path to target program if it's available, and NIL otherwise."
  (cl-destructuring-bind (default . custom)
      (if (ebal--cabal-mode-p)
          (cons "cabal" ebal-cabal-executable)
        (cons "stack" ebal-stack-executable))
    (cond ((executable-find default)
           default)
          ((and custom (f-file? custom))
           custom))))

(defun ebal--sandbox-exists-p (dir)
  "Return non-NIL value if sandbox exists in DIR."
  (or (f-file? (f-expand "cabal.config"         dir))
      (f-file? (f-expand "cabal.sandbox.config" dir))))

(defun ebal--stack-initialized-p (dir)
  "Return non-NIL value if sandbox exists in DIR."
  (f-file? (f-expand "stack.yaml" dir)))

(defun ebal--implicit-sandbox-p ()
  "Return non-NIL value if sandbox should be implicitly created."
  (if (eql ebal-sandboxing 'ask)
      (yes-or-no-p "Create sandbox here?")
    ebal-sandboxing))

(defun ebal--installed-packages (dir)
  "Call Cabal as if from directory DIR and return list of installed packages.

This uses variation of \"cabal list\" internally."
  (let ((default-directory dir))
    (with-temp-buffer
      (shell-command
       (format "%s list --installed --simple-output"
               (ebal--target-executable))
       (current-buffer))
      (ebal--all-matches "^\\(.+\\)[[:blank:]]*$"))))

(defun ebal--stack-templates ()
  "Return list of available Stack templates.

If `ebal--operation-mode' is not stack, return NIL."
  (when (eq ebal-operation-mode 'stack)
    (with-temp-buffer
      (shell-command
       (format "%s templates"
               (ebal--target-executable))
       (current-buffer))
      (remove "Template"
              (ebal--all-matches "^\\(\\([[:alnum:]]\\|-\\)+\\)")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preparation

(defun ebal--prepare ()
  "Locate, read, and parse configuration files and set various variables.

This commands searches for first \"*.cabal\" files traversing
directories upwards beginning with `default-directory'.  When
Cabal file is found, the following variables are set:

  `ebal--project-name'
  `ebal--project-version'
  `ebal--project-targets'

If \"*.ebal\" file is present, `ebal-project-option-alist' is
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
              ;; invocation). This means we should unconditionally parse
              ;; everything without checking of date of last modification.
              (ebal--parse-cabal-file cabal-file)
              (setq ebal--cabal-mod-time (ebal--mod-time cabal-file))
              (if (not ebal-file)
                  (setq ebal-project-option-alist nil)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level construction of individual commands

(defun ebal--format-command (command &rest args)
  "Generate textual representation of command line.

COMMAND is the name of command and ARGS are arguments.  Result is
expected to be used as argument of `compile'."
  (mapconcat
   #'identity
   (append
    (list (shell-quote-argument (ebal--target-executable))
          command)
    (mapcar #'shell-quote-argument
            (remove nil args)))
   " "))

(defun ebal--register-command (kind command &rest args)
  "Register command to run before next call of target program.

KIND tells when to perform the command, meaningful values are:

  before—execute the command before next command;
  after—execute the command after next command.

All other values of KIND have effect of ‘before’.

COMMAND is the name of command and ARGS are arguments.  Return
the formatted command."
  (let ((cmd (apply #'ebal--format-command command args)))
    (if (eq kind 'after)
        (push cmd ebal--post-commands)
      (push cmd ebal--pre-commands))
    cmd))

(defun ebal--ensure-sandbox-exists (dir &optional when)
  "Ensure that Cabal sandbox exists in DIR.

This means that if sandbox is missing in DIR, special command
will be registered to create it.  This function respects settings
regarding sandboxing.

Argument WHEN controls when to run the registered command, before
or after the next command.  Recognized values are:

  before (default)—execute the command before next command;
  after—execute the command after next command.

This only works when the package is in Cabal mode."
  (when (and (ebal--cabal-mode-p)
             (not (ebal--sandbox-exists-p dir))
             (ebal--implicit-sandbox-p))
    (ebal--register-command (or when 'before) "sandbox init")))

(defun ebal--ensure-stack-init (dir &optional when)
  "Ensure that stack is initialized for project in DIR.

This means that if \"stack.yaml\" is missing in DIR, special
command will be registered to create it.

Argument WHEN controls when to run the registered command, before
or after the next command.  Recognized values are:

  before (default)—execute the command before next command;
  after—execute the command after next command.

This only works when the package is in Stack mode."
  (when (and (ebal--stack-mode-p)
             (not (ebal--stack-initialized-p dir)))
    (ebal--register-command (or when 'before) "init")))

(defun ebal--call-target (dir command &rest args)
  "Call target as if from DIR performing COMMAND with arguments ARGS.

Arguments are quoted if necessary and NIL arguments are ignored.
This uses `compile' internally."
  (let ((default-directory dir)
        (compilation-buffer-name-function
         (lambda (_major-mode)
           (format "*%s-%s*"
                   (downcase
                    (replace-regexp-in-string
                     "[[:space:]]"
                     "-"
                     ebal--project-name))
                   (if (ebal--cabal-mode-p) "cabal" "stack")))))
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

(defun ebal--perform-command (command &rest args)
  "Perform target command COMMAND.

This function should be called in “prepared” environment, where
`ebal--actual-command' is bound to name of executing command.

If argument ARGS is given, its elements will be quoted and added
to command line.

This is low-level operation, it doesn't run `ebal--prepare', thus
it cannot be used on its own by user."
  (run-hooks ebal-before-command-hook)
  (apply #'ebal--call-target
         ebal--last-directory
         command
         (append
          (cdr (assq ebal--actual-command
                     ebal-global-option-alist))
          (cdr (assq ebal--actual-command
                     ebal-project-option-alist))
          args))
  (run-hooks ebal-after-command-hook))

(defmacro ebal--define-command (name kbd mode &rest body)
  "Define new Ebal command named NAME.

KBD is keybinding (a character) that is used by
`ebal-command-popup' function.

MODE specifies modes in which the command will be available:

  cabal—only in Cabal mode
  stack—only in Stack mode
  both—in both Cabal and Stack mode

BODY is an implicit PROGN.

Note that `ebal--actual-command' is let-bound to name of actual
command inside of BODY.  Some commands can check ARG variable
that's bound to argument when actual command is called as
dependency."
  (declare (indent 3))
  `(let ((func
          (lambda (&optional arg)
            (ignore arg)
            (let ((ebal--actual-command ',name))
              ,@body))))
     (when (memq (quote ,mode) '(cabal both))
       (push (cons ',name func)
             ebal--cabal-command-alist))
     (when (memq (quote ,mode) '(stack both))
       (push (cons ',name func)
             ebal--stack-command-alist))
     (cl-pushnew (cons ,kbd ',name)
                 ebal-popup-key-alist
                 :key #'car)))

;;;###autoload
(defun ebal-execute (&optional command)
  "Perform cabal command COMMAND.

When called interactively or when COMMAND is NIL, propose to
choose command with `ebal-select-command-function'."
  (interactive)
  (if (ebal--target-executable)
      (if (ebal--prepare)
          (let* ((command-alist
                  (if (ebal--cabal-mode-p)
                      ebal--cabal-command-alist
                    ebal--stack-command-alist))
                 (command
                  (or command
                      (intern
                       (funcall
                        ebal-select-command-function
                        "Choose command: "
                        (mapcar (lambda (x) (symbol-name (car x)))
                                command-alist)
                        nil
                        t))))
                 (fnc (cdr (assq command command-alist))))
            (when fnc
              (funcall fnc)))
        (message "Cannot locate ‘.cabal’ file."))
    (message "Cannot locate Cabal executable on this system.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions of all supported commands

(ebal--define-command build ?b cabal
  (let ((target
         (or arg
             (funcall ebal-completing-read-function
                      "Build target: "
                      (cons "default" ebal--project-targets)
                      nil t nil nil "default"))))
    (apply #'ebal--perform-command
           "build"
           (unless (string= target "default")
             (list target)))))

(ebal--define-command build ?b stack
  (ebal--ensure-stack-init ebal--last-directory)
  (ebal--perform-command "build"))

(ebal--define-command configure ?c cabal
  (ebal--perform-command "configure"))

(ebal--define-command sdist ?d both
  (ebal--ensure-stack-init ebal--last-directory)
  (ebal--perform-command "sdist"))

(ebal--define-command bench ?e both
  ;; TODO Improve this so specific benchmarks can be run.
  (ebal--ensure-stack-init ebal--last-directory)
  (ebal--perform-command "bench"))

(ebal--define-command freeze ?f cabal
  (ebal--perform-command "freeze"))

(ebal--define-command fetch ?g cabal
  (let* ((installed-packages
          (ebal--installed-packages ebal--last-directory))
         (package
          (or arg
              (funcall ebal-completing-read-function
                       "Package to fetch: "
                       installed-packages
                       nil t nil nil (car installed-packages)))))
    (ebal--perform-command "fetch" package)))

(ebal--define-command haddock ?h both
  (ebal--ensure-stack-init ebal--last-directory)
  (ebal--perform-command "haddock"))

(ebal--define-command install ?i both
  (ebal--ensure-sandbox-exists ebal--last-directory)
  (ebal--ensure-stack-init     ebal--last-directory)
  (ebal--perform-command "install"))

(ebal--define-command check ?k cabal
  (ebal--perform-command "check"))

(ebal--define-command list ?l cabal
  (ebal--perform-command "list"))

(ebal--define-command sandbox-init ?n cabal
  (if (ebal--sandbox-exists-p ebal--last-directory)
      (message "Sandbox already exists")
    (ebal--perform-command "sandbox init")))

(ebal--define-command info ?o cabal
  (let* ((installed-packages
          (ebal--installed-packages ebal--last-directory))
         (package
          (or arg
              (funcall ebal-completing-read-function
                       "Show info about package: "
                       nil t nil nil (car installed-packages)))))
    (ebal--perform-command "info" package)))

(ebal--define-command test ?t both
  ;; TODO allow to run specific test components
  (ebal--ensure-stack-init ebal--last-directory)
  (ebal--perform-command "test"))

(ebal--define-command update ?u both
  (ebal--ensure-stack-init ebal--last-directory)
  (ebal--perform-command "update"))

(ebal--define-command sandbox-delete ?x cabal
  (if (ebal--sandbox-exists-p ebal--last-directory)
      (when (yes-or-no-p "Really delete the sandbox and all packages?")
        (ebal--perform-command "sandbox delete"))
    (message "There is no sandbox to delete")))

(ebal--define-command clean ?z both
  (ebal--ensure-stack-init ebal--last-directory)
  (ebal--perform-command "clean"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface

(defalias 'ebal-built-in-completing-read 'completing-read)
(defalias 'ebal-command-completing-read  'completing-read)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ebal command popup

(defun ebal-command-popup
    (prompt collection &optional predicate
            _require-match _initial-input _hist _def _inherit-input-method)
  "Show a popup displaying PROMPT and COLLECTION of commands.

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
      (let ((buffer (get-buffer-create
                     (if (ebal--cabal-mode-p)
                         "*Cabal Commands*"
                       "*Stack Commands*"))))
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
                        'face 'ebal-project-name))
           (when ebal--project-version
             (insert
              (propertize (format " %s" ebal--project-version)
                          'face 'ebal-project-version)))
           (insert "\n\n")
           (insert (propertize "Commands:\n" 'face 'ebal-header))
           (dolist (item col)
             (cl-destructuring-bind (key . command) item
               (insert (propertize (string key) 'face 'ebal-key))
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
  "Read user's input using `ebal-completing-read-function'.

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
  (unless (or ebal--init-aborted
              ebal--init-template-selected)
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
                         require-match
                         nil
                         nil
                         (car collection))
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
  "Create a .cabal, Setup.hs, and optionally a LICENSE file interactively.

It's also possible to use a Stack template.  Note that in any
case you should first create directory for your project and only
then call this command."
  (interactive)
  (if (ebal--prepare)
      (message "The directory is already Cabalized, it seems")
    (run-hooks ebal-before-init-hook)
    (setq ebal--init-aborted           nil
          ebal--init-template-selected nil)
    (let* ((ebal--project-name
            (ebal--init-query
             "Package name: "
             (file-name-nondirectory
              (directory-file-name
               default-directory))))
           (template
            (when (ebal--stack-mode-p)
              (let ((result
                     (ebal--init-query
                      "Use template: "
                      (cons "none" (ebal--stack-templates)))))
                (when result
                  (setq ebal--init-template-selected t)
                  result))))
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
            (when (string-equal type "Executable")
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
             '("src" "none"))))
      (unless ebal--init-aborted
        (ebal--ensure-sandbox-exists default-directory 'after)
        (if ebal--init-template-selected
            ;; Stack template
            (ebal--call-target
             default-directory
             "new"
             "--bare"
             ebal--project-name
             template)
          ;; Cabal init
          (ebal--ensure-stack-init default-directory 'after)
          (let ((ebal-operation-mode 'cabal))
            (ebal--call-target
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
             (unless (y-or-n-p
                      "Include documentation on what each field means? ")
               "--no-comments"))))))
    (run-hooks ebal-after-init-hook)))

(provide 'ebal)

;;; ebal.el ends here
