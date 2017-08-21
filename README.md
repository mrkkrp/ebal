# Ebal

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/ebal-badge.svg)](https://melpa.org/#/ebal)
[![Build Status](https://travis-ci.org/mrkkrp/ebal.svg?branch=master)](https://travis-ci.org/mrkkrp/ebal)

*If you're using stack, please prefer
[hasky-stack](https://github.com/hasky-mode/hasky-stack) from now on. Ebal
is in maintenance mode.*

This is an Emacs interface to Cabal and Stack. Currently, it provides fast
and easy access to most commands (†—commands available in Stack mode):

* <kbd>M-x ebal-init</kbd> `cabal init` (useful even in Stack mode)
* <kbd>M-x ebal-execute</kbd>—opens a popup menu with the following:
  * <kbd>b</kbd> `build` †
  * <kbd>c</kbd> `configure`
  * <kbd>d</kbd> `sdist` †
  * <kbd>e</kbd> `bench` †
  * <kbd>f</kbd> `freeze`
  * <kbd>g</kbd> `fetch`
  * <kbd>g</kbd> `haddock` †
  * <kbd>i</kbd> `install` †
  * <kbd>k</kbd> `check`
  * <kbd>l</kbd> `list`
  * <kbd>n</kbd> `sandbox init`
  * <kbd>o</kbd> `info`
  * <kbd>t</kbd> `test` †
  * <kbd>u</kbd> `update` †
  * <kbd>x</kbd> `sandbox delete`
  * <kbd>z</kbd> `clean` †

Note that `stack init` is called for you automatically in Stack mode when
`stack.yaml` is missing.

## Installation

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'ebal)
```

It's available via MELPA, so you can just <kbd>M-x package-install RET ebal
RET</kbd>.

## Usage

If you want to use Ebal with Stack, add this to your configuration:

```emacs-lisp
(setq ebal-operation-mode 'stack)
```

The package provides two commands:

* `ebal-init` that acts as a wizard helping create new Cabal project;

* `ebal-execute`—this allows to perform any command while visiting any file
  or directory in a project.

You can create key bindings for these commands to simplify interaction. I
advise creating of simple key binding at least for `ebal-execute`, since you
will call it often.

### `ebal-init`

The wizard mirrors built-in Cabal command `cabal init`, but thanks to Emacs,
it provides probably much better experience than the original command line
tool. It's worth noticing that Ebal doesn't perform generation of `.cabal`
file for you, it only gathers arguments for invocation of `cabal init`.

### `ebal-execute`

The command displays a popup menu that contains name of project and its
version. It also displays collection of commands, to invoke any of them, you
only need to press a key:

![Ebal Execute](https://raw.githubusercontent.com/mrkkrp/ebal/gh-pages/ebal-execute.png)

Some commands, like `build` can gather additional info to help you to enter
arguments. `build` shows a list of all build targets extracted from your
`.cabal` file, while `cabal info` will display complete list of installed
packages to choose from, etc.

The usage should be pretty straightforward, so let me tell you how to
customize the package.

## Customization

To customize Ebal, you can either set variables or use the customization
interface, which may seem more friendly for some people.

To use the customization interface type <kbd>M-x customize-group RET ebal
RET</kbd>.

----

```
ebal-operation-mode ⇒ cabal
```
Mode of operation for Ebal package.

The following values are recognized:

* `cabal`—Ebal works as an interface for Cabal
* `stack`—Ebal works as an interface for Stack

All other values of this variable produce the same effect as `cabal`.

----

```
ebal-cabal-executable ⇒ nil
```

Path to Cabal executable.

If it's not `nil`, this value is used in invocation of Cabal commands
instead of the standard `"cabal"` string. Set this variable if your Cabal is
in a strange place where OS cannot find it.

Note that the path is quoted with `shell-quote-argument` before being used
to compose command line.

----

```
ebal-stack-executable ⇒ nil
```

Similar to `ebal-cabal-executable`, but for Stack.

----

```
ebal-global-option-alist ⇒ nil
```

Alist that maps names of commands to their default options.

Names of commands are symbols and options are lists of strings.

Note that this is a global collection of options. If you want to specify an
option to be used only with a specific command and in a specific project,
see `ebal-project-option-alist` and corresponding setup instructions.

----

```
ebal-project-option-alist ⇒ nil
```

Alist that maps names of commands to their default options.

Names of commands are symbols and options are lists of strings.

This variable represents user's preferences for current project. Value of
the variable is read from `"*.ebal"` file that may be present in project's
root directory (the same directory that contains `"*.cabal"` file).

Don't set this variable manually, instead create `"project-name.ebal"` file
and put desired value (a Lisp Object) into it unquoted.

----

```
ebal-sandboxing ⇒ ask
```

This determines Ebal's policy towards sandboxing.

The following values are recognized (Cabal mode only):

* `nil`—don't create sandboxes unless user explicitly runs command to create
  one.

* `ask`—ask if user wants to create a sandbox (so it's harder to forget to
  create it), this is often preferable because most Haskell developers want
  sandboxes everywhere nowadays (default).

* `always`—create sandboxes silently when they are missing and they should
  be created. With this option every your project is sandboxed without any
  effort on your side.

All other values of this variable produce the same effect as `always`.

----

```
ebal-completing-read-function ⇒ ebal-built-in-completing-read
```

Function to be called when requesting input from the user.

----

```
ebal-select-command-function ⇒ ebal-command-popup
```

Function to call to select Ebal command.

This is what `ebal-execute` uses. Default is Ebal custom popup buffer, but
you can use IDO-powered variant if you like or plain
`ebal-command-completing-read`.

The function is called with arguments like those that `completing-read`
takes.

----

```
ebal-popup-key-alist ⇒ nil
```

Alist that maps names of commands to keys used in Ebal popup.

This is used by `ebal-command-popup`.

----

```
ebal-before-init-hook ⇒ nil
```

Hook to run before execution of `ebal-init` function.

----

```
ebal-after-init-hook ⇒ nil
```

Hook to run after execution of `ebal-init` function.

----

```
ebal-before-command-hook ⇒ nil
```

Hook to run before execution of particular command.

Name of the command is available in `ebal--actual-command`.

----

```
ebal-after-command-hook ⇒ nil
```

Hook to run after execution of particular command.

Name of the command is available in `ebal--actual-command`.

## Defining your own commands

This is somewhat advanced topic, but it's possible to define your own
commands. Use `ebal--define-command` to define new commands, see its
associated documentation in Emacs. Here is an example of calling `yesod
devel` with `stack`:

```emacs-lisp
(require 'ebal)

(ebal--define-command yesod-devel ?y stack
  (ebal--ensure-stack-init ebal--last-directory)
  (ebal--perform-command "exec" "--" "yesod" "devel"))
```

This is admittedly basic example, but it should be of some use. See how
other commands are defined in source code for inspiration.

## License

Copyright © 2015–2017 Mark Karpov

Distributed under GNU GPL, version 3.
