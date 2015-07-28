# Ebal

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/ebal.svg?branch=master)](https://travis-ci.org/mrkkrp/ebal)

This is Emacs interface to Cabal. Currently, it provides fast and easy
access to most Cabal commands:

* <kbd>M-x ebal-init</kbd> `cabal init`
* <kbd>M-x ebal-execute</kbd> — opens a popup menu with the following:
  * <kbd>b</kbd> `cabal build`
  * <kbd>c</kbd> `cabal configure`
  * <kbd>d</kbd> `cabal sdist`
  * <kbd>e</kbd> `cabal bench`
  * <kbd>f</kbd> `cabal freeze`
  * <kbd>g</kbd> `cabal fetch`
  * <kbd>i</kbd> `cabal install`
  * <kbd>k</kbd> `cabal check`
  * <kbd>l</kbd> `cabal list`
  * <kbd>n</kbd> `cabal sandbox init`
  * <kbd>o</kbd> `cabal info`
  * <kbd>t</kbd> `cabal test`
  * <kbd>u</kbd> `cabal update`
  * <kbd>x</kbd> `cabal sandbox delete`
  * <kbd>z</kbd> `cabal clean`

The project is young and experimental, fell free to propose improvements or
open pull requests.

## Installation

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'ebal)
```

## Usage

The package provides two commands:

* `ebal-init` that acts as a wizard helping create new Cabal project;

* `ebal-execute` — this allows to perform any Cabal command while visiting
  any file or directory in a Cabal project.

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

Some commands, like `cabal build` can gather additional info to help you to
enter arguments. `cabal build` shows (if you're using IDO) list of all build
targets extracted from your `.cabal` file, while `cabal info` will display
complete list of installed packages to choose from, etc.

The usage should be pretty straightforward, so let me tell you how to
customize the package.

## Customization

To customize Ebal, you can set variables as well as use Emacs own
customization interface, which may be more user-friendly for some.

To use customization interface type <kbd>M-x customize-group RET ebal
RET</kbd>.

----

```
ebal-cabal-executable ⇒ nil
```

Path to Cabal executable.

If it's not `nil`, this value is used in invocation of Cabal commands
instead of standard `"cabal"` string. Set this variable if your Cabal is in
a strange place where OS cannot find it.

Note that the path is quoted with `shell-quote-argument` before being used
to compose command line.

----

```
ebal-global-option-alist ⇒
  ((clean)
   (sandbox-delete)
   (update)
   (test)
   (info)
   (sandbox-init)
   (list "--installed" "--simple-output")
   (check)
   (install "--only-dependencies" "--enable-tests" "--enable-benchmarks")
   (haddock)
   (fetch)
   (freeze "--enable-tests" "--enable-benchmarks")
   (bench)
   (sdist)
   (configure "--enable-tests" "--enable-benchmarks")
   (build))
```

Alist that maps names of commands to their default options.

Names of commands are symbols and options are lists of strings.

Note that this is global collection of options. If you want to specify
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

The following values are recognized:

`nil` — don't create sandboxes unless user explicitly runs command to create
one.

`ask` — ask if user wants to create a sandbox (so it's harder to forget to
create it), this is often preferable because most Haskell developers want
sandboxes everywhere nowadays (default).

`always` — create sandboxes silently when they are missing and they should
be created. With this option every your project is sandboxed without any
effort on your side.

All other values of this variabe produce the same effect as `always`.

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

This is what `ebal-execute` uses.  Default is Ebal custom popup buffer, but
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

You can check name of the command in `ebal--actual-command`.

----

```
ebal-after-command-hook ⇒ nil
```

Hook to run after execution of particular command.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
