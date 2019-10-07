# bankcheck

`bankcheck` is an application that supports the creation of `stack new` templates.

## Background

`stack new` takes a file with the extension `.hsfiles` such as
[this](https://github.com/leigh-perry/template-haskell/blob/master/lpskel.hsfiles)
and uses the contents of that file as a template.
`.hsfiles` files can specify multiple files by separating the contents of those files
with the `{-# START_FILE #-}` pragma. When the template is passed to `stack new`,
Stack will create those files and any specified directories according to the `{-# START_FILE #-}`
pragmas, eg `{-# START_FILE src/Program.hs #-}` will be expanded as a file `src/Program.hs`.

## Why This App?

Preparing and maintaining the contents of the `.hsfiles` file is painful. With this app,
you can point it a sample project and its subdirectories. `bankcheck` will then crunch
the sample project files into an overall `.hsfiles`.

`bankcheck` omits files and directories such as `.git` and `out`.

## Usage

`bankcheck <base input directory> <target hsfiles path>`
