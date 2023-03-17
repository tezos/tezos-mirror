# Michelson Emacs mode
This mode is a work in progress.
Please contact us with bugs and feature requests.
All of the options mentioned below are also accessible via the customize menu.

## Dependencies:
To operate the mode, please install the following dependencies.
All are available from either melpa, elpa, or marmalade
and are available under a free software license.

| Package  | Package Repository | Sources |
| -------- | ------------------ | --------------- |
| deferred | Melpa  | https://github.com/kiwanami/emacs-deferred |


## Required Configuration
The dependency `deferred` can be installed by running `M-x package-install-file`.
The package file is located in the emacs folder of the Octez code base.

If you use the Spacemacs distribution, you can add the package in `dotspacemacs-additional-packages`, like so:
```elisp
dotspacemacs-additional-packages '(
  (michelson-mode :location (recipe :fetcher url
                                   :url "https://gitlab.com/tezos/tezos/-/raw/master/emacs/michelson-mode.el")))
```
`quelpa` will be used under the hood to fetch and load the file.

Before using the Emacs mode, you must configure the `michelson-client-command`.
If you have compiled the Tezos Git repository,
set this to be the path to the `octez-client` binary on your system.
Make sure you have an up-to-date version of the client compiled.
You must also start an Octez node to enable typechecking features.
This option is recommended because it is faster than operating through
the docker container.

Here are examples of the client configuration:
```elisp
(setq michelson-client-command "~/tezos/octez-client")
(setq michelson-alphanet nil)
```

##### Alternatively, to set up the Michelson mode to use the Octez client in mockup mode (to typecheck Michelson scripts without interacting with an Octez node)
```elisp
(setq michelson-client-command "~/tezos/octez-client --base-dir /tmp/mockup --mode mockup --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK")
(setq michelson-alphanet nil)
```

Note that the Michelson mode will be chosen automatically by Emacs for files with a .tz or .tez extension.

We can now open our favourite contract `michelson_test_scripts/attic/id.tz` in Emacs and,
when moving the cursor on a Michelson instruction, in the bottom of the windows Emacs should display
the state of the stack before (left) and after (right) the application of the instruction.
The Emacs mode automatically type-checks your program and reports errors; once you are happy with the result you can ask the client to run it locally:

```elisp
octez-client run script ./michelson_test_scripts/attic/id.tz \
             on storage '"hello"' and input '"world"'
```

## Additional Options
There are various features of the Emacs mode you may wish to configure.

To view legacy contracts and inspect stack types of old contracts you can add the `--legacy` flag
to the `michelson-extra-flags` variable. This will have Emacs display correct type information.

```elisp
(setq michelson-extra-flags '("--legacy"))
```

If you previously added to the flags list (it's empty by default) and don't want to overwrite existing flags you can do

Alternatively, you can use the `customize` interactive function to set those flags.

```elisp
(add-to-list 'michelson-extra-flags "--legacy")
```

### Error display
When writing a contract, you may wish to disable error display in order to
avoid the "wrong stack type at end of body" error that is often present.
This can be done by changing the
`michelson-print-errors` and `michelson-highlight-errors` options.
Both of these options also have interactive toggles for easy access.

### Live printing
You can disable live printing using the `michelson-live-editing` option.
If this option is disabled, both type and error printing are suppressed.
No background command will be run, limiting the mode to syntax highlighting.
This command can also be toggled interactively using the
`michelson-toggle-live-editing` command.

### Faces
The highlighting colors used can be configured. See the customize menu for details.
