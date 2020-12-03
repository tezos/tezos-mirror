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
To use the mode, you must load the `michelson-mode.el` file into Emacs.
Add the following to your `.emacs` file.
```elisp
(load "~/tezos/tezos/emacs/michelson-mode.el" nil t)
```
If you use the Spacemacs distribution, you can add the package in `dotspacemacs-additional-packages`, like:
```elisp
dotspacemacs-additional-packages '(
  (michelson-mode :location (recipe :fetcher url
                                   :url "https://gitlab.com/tezos/tezos/-/raw/master/emacs/michelson-mode.el")))
```
`quelpa` will be used under the hood to fetch and load the file.

Before using the Emacs mode, you must configure the `michelson-client-command`.
If you have compiled the Tezos Git repository,
set this to be the path to the `tezos-client` binary on your system.
Make sure you have an up to date version of the client compiled.
You must also start a tezos node to enable typechecking features.
This option is recommended because it is faster than operating through
the docker container.

If you wish to run the Emacs mode with one of the Docker scripts
(mainnet.sh, carthagenet.sh...), use the path of the script, plus the
word `client`. You must also set the `michelson-alphanet` variable to
be `t`.  If you do not set this option, the mode will not work with
the Docker script.

Here are examples of the client configuration:
### Without the Docker script
```elisp
(setq michelson-client-command "~/tezos/tezos/tezos-client")
(setq michelson-alphanet nil)
```
### With the Docker script for Carthagenet
```elisp
(setq michelson-client-command "~/tezos/alphanet/carthagenet.sh client")
(setq michelson-alphanet t)
```

## Additional Options
There are various feature of the Emacs mode you may wish to configure.

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
