# Development guide

## Build

To build a development version, install cabal and ghc and run:

```shell
$ git clone https://github.com/owo-lang/OwO
$ cd OwO
$ cabal install
```

To install bash completions temporarily, run:

```shell
$ source <(owo --bash-completion-script $(which owo))
```

To install bash completions for all users permanently, run:

```shell
$ owo --bash-completion-script $(which owo) >> owo_completion
$ sudo mv owo_completion /etc/.bash_completion.d/
```

## Setup your developing environment

### Code

TODO

### Emacs

You should have `stylish-haskell` installed:

```shell
$ cabal install stylish-haskell
```

Then, add this to your `.emacs` file:

```elisp
(custom-set-variables
 '(haskell-stylish-on-save t))
```

So that you can format your code when you save them.
