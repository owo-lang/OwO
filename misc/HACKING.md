# Development guide

## Build

To build a development version, install cabal and ghc and run:

```shell
$ git clone https://github.com/owo-lang/OwO
$ cd OwO
$ cabal install alex
$ cabal install
```

You can also use stack, no problem:

```shell
$ git clone https://github.com/owo-lang/OwO
$ cd OwO
$ stack install alex
$ stack install
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

You can run `hlint` with this command:

```shell
$ hlint src
```

## Testing

You need perl5 \> v5.10 to run tests.

### Parsing tests

```shell
$ cd src/test/testData
$ ./test_runner.pl
```

## Coding convention

* Functions/record projections should be camelCase

## Setup your editor

### IntelliJ-Haskell

You're basically all set.

### Code

Install the `stylish-haskell` extension to format the code easier.
You should always format your code before pushing it.

It's recommended to use drammie's `Simple GHC (Haskell) Integration` plugin.
Configure your workspace:

```json
{
    "ghcSimple.workspaceType": "stack"
}
```

You can also install the extension `Run On Save` to simply run the formatter on
saving your `*.hs` file, by giving such commands in settings:

```json
{
    "emeraldwalk.runonsave": {
        "commands": [
            {
                "match": "\\.hs$",
                "cmd": "stylish-haskell -i ${file}"
            }
        ]
    }
}
```

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
