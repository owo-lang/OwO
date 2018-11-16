# OwO

The compiler of the OwO pwogwamming langwage. WIP.

This pwogwamming langwage is inspired by Agda, Idris, Haskell and many
other nice pwogwamming langwages.

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

## License

Apache-2.0

```text
  ___            ___
 / _ \          / _ \
| | | |_      _| | | |
| | | \ \ /\ / / | | |
| |_| |\ V  V /| |_| |
 \___/  \_/\_/  \___/
    (What's this?)
```
