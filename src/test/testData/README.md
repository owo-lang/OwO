## Test Data

This is a simple parsing/lexing test case set.

We store input and expected output here, running test is to get the actual
output and do a comparison.

### Running tests

A simple `stack test` command can get the job done.

```shell
$ stack test
```

The underlying test script can forcibly write the actual output to the expected
data, so if you want to be asked for it, run the script manually and switch the
option `--force` on.

```shell
$ cd src/test/testData
$ perl test_runner.pl --force
```
