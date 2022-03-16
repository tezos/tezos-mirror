Run a test that should fail:

  $ ./tezt.sh --file test_retry.ml --test 'Failing test'
  Starting test: Failing test
  [error] Always failing test
  [FAILURE] (1/1, 1 failed) Failing test
  Try again with: ./main.exe --verbose --file test_retry.ml --test 'Failing test'
  [1]
