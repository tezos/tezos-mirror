Without --retry, we fail as usual.

  $ ./tezt.sh retry
  [SUCCESS] (1/3) Success
  Starting test: Fail every other run test
  [error] Failing test on first try
  [FAILURE] (2/3, 1 failed) Fail every other run test
  Try again with: ./main.exe --verbose --test 'Fail every other run test'
  [1]

With --keep-going but without --retry, we run all tests and fail as usual.

  $ ./tezt.sh retry --keep-going
  [SUCCESS] (1/3) Success
  Starting test: Fail every other run test
  [error] Failing test on first try
  [FAILURE] (2/3, 1 failed) Fail every other run test
  Try again with: ./main.exe --verbose --test 'Fail every other run test'
  Starting test: Failing test
  [error] Always failing test
  [FAILURE] (3/3, 2 failed) Failing test
  Try again with: ./main.exe --verbose --test 'Failing test'
  [1]

If we set --retry, then the "fail once" test will eventually
succeed. However, the "fail always" test will fail the test suite.

  $ ./tezt.sh --retry 1 retry
  [SUCCESS] (1/3) Success
  Starting test: Fail every other run test
  [error] Failing test on first try
  [warn] 1 retry(ies) left for test: Fail every other run test
  [SUCCESS] (2/3) Fail every other run test
  Starting test: Failing test
  [error] Always failing test
  [warn] 1 retry(ies) left for test: Failing test
  Starting test: Failing test
  [error] Always failing test
  [FAILURE] (3/3, 1 failed) Failing test
  Try again with: ./main.exe --verbose --test 'Failing test'
  [1]


If we deselect the "fail always" test, the suite succeeds:

  $ ./tezt.sh --retry 1 retry /always
  [SUCCESS] (1/2) Success
  Starting test: Fail every other run test
  [error] Failing test on first try
  [warn] 1 retry(ies) left for test: Fail every other run test
  [SUCCESS] (2/2) Fail every other run test

With --keep-going, we still fail:

  $ ./tezt.sh --retry 1 retry --keep-going
  [SUCCESS] (1/3) Success
  Starting test: Fail every other run test
  [error] Failing test on first try
  [warn] 1 retry(ies) left for test: Fail every other run test
  [SUCCESS] (2/3) Fail every other run test
  Starting test: Failing test
  [error] Always failing test
  [warn] 1 retry(ies) left for test: Failing test
  Starting test: Failing test
  [error] Always failing test
  [FAILURE] (3/3, 1 failed) Failing test
  Try again with: ./main.exe --verbose --test 'Failing test'
  [1]

Looping is handled:

  $ ./tezt.sh --retry 1 retry --keep-going --loop-count 2
  [SUCCESS] (1/3) (loop 1) Success
  Starting test: Fail every other run test
  [error] Failing test on first try
  [warn] 1 retry(ies) left for test: Fail every other run test
  [SUCCESS] (2/3) (loop 1) Fail every other run test
  Starting test: Failing test
  [error] Always failing test
  [warn] 1 retry(ies) left for test: Failing test
  Starting test: Failing test
  [error] Always failing test
  [FAILURE] (3/3, 1 failed) (loop 1) Failing test
  Try again with: ./main.exe --verbose --test 'Failing test'
  [SUCCESS] (1/3, 1 failed) (loop 2) Success
  Starting test: Fail every other run test
  [error] Failing test on first try
  [warn] 1 retry(ies) left for test: Fail every other run test
  [SUCCESS] (2/3, 1 failed) (loop 2) Fail every other run test
  Starting test: Failing test
  [error] Always failing test
  [warn] 1 retry(ies) left for test: Failing test
  Starting test: Failing test
  [error] Always failing test
  [FAILURE] (3/3, 2 failed) (loop 2) Failing test
  Try again with: ./main.exe --verbose --test 'Failing test'
  [1]

Retries work with `-j`, but we don't know the order of the output. So
we only check that the correct set of tests have run and their result.

  $ ./tezt.sh --retry 1 retry -j 2 --keep-going | grep '^\[[A-Z]\+\]' | sed 's/^\(\[[A-Z]\+\] \)(.*) /\1/' | LC_COLLATE=c sort
  [FAILURE] Failing test
  [SUCCESS] Fail every other run test
  [SUCCESS] Success
