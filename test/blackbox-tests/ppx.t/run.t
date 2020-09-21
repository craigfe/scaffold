`scaffold` will create the correct top-level `dune` file for us:

  $ scaffold emit-toplevel > dune.inc
  $ touch dune.scaffold-inc
  $ chmod +w dune.scaffold-inc

Running `dune runtest` for the first time will bootstrap the workflow:

  $ dune runtest --auto-promote
  File "dune.scaffold-inc", line 1, characters 0-0:
  Error: Files _build/default/dune.scaffold-inc and
  _build/default/dune.scaffold-inc.gen differ.
          main alias runtest
  Initially at: $TESTCASE_ROOT/_build/default
  inner: $TESTCASE_ROOT/_build/default
  inner: $TESTCASE_ROOT/_build
  inner: $TESTCASE_ROOT
  File './passing' does not yet exist. Generating it...
  File './passing/dune' does not exist yet. Generating it...
  File './failing' does not yet exist. Generating it...
  File './failing/dune' does not exist yet. Generating it...
  Dune files successfully installed. `dune runtest` again to populate the newly-created `dune.inc` files.
  Promoting _build/default/dune.scaffold-inc.gen to dune.scaffold-inc.
  [1]

  $ ! (dune runtest --auto-promote)
  File "failing/dune.inc", line 1, characters 0-0:
  Error: Files _build/default/failing/dune.inc and
  _build/default/failing/dune.inc.gen differ.
  File "passing/dune.inc", line 1, characters 0-0:
  Error: Files _build/default/passing/dune.inc and
  _build/default/passing/dune.inc.gen differ.
          main alias runtest
  Initially at: $TESTCASE_ROOT/_build/default
  inner: $TESTCASE_ROOT/_build/default
  inner: $TESTCASE_ROOT/_build
  inner: $TESTCASE_ROOT
  Dune files successfully installed. `dune runtest` again to populate the newly-created `dune.inc` files.
  Promoting _build/default/failing/dune.inc.gen to failing/dune.inc.
  Promoting _build/default/passing/dune.inc.gen to passing/dune.inc.

  $ echo >passing/test.ml ';; assert ([%foo] = "bar")'
  $ touch passing/test.expected

  $ ! (dune runtest --auto-promote)
  File "passing/dune.inc", line 1, characters 0-0:
  Error: Files _build/default/passing/dune.inc and
  _build/default/passing/dune.inc.gen differ.
          main alias runtest
  Initially at: $TESTCASE_ROOT/_build/default
  inner: $TESTCASE_ROOT/_build/default
  inner: $TESTCASE_ROOT/_build
  inner: $TESTCASE_ROOT
  Dune files successfully installed. `dune runtest` again to populate the newly-created `dune.inc` files.
  Promoting _build/default/passing/dune.inc.gen to passing/dune.inc.

  $ ! (dune runtest --auto-promote)
          main alias runtest
  Initially at: $TESTCASE_ROOT/_build/default
  inner: $TESTCASE_ROOT/_build/default
  inner: $TESTCASE_ROOT/_build
  inner: $TESTCASE_ROOT
  Dune files successfully installed. `dune runtest` again to populate the newly-created `dune.inc` files.
  File "passing/test.expected", line 1, characters 0-0:
  Error: Files _build/default/passing/test.expected and
  _build/default/passing/test.actual differ.
  Promoting _build/default/passing/test.actual to passing/test.expected.

  $ cat passing/test.expected
  ;;assert ("bar" = "bar")
