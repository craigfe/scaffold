`ppx-tester` will create the correct top-level `dune` file for us:

  $ ppx-tester emit-toplevel > dune.inc

Running `dune runtest` for the first time will bootstrap the workflow:

  $ dune runtest --auto-promote
  File './passing' does not yet exist. Generating it...
  File './passing/dune' does not exist yet. Generating it...
  File './failing' does not yet exist. Generating it...
  File './failing/dune' does not exist yet. Generating it...
  Dune files successfully installed. `dune runtest` again to run the newly-installed files.

  $ ! (dune runtest --auto-promote)
  File "failing/dune.inc", line 1, characters 0-0:
  Error: Files _build/default/failing/dune.inc and
  _build/default/failing/dune.inc.gen differ.
  File "passing/dune.inc", line 1, characters 0-0:
  Error: Files _build/default/passing/dune.inc and
  _build/default/passing/dune.inc.gen differ.
  Promoting _build/default/failing/dune.inc.gen to failing/dune.inc.
  Promoting _build/default/passing/dune.inc.gen to passing/dune.inc.

  $ echo >passing/test.ml "Foo"
  $ touch passing/test.expected

  $ dune runtest --auto-promote
  File "passing/dune.inc", line 1, characters 0-0:
  Error: Files _build/default/passing/dune.inc and
  _build/default/passing/dune.inc.gen differ.
  Promoting _build/default/passing/dune.inc.gen to passing/dune.inc.
  [1]

  $ dune runtest
  File "passing/test.expected", line 1, characters 0-0:
  Error: Files _build/default/passing/test.expected and
  _build/default/passing/test.actual differ.
  File "passing/test.ml", line 1, characters 0-3:
  1 | Foo
      ^^^
  Error: Unbound constructor Foo
  [1]
