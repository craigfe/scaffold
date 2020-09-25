`scaffold` will create the correct top-level `dune` file for us:

  $ scaffold emit-toplevel > dune.inc
  $ touch dune.scaffold-inc
  $ chmod +w dune.scaffold-inc

  $ cat dune.inc
  (test
   (name main)
   (deps (source_tree .))
   (modules main)
   (libraries scaffold))
  
  (rule
   (targets dune.scaffold-inc.gen)
   (deps
    (source_tree .))
   (action
    (with-stdout-to
     %{targets}
     (run %{exe:main.exe} generate --path ''))))
  
  (rule
   (alias runtest)
   (action
    (diff dune.scaffold-inc dune.scaffold-inc.gen)))
  
  (include dune.scaffold-inc)

Running `dune runtest` for the first time will bootstrap the workflow:

  $ dune runtest --force --auto-promote
  File "dune.scaffold-inc", line 1, characters 0-0:
  Error: Files _build/default/dune.scaffold-inc and
  _build/default/dune.scaffold-inc.gen differ.
          main alias runtest
  Files successfully installed in `$TESTCASE_ROOT':
  
  - ./failing/.
  - ./failing/dune
  - ./failing/dune.inc
  - ./passing/.
  - ./passing/dune
  - ./passing/dune.inc
  
  `dune runtest` again to populate the newly-created `dune.inc` files.
  Promoting _build/default/dune.scaffold-inc.gen to dune.scaffold-inc.
  [1]

  $ ! (dune runtest --force --auto-promote)
  File "failing/dune.inc", line 1, characters 0-0:
  Error: Files _build/default/failing/dune.inc and
  _build/default/failing/dune.inc.gen differ.
  File "passing/dune.inc", line 1, characters 0-0:
  Error: Files _build/default/passing/dune.inc and
  _build/default/passing/dune.inc.gen differ.
  Promoting _build/default/failing/dune.inc.gen to failing/dune.inc.
  Promoting _build/default/passing/dune.inc.gen to passing/dune.inc.

We can add a new test case and populate it without needing to create a
corresponding `.expected` file:

  $ echo >passing/test.ml ';; assert ([%foo] = "bar")'

  $ ! (dune runtest --force --auto-promote)
  File "passing/dune.inc", line 1, characters 0-0:
  Error: Files _build/default/passing/dune.inc and
  _build/default/passing/dune.inc.gen differ.
          main alias runtest
  Files successfully installed in `$TESTCASE_ROOT':
  
  - passing/test.expected
  
  `dune runtest` again to populate the newly-created `dune.inc` files.
  Promoting _build/default/passing/dune.inc.gen to passing/dune.inc.

  $ ! (dune runtest --force --auto-promote)
  File "passing/test.expected", line 1, characters 0-0:
  Error: Files _build/default/passing/test.expected and
  _build/default/passing/test.actual differ.
  Promoting _build/default/passing/test.actual to passing/test.expected.

  $ cat passing/test.expected
  ;;assert ("bar" = "bar")
