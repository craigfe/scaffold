(** [Scaffold] provides a set of combinators for declaratively specifying test
    directory structures for OCaml projects that use Dune. It's capable of
    generating simple Dune rules for basic unit-testing of libraries or
    cram-testing of executables, but is intended for more elaborate use-cases
    such as testing PPXes and binary-generating libraries that would otherwise
    require a lot of manual configuration.

    {2 How it works}

    The test directory is specified as a top level {{!Scaffold.spec}
    specification} inside an executable file, and the source tree below that
    file will be managed accordingly. [Scaffold] performs the initial bootstrap
    by creating the directory structure with [dune] files.

    {[
      test/
      |-- spec.ml <---[generated-by]---\
      |-- dune                         |
      |-- dune.inc - - - - - - - - - ->|
      `-- ppx_foo/                     |
          |-- dune                     |
          |-- dune.inc - - - - - - - ->|
          |-- case1.ml
          |-- case1.expected
          |-- case2.ml
          |-- case2.expected
         ...
    ]} *)

(** {2 Test directories} *)

(** @inline *)
include sig
  include module type of Engine.Dsl
end

val declare : spec -> _
