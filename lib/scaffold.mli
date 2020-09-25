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

type dir
(** The type of specifications of directories inside a test suite. *)

val executables :
  ?expect_failure:unit ->
  ?ppx:string ->
  ?sanitize:(in_channel -> out_channel -> unit) ->
  unit ->
  dir
(** Define a directory in which each [.ml] file is an executable corresponding
    to a test case.Each executable will have its output captured in a
    corresponding [.expected] file, after passing it through the optional
    [sanitize] filter.

    If [expect_failure] is passed, the execution of the executable must fail
    with a non-zero exit code. New test cases can be added by creating a new
    [.ml] file in the directory. *)

val ppx_tests :
  ?expect_failure:unit ->
  ?ppx:string ->
  ?styler:string ->
  ?and_then:[ `Noop | `Build | `Run ] ->
  unit ->
  dir
(** Define a directory in which each [.ml] file is a test case for a PPX. Each
    file is preprocessed and then the AST is printed to a corresponding
    [.expected] file.

    - If [expect_failure] is passed, then the preprocessor is required to fail
      and the [.expected] file instead captures the standard output/error of the
      process.

    - If [styler] is passed, it specifies the name of a binary to use for
      formatting the resulting AST before writing it to the [.expected] file.

    - If [and_then = `Build | `Run] is passed, then the post-processed
      executable will be built or build-and-executed respectively. These
      processes are required to succeed. *)

val group :
  ?package:string ->
  ?libraries:string list ->
  ?ppx:string ->
  (string * dir) list ->
  dir
(** Group a set of directories. Raises an exception if there are multiple
    directories with a given name. *)

type spec
(** The type of test suite specifications. *)

val v :
  ?package:string ->
  ?libraries:string list ->
  ?ppx:string ->
  this_file:string ->
  (string * dir) list ->
  spec
(** Declare a test suite rooted at the current directory.

    @param package The package with which to associate the [runtest] aliases
    @param ppx The [ocamlfind] library name of the PPX to be tested.
    @param this_file The file that defines the root of the project (intended to
    be called with [~this_file:__FILE__]). *)

val declare : spec -> _
