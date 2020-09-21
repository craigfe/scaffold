module Dsl : sig
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
    ?and_then:[ `Build | `Run ] ->
    unit ->
    dir
  (** Define a directory in which each [.ml] file is a test case for a PPX. Each
      file is preprocessed and then the AST is printed to a corresponding
      [.expected] file.

      - If [expect_failure] is passed, then the preprocessor is required to fail
        and the [.expected] file instead captures the standard output/error of
        the process.

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
      @param this_file The file that defines the root of the project (intended
      to be called with [~this_file:__FILE__]). *)
end

(** Given a suite, generate the [dune] files necessary to bootstrap a workflow
    (or do nothing if the workflow is already bootstrapped). *)
module Bootstrap : sig
  val perform : Dsl.spec -> unit
end

val emit_dune_inc : Dsl.spec -> path:string list -> unit
