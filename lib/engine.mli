module Dsl : sig
  type dir
  (** The type of specifications of directories inside a test suite. *)

  val executables :
    mode:[ `Output_expect | `Ppx_expect ] ->
    ?expect_failure:unit ->
    ?ppx:string ->
    ?sanitize:(in_channel -> out_channel -> unit) ->
    unit ->
    dir
  (** Define a directory in which each [.ml] file is an executable corresponding
      to a test case. There are two possible [mode]s:

      - [`Output_expect]: each executable will have its output captured in a
        corresponding [.expected] file, after passing it through the optional
        [sanitize] filter.

      - [`Ppx_expect]: each [.ml] file is preprocessed using [ppx] and then the
        AST is printed to a corresponding [.expected] file. The post-processed
        file is also required to compile.

      If [expect_failure] is passed, the execution of the executable or PPX
      respetively must fail with a non-zero exit code. New test cases can be
      added by creating a new [.ml] file in the directory. *)

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
