(** Given a suite, generate the [dune] files necessary to bootstrap a workflow
    (or do nothing if the workflow is already bootstrapped). *)
module Bootstrap : sig
  val perform : Dsl.spec -> unit
end

val emit_dune_inc : Dsl.spec -> path:string list -> unit
