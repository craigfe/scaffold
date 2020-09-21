val fetch_test_cases : dir:string -> Dsl.Test_case.t list option
val emit_toplevel_dune_inc : Dsl.spec -> unit
val emit_dune_inc : Dsl.spec -> path:string list -> unit
