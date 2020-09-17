open Cmdliner

let emit_toplevel =
  Term.(
    const (fun () ->
        Printf.printf {|(test
 (name main)
 (libraries scaffold))|})
    $ const ())

let () =
  let default =
    let default_info =
      let doc = "Continuously benchmark a Git repository." in
      Term.info ~doc "pipeline"
    in
    Term.(ret (const (`Help (`Auto, None))), default_info)
  in
  Term.(
    exit
    @@ eval_choice default
         [
           ( emit_toplevel,
             Term.info
               ~doc:"Emit the toplevel [dune] file for a workflow declaration"
               "emit-toplevel" );
         ])
