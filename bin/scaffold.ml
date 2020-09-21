open Cmdliner

let emit_toplevel =
  Term.(
    const (fun () ->
        Printf.printf
          {|(test
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
   %%{targets}
   (run %%{exe:main.exe} generate --path ''))))

(rule
 (alias runtest)
 (action
  (diff dune.scaffold-inc dune.scaffold-inc.gen)))

(include dune.scaffold-inc)
|})
    $ const ())

let () =
  let default =
    let default_info =
      let doc = "Scaffolding helpers for OCaml testing frameworks" in
      Term.info ~doc "scaffold"
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
