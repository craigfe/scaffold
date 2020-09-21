open Utils

let ( / ) = Filename.concat
let filepath_of_steps = List.fold_left ( / ) ""
let env_defined = Sys.getenv_opt >> function Some _ -> true | None -> false

let new_file ~path =
  Format.kasprintf (fun contents ->
      let oc = open_out path in
      Printf.fprintf oc "%s" contents;
      close_out oc)

(* Starting from the current working directory, navigate upwards until a
   [dune-project] file is found. *)
let goto_project_root () =
  let inside_dune = env_defined "INSIDE_DUNE" in
  let rec inner ~ignore_ dir =
    let file_exists = Sys.file_exists (dir / "dune-project") in
    if file_exists && not ignore_ then Unix.chdir dir
    else
      let parent = Filename.dirname dir in
      assert (parent <> dir);
      inner ~ignore_:(ignore_ && not file_exists) (Filename.dirname dir)
  in
  inner ~ignore_:inside_dune (Sys.getcwd ())

(* For each case, we need to set up a [dune] to update the [dune.inc] files for
     each of the [.ml] files available in the directory
*)
let generate_group (suite : Dsl.spec) path =
  let package, _ = Dsl.get_package_and_libraries suite path in
  let dir = List.fold_left ( / ) (Filename.dirname suite.file) path in
  let dune_file = dir / "dune" in
  let pp_package ppf =
    match package with
    | Some p -> Format.fprintf ppf "@,(package %s)" p
    | None -> ()
  in
  if not (Sys.file_exists dir) then
    (* log (fun f -> f "File '%s' does not yet exist. Generating it..." dir); *)
    Unix.mkdir dir 0o777;
  (* log (fun f -> f "File '%s' does not exist yet. Generating it..." dune_file); *)
  new_file ~path:dune_file
    {|
(include dune.inc)

(rule
 (targets dune.inc.gen)
 (deps
  (source_tree .))
 (action
  (with-stdout-to
   %%{targets}
   (run %%{exe:../%s.exe} generate --path '%s'))))

(rule
 (alias runtest)%t
 (action
  (diff dune.inc dune.inc.gen)))
%!|}
    (Filename.chop_extension (Filename.basename suite.file))
    (filepath_of_steps path) pp_package

(** Given a spec, get the files that must be automatically generated during the
    bootstrapping process (given the current state of the FS). *)
let files_to_generate (t : Dsl.spec) : (string * (unit -> unit)) list =
  let if_not_exists ~path (name, f) =
    (* TODO: avoid repeatedly coercing between [string list] and [string] here *)
    let path = Filename.concat (filepath_of_steps path) name in
    if Sys.file_exists path then [] else [ (path, f) ]
  in
  let root_actions =
    if_not_exists ~path:[]
      ("dune.inc", fun () -> Engine.emit_toplevel_dune_inc t)
  in
  let leaf_actions =
    Dsl.fold_leaves
      (fun path _ acc ->
        let dune =
          if_not_exists ~path ("dune", fun () -> generate_group t path)
        in
        let dune_inc =
          if_not_exists ~path
            ( "dune.inc",
              fun () -> new_file ~path:(filepath_of_steps path / "dune.inc") ""
            )
        in
        let expected_files =
          Engine.fetch_test_cases ~dir:(filepath_of_steps path)
          |> Option.map
               (List.filter_map (fun Dsl.Test_case.{ name; has_expected; _ } ->
                    let path =
                      filepath_of_steps path
                      / (name ^ Dsl.Test_case.expected_ext)
                    in
                    if not has_expected then
                      Some (path, fun () -> new_file ~path "")
                    else None))
          |> Option.fold ~none:[] ~some:Fun.id
        in
        dune @ dune_inc @ expected_files @ acc)
      t []
  in
  root_actions @ leaf_actions

let perform suite =
  goto_project_root ();
  match files_to_generate suite with
  | [] -> ()
  | _ :: _ as fs ->
      List.iter (fun (_, f) -> f ()) fs;
      log (fun f ->
          let names = List.map (fun (a, _) -> a) fs in
          f
            "Dune files `%a' successfully installed. `dune runtest` again to \
             populate the newly-created `dune.inc` files."
            Fmt.(Dump.list string)
            names)
