open Utils

let ( / ) = Filename.concat

let log :
    type a. (((a, Format.formatter, unit, unit) format4 -> a) -> unit) -> unit =
 fun f ->
  f Format.eprintf;
  Format.eprintf "\n%!"

module Test_case = struct
  let expected_ext = ".expected"
  let opts_ext = ".opts"

  type t = { name : string; has_expected : bool; has_opts : bool }
end

(* Global configuration for tests in which the PPX fails (for consistency with
   various compiler versions / platforms). *)
let ppx_fail_global_stanzas ppf =
  Format.fprintf ppf
    {|(env
 (_
  (env-vars
   (OCAML_ERROR_STYLE "short")
   (OCAML_COLOR "never"))))

|}

let pp_list pp_elt ppf =
  List.iter (fun elt ->
      pp_elt ppf elt;
      Format.pp_print_space ppf ())

let pp_library_dependencies (suite : Dsl.spec) ppf =
  match suite.libraries with
  | [] -> ()
  | ls -> Fmt.pf ppf "@,(libraries %a)" (pp_list Format.pp_print_string) ls

let pp_package (suite : Dsl.spec) ppf =
  match suite.package with
  | None -> ()
  | Some p -> Fmt.pf ppf "@,(package %s)" p

let pp_ppx (suite : Dsl.spec) ppf =
  match suite.ppx with
  | None -> ()
  | Some p -> Fmt.pf ppf "@,(preprocess (pps %s))" p

let pp_rule (test_case : Test_case.t) (case : Dsl.executables) ppf =
  let pp_action ppf =
    match case.expect_state with
    | Output _ -> failwith "TODO"
    | Ppxed_ast { styler; _ } ->
        let pp_styler ppf =
          match styler with Some s -> Fmt.pf ppf " -styler %s" s | None -> ()
        in
        let pp_bin = "%{exe:../pp.exe}" (* TODO: determine path properly *) in
        Format.fprintf ppf
          ( if case.expect_failure then
            "; expect the process to fail, capturing stderr@,\
             @[<v 1>(with-stderr-to@,\
             %%{targets}@,\
             (bash \"! %s %t -no-color --impl %%{input}\"))@]"
            (* TODO: use (with-accepted-exit-codes) *)
          else "(run %s%t --impl %%{input} -o %%{targets})" )
          pp_bin pp_styler
  in
  Format.fprintf ppf
    "; Run the PPX on the `.ml` file@,\
     @[<v 1>(rule@,\
     (targets %s.actual)@,\
     @[<v 1>(deps@,\
     (:input %s.ml))@]@,\
     @[<v 1>(action@,\
     %t))@]@]"
    test_case.name test_case.name pp_action

let output_stanzas (suite : Dsl.spec) ~(case : Dsl.executables) =
  let pp_library ppf base =
    (* If the PPX will fail, we don't need to declare the file as executable *)
    if not case.expect_failure then
      Format.fprintf ppf
        "; The executable under test@,\
         @[<v 1>(executable@ (name %s)@ (modules %s)%t%t)@]" base base
        (pp_ppx suite)
        (pp_library_dependencies suite)
    else ()
  in
  let pp_diff_alias ppf base =
    Format.fprintf ppf
      "; Compare the post-processed output to the %s file@,\
       @[<v 1>(rule@,\
       (alias runtest)@,\
       %t@[<v 1>(action@,\
       @[<hov 2>(diff@ %s%s@ %s.actual)@])@])@]" Test_case.expected_ext
      (pp_package suite) base Test_case.expected_ext base
  in
  let pp_run_alias ppf base =
    (* If we expect the derivation to succeed, then we should be able to compile
       the output. *)
    if not case.expect_failure then
      Format.fprintf ppf
        "@,\
         @,\
         ; Ensure that the post-processed executable runs correctly@,\
         @[<v 1>(rule@,\
         (alias runtest)@,\
         %t@[<v 1>(action@,\
         @[<hov 2>(run@ ./%s.exe)@])@])@]" (pp_package suite) base
    else ()
  in
  fun ppf (Test_case.{ name; _ } as tc) ->
    let header =
      let left = Fmt.str "; -------- Test: `%s.ml` " name in
      let right = String.make (max 0 (80 - String.length left)) '-' in
      left ^ right
    in
    Format.fprintf ppf "@[<v 0>%s@,@,%a@,@,%t@,@,%a%a@,@]@." header pp_library
      name (pp_rule tc case) pp_diff_alias name pp_run_alias name

let env_defined = Sys.getenv_opt >> function Some _ -> true | None -> false

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

let remove_extension =
  let rec inner f =
    try inner (Filename.chop_extension f) with Invalid_argument _ -> f
  in
  inner

module Str_set = Set.Make (String)

(* Each test case has a single basename and a set of file extensions ([.ml],
   [.expected] and optionally [.opts]). *)
let fetch_test_cases ~(dir : string) : Test_case.t list option =
  if Sys.file_exists dir then
    Sys.readdir dir
    |> Array.to_list
    |> List.sort String.compare
    |> List.group ~break:(under2 remove_extension ( <> ))
    |> List.filter_map (fun files ->
           let name = remove_extension (List.hd files) in
           let extensions =
             files |> List.map Filename.extension |> Str_set.of_list
           in
           match Str_set.mem ".ml" extensions with
           | false -> None
           | true ->
               let has_expected =
                 Str_set.mem Test_case.expected_ext extensions
               in
               let has_opts = Str_set.mem Test_case.opts_ext extensions in
               Some Test_case.{ name; has_expected; has_opts })
    |> Option.some
  else None

let emit_toplevel_dune_inc (suite : Dsl.spec) =
  match suite.ppx with
  | None -> ()
  | Some s ->
      Fmt.pr
        {|
(rule
 (targets pp.ml)
 (action (with-stdout-to pp.ml (echo "Ppxlib.Driver.standalone ()"))))

(executable
 (name pp)
 (modules pp)
 (libraries ppxlib %s))
  |}
        s

let emit_dune_inc (suite : Dsl.spec) ~path =
  match path with
  | [] -> emit_toplevel_dune_inc suite
  | _ :: _ ->
      let ppf = Format.std_formatter in
      let case =
        match Dsl.index suite.groups path with
        | Some s -> s
        | None -> Fmt.failwith "Nothing at path %a" Fmt.(Dump.list string) path
      in
      let dir = Sys.getcwd () in
      if case.expect_failure then ppx_fail_global_stanzas ppf;
      fetch_test_cases ~dir
      |> Option.get
      |> List.iter (output_stanzas ~case suite ppf);
      Format.fprintf ppf "\n%!"

let new_file ~path =
  Format.kasprintf (fun contents ->
      let oc = open_out path in
      Printf.fprintf oc "%s" contents;
      close_out oc)

module Bootstrap = struct
  let filepath_of_steps = List.fold_left Filename.concat ""

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

  (** Given a spec, get the files that must be automatically generated during
      the bootstrapping process (given the current state of the FS). *)
  let files_to_generate (t : Dsl.spec) : (string * (unit -> unit)) list =
    let if_not_exists ~path (name, f) =
      (* TODO: avoid repeatedly coercing between [string list] and [string] here *)
      let path = Filename.concat (filepath_of_steps path) name in
      if Sys.file_exists path then [] else [ (path, f) ]
    in
    let root_actions =
      if_not_exists ~path:[] ("dune.inc", fun () -> emit_toplevel_dune_inc t)
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
                fun () ->
                  new_file ~path:(filepath_of_steps path / "dune.inc") "" )
          in
          let expected_files =
            fetch_test_cases ~dir:(filepath_of_steps path)
            |> Option.map
                 (List.filter_map (fun Test_case.{ name; has_expected; _ } ->
                      let path =
                        filepath_of_steps path / (name ^ Test_case.expected_ext)
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
end

let bootstrap = Bootstrap.perform
