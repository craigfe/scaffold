open Utils

let ( / ) = Filename.concat

let log :
    type a. (((a, Format.formatter, unit, unit) format4 -> a) -> unit) -> unit =
 fun f ->
  f Format.eprintf;
  Format.eprintf "\n%!"

module Test_case = struct
  type t = { name : string; has_expected : bool; has_opts : bool }
end

module Dsl = struct
  type executables = {
    mode : [ `Output_expect | `Ppx_expect ];
    expect_failure : bool;
    ppx : string option;
    sanitize : (in_channel -> out_channel -> unit) option;
  }

  type dir =
    | Group of {
        package : string option;
        libraries : string list;
        children : bindings;
      }
    | Executables of executables

  and bindings = (string * dir) list

  let rec index (b : bindings) : string list -> executables option = function
    | [] -> assert false
    | p :: ps ->
        List.find_opt (fst >> ( = ) p) b
        |> Fun.flip Option.bind (fun (_, c) ->
               match (ps, c) with
               | [], Executables e -> Some e
               | _ :: _, Group { children; _ } -> index children ps
               | _ -> None)

  let group ?package ?(libraries = []) ?ppx:_ children =
    Group { package; libraries; children }

  let executables ~mode ?expect_failure ?ppx ?sanitize () =
    let expect_failure = Bool.of_option_flag expect_failure in
    Executables { mode; expect_failure; ppx; sanitize }

  type spec = {
    package : string option;
    libraries : string list;
    file : string;
    ppx : string option;
    groups : (string * dir) list;
  }

  let iter_leaves (f : string list -> executables -> unit) t =
    let rec inner path = function
      | Executables e -> f (List.rev path) e
      | Group { children; _ } ->
          List.iter (fun (name, c) -> inner (name :: path) c) children
    in
    List.iter (fun (name, c) -> inner [ name ] c) t.groups

  let get_package_and_libraries t : string list -> string option * string list =
    let rec inner acc b = function
      | [] -> acc
      | p :: ps -> (
          let child =
            List.find_map (fun (name, c) -> if name = p then Some c else None) b
          in
          match (child, ps) with
          | Some (Executables _), [] -> acc
          | Some (Group { package; libraries; children }), _ :: _ ->
              let acc =
                ( (match package with Some _ as p -> p | None -> fst acc),
                  libraries @ snd acc )
              in
              inner acc children ps
          | _ -> assert false )
    in
    inner (t.package, t.libraries) t.groups

  let v ?package ?(libraries = []) ?ppx ~this_file:file groups =
    { package; libraries; file; ppx; groups }
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

let output_stanzas (suite : Dsl.spec) ~expect_failure =
  let pp_library ppf base =
    (* If the PPX will fail, we don't need to declare the file as executable *)
    if not expect_failure then
      Format.fprintf ppf
        "; The PPX-dependent executable under test@,\
         @[<v 1>(executable@ (name %s)@ (modules %s)@ (preprocess (pps \
         %s))%t)@]"
        base base (suite.ppx |> Option.get)
        (pp_library_dependencies suite)
    else ()
  in
  let pp_rule ppf base =
    let pp_action ppf expect_failure =
      Format.fprintf ppf
        ( if expect_failure then
          "; expect the process to fail, capturing stderr@,\
           @[<v 1>(with-stderr-to@,\
           %%{targets}@,\
           (bash \"! ./%%{pp} -no-color --impl %%{input}\"))@]"
        else "(run %%{bin:ppx-tester-pp} --impl %%{input} -o %%{targets})" )
    in
    Format.fprintf ppf
      "; Run the PPX on the `.ml` file@,\
       @[<v 1>(rule@,\
       (targets %s.actual)@,\
       @[<v 1>(deps@,\
       (:input %s.ml))@]@,\
       @[<v 1>(action@,\
       %a))@]@]"
      base base pp_action expect_failure
  in
  let pp_diff_alias ppf base =
    Format.fprintf ppf
      "; Compare the post-processed output to the .expected file@,\
       @[<v 1>(rule@,\
       (alias runtest)@,\
       %t@[<v 1>(action@,\
       @[<hov 2>(diff@ %s.expected@ %s.actual)@])@])@]" (pp_package suite) base
      base
  in
  let pp_run_alias ppf base =
    (* If we expect the derivation to succeed, then we should be able to compile
       the output. *)
    if not expect_failure then
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
  fun ppf Test_case.{ name; _ } ->
    Format.fprintf ppf
      "@[<v 0>; -------- Test: `%s.ml` --------@,@,%a@,@,%a@,@,%a%a@,@]@." name
      pp_library name pp_rule name pp_diff_alias name pp_run_alias name

let env_defined = Sys.getenv_opt >> function Some _ -> true | None -> false

(* Starting from the current working directory, navigate upwards until a
   [dune-project] file is found. *)
let goto_project_root () =
  let inside_dune =
    env_defined "INSIDE_DUNE" && not (env_defined "INSIDE_CRAM")
  in
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
let fetch_test_cases ~(dir : string) : Test_case.t list =
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
             let has_expected = Str_set.mem ".expected" extensions in
             let has_opts = Str_set.mem ".opts" extensions in
             Some Test_case.{ name; has_expected; has_opts })

let emit_dune_inc (suite : Dsl.spec) ~path =
  let ppf = Format.std_formatter in
  let case = Dsl.index suite.groups path |> Option.get in
  let dir = Sys.getcwd () in
  if case.expect_failure then ppx_fail_global_stanzas ppf;
  fetch_test_cases ~dir
  |> List.iter (output_stanzas ~expect_failure:case.expect_failure suite ppf);
  Format.fprintf ppf "\n%!"

module Bootstrap = struct
  (* For each case, we need to:
     - set up [dune] to genereate [dune.inc] files the [.ml] files available in the directory
     - create an empty [dune.inc] file
  *)
  let generate_group (suite : Dsl.spec) path =
    let _case = Dsl.index suite.groups path |> Option.get in
    let package, _ = Dsl.get_package_and_libraries suite path in
    let dir = List.fold_left ( / ) (Filename.dirname suite.file) path in
    let dune_file = dir / "dune" in
    let dune_inc_file = dir / "dune.inc" in
    let pp_package ppf =
      match package with
      | Some p -> Format.fprintf ppf "@,(package %s)" p
      | None -> ()
    in
    if not (Sys.file_exists dir) then (
      log (fun f -> f "File '%s' does not yet exist. Generating it..." dir);
      Unix.mkdir dir 0o777 );
    if Sys.file_exists dune_file then false
    else (
      log (fun f ->
          f "File '%s' does not exist yet. Generating it..." dune_file);
      let dune_file = Unix.openfile dune_file [ O_CREAT; O_RDWR ] 0o666 in
      let ppf =
        dune_file
        |> Unix.out_channel_of_descr
        |> Format.formatter_of_out_channel
      in
      Format.fprintf ppf
        {|
(include dune.inc)

(rule
 (targets dune.inc.gen)
 (deps
  (source_tree .))
 (action
  (with-stdout-to
   %%{targets}
   (run %%{exe:../%s.exe} generate --path %s))))

(rule
 (alias runtest)%t
 (action
  (diff dune.inc dune.inc.gen)))
%!|}
        (Filename.chop_extension (Filename.basename suite.file))
        (String.concat "/" path) pp_package;
      Unix.close dune_file;
      Unix.openfile dune_inc_file [ O_CREAT ] 0o666 |> Unix.close;
      true )

  let perform suite =
    goto_project_root ();
    let changes_made =
      Dsl.iter_leaves
        (fun path _case -> ignore (generate_group suite path))
        suite;
      true
    in
    if changes_made then
      log (fun f ->
          f
            "Dune files successfully installed. `dune runtest` again to run \
             the newly-installed files.")
end
