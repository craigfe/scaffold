open Utils
module Tc = Dsl.Test_case

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

let pp_library_dependencies (suite : Dsl.spec) ppf =
  match suite.libraries with
  | [] -> ()
  | ls -> Fmt.pf ppf "@,(libraries %a)" Fmt.(list ~sep:sp string) ls

let pp_package (suite : Dsl.spec) ppf =
  match suite.package with
  | None -> ()
  | Some p -> Fmt.pf ppf "@,(package %s)" p

let pp_ppx (suite : Dsl.spec) ppf =
  match suite.ppx with
  | None -> ()
  | Some p -> Fmt.pf ppf "@,(preprocess (pps %s))" p

let pp_rule (test_case : Tc.t) (case : Dsl.executables) ppf =
  let with_styler fmt ppf =
    match case.expect_state with
    | Output _ -> failwith "TODO"
    | Ppxed_ast { styler; _ } -> (
        match styler with
        | Some s -> Fmt.pf ppf "(pipe-stdout %t (run %s))" fmt s
        | None -> fmt ppf )
  in
  let pp_action ppf =
    match case.expect_state with
    | Output _ -> failwith "TODO"
    | Ppxed_ast _ ->
        let pp_bin = "%{exe:../pp.exe}" (* TODO: determine path properly *) in
        Format.fprintf ppf
          ( if case.expect_failure then
            "; expect the process to fail, capturing stderr@,\
             @[<v 1>(with-stderr-to@,\
             %%{targets}@,\
             (bash \"! %s -no-color --impl %%{input}\"))@]"
            (* TODO: use (with-accepted-exit-codes) *)
          else "(run %s --impl %%{input})" )
          pp_bin
  in
  Format.fprintf ppf
    "; Run the PPX on the `.ml` file@,\
     @[<v 1>(rule@,\
     (targets %s.actual)@,\
     @[<v 1>(deps@,\
     (:input %s.ml))@]@,\
     @[<v 1>(action@,\
     @[<v 1>(with-stdout-to@,\
     %%{targets}@,\
     %t)@])@])@]"
    test_case.name test_case.name (with_styler pp_action)

let output_stanzas (suite : Dsl.spec) ~(case : Dsl.executables) =
  match case.expect_state with
  | Output _ -> failwith "TODO"
  | Ppxed_ast { and_then; _ } ->
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
           @[<hov 2>(diff@ %s%s@ %s.actual)@])@])@]" Tc.expected_ext
          (pp_package suite) base Tc.expected_ext base
      in
      let pp_run_alias ppf base =
        match (case.expect_failure, and_then) with
        | true, (`Build | `Run) ->
            failwith
              "Cannot build or run a PPXed executable where the PPX is \
               expected to fail"
            (* TODO: Handle DSL errors in a consistent way *)
        | (true | false), `Noop -> ()
        | false, `Build ->
            Fmt.pf ppf
              "@,\
               @,\
               ; Ensure that the post-process executable builds correctly@,\
               @[<v 1>(rule@,\
               (alias runtest)@,\
               (deps %s.exe)@,\
               %t(action progn))@]" base (pp_package suite)
        | false, `Run ->
            Fmt.pf ppf
              "@,\
               @,\
               ; Ensure that the post-processed executable runs correctly@,\
               @[<v 1>(rule@,\
               (alias runtest)@,\
               %t@[<v 1>(action@,\
               @[<hov 2>(run@ ./%s.exe)@])@])@]" (pp_package suite) base
      in
      fun ppf (Tc.{ name; _ } as tc) ->
        let header =
          let left = Fmt.str "; -------- Test: `%s.ml` " name in
          let right = String.make (max 0 (80 - String.length left)) '-' in
          left ^ right
        in
        Format.fprintf ppf "@[<v 0>%s@,@,%a@,@,%t@,@,%a%a@,@]@." header
          pp_library name (pp_rule tc case) pp_diff_alias name pp_run_alias name

let remove_extension =
  let rec inner f =
    try inner (Filename.chop_extension f) with Invalid_argument _ -> f
  in
  inner

module Str_set = Set.Make (String)

(* Each test case has a single basename and a set of file extensions ([.ml],
   [.expected] and optionally [.opts]). *)
let fetch_test_cases ~(dir : string) : Tc.t list option =
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
               let has_expected = Str_set.mem Tc.expected_ext extensions in
               let has_opts = Str_set.mem Tc.opts_ext extensions in
               Some Tc.{ name; has_expected; has_opts })
    |> Option.some
  else None

let emit_toplevel_dune_inc (suite : Dsl.spec) =
  match suite.ppx with
  | None -> ()
  | Some s ->
      Fmt.pr
        {|(rule
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
