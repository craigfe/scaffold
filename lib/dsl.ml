open Utils

type path = string list

type expect_state =
  | Ppxed_ast of { styler : string option; and_then : [ `Build | `Run ] option }
  | Output of { sanitize : (in_channel -> out_channel -> unit) option }

type executables = {
  ppx : string option;
  expect_failure : bool;
  expect_state : expect_state;
}

type dir =
  | Group of {
      package : string option;
      libraries : string list;
      children : bindings;
    }
  | Executables of executables

and bindings = (string * dir) list

let rec index (b : bindings) : path -> executables option = function
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

let executables ?expect_failure ?ppx ?sanitize () =
  let expect_failure = Bool.of_option_flag expect_failure in
  let expect_state = Output { sanitize } in
  Executables { ppx; expect_failure; expect_state }

let ppx_tests ?expect_failure ?ppx ?styler ?and_then () =
  let expect_failure = Bool.of_option_flag expect_failure in
  let expect_state = Ppxed_ast { styler; and_then } in
  Executables { ppx; expect_failure; expect_state }

type spec = {
  package : string option;
  libraries : string list;
  file : string;
  ppx : string option;
  groups : (string * dir) list;
}

let fold_leaves (type a) (f : path -> executables -> a -> a) t (acc : a) : a =
  let rec inner path acc = function
    | Executables e -> f (List.rev path) e acc
    | Group { children; _ } ->
        List.fold_left
          (fun acc (name, c) -> inner (name :: path) acc c)
          acc children
  in
  List.fold_left (fun acc (name, c) -> inner [ name ] acc c) acc t.groups

(* let iter_leaves (f : path -> executables -> unit) t =
 *   fold_leaves (fun p e () -> f p e) t () *)

let get_package_and_libraries t : path -> string option * string list =
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
