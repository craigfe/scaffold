let ( >> ) f g x = g (f x)
let under2 f b x y = b (f x) (f y)

let log :
    type a. (((a, Format.formatter, unit, unit) format4 -> a) -> unit) -> unit =
 fun f ->
  f Format.eprintf;
  Format.eprintf "\n%!"

module Bool = struct
  let of_option_flag = function Some () -> true | None -> false
end

module Option = struct
  let map f = function Some x -> Some (f x) | None -> None
  let bind x f = match x with Some x -> f x | None -> None
  let get = function Some x -> x | None -> failwith "Option.get None"
  let some x = Some x
  let fold ~none ~some = function None -> none | Some x -> some x
end

module Fun = struct
  let id x = x
  let flip f a b = f b a
end

module List = struct
  include List

  let filter_map f =
    let rec aux accu = function
      | [] -> List.rev accu
      | x :: l -> (
          match f x with None -> aux accu l | Some v -> aux (v :: accu) l )
    in
    aux []

  let rec find_opt p = function
    | [] -> None
    | x :: l -> if p x then Some x else find_opt p l

  let rec find_map f = function
    | [] -> None
    | x :: l -> (
        match f x with Some _ as result -> result | None -> find_map f l )

  let group ~break =
    List.fold_left
      (fun acc x ->
        match acc with
        | [] -> [ [ x ] ]
        | (last :: _ as current_group) :: gs ->
            if break last x then [ x ] :: List.rev current_group :: gs
            else (x :: current_group) :: gs
        | [] :: _ ->
            (* Empty groups are never created *)
            assert false)
      []
    >> function
    | [] -> []
    | x :: xs -> List.rev (List.rev x :: xs)
end
