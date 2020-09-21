let ( >> ) f g x = g (f x)
let under2 f b x y = b (f x) (f y)

let log :
    type a. (((a, Format.formatter, unit, unit) format4 -> a) -> unit) -> unit =
 fun f ->
  f Format.eprintf;
  Format.eprintf "\n%!"

module Bool = struct
  include Bool

  let of_option_flag = function Some () -> true | None -> false
end

module List = struct
  include List

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
