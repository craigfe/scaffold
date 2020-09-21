open Scaffold

let () =
  v ~this_file:__FILE__ ~ppx:"ppx_foo"
    [
      ("passing", executables ~mode:`Ppx_expect ());
      ("failing", executables ~mode:`Ppx_expect ());
    ]
  |> declare
