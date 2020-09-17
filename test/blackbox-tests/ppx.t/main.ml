open Scaffold

let suite =
  v ~this_file:__FILE__ ~ppx:"ppx_alcotest"
    [
      ("passing", executables ~mode:`Ppx_expect ());
      ("failing", executables ~mode:`Ppx_expect ());
    ]
  |> declare
