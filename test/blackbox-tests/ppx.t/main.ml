open Scaffold

let () =
  v ~this_file:__FILE__ ~ppx:"ppx_foo"
    [ ("passing", ppx_tests ()); ("failing", ppx_tests ()) ]
  |> declare
