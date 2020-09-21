open Ppxlib

let () =
  Driver.register_transformation
    ~extensions:
      [
        Extension.declare "foo" Extension.Context.expression
          Ast_pattern.(pstr nil)
          (fun ~loc ~path:_ -> Ast_builder.Default.estring ~loc "bar");
      ]
    "foo"
