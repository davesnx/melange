let ( // ) = Ext_path.( // )

type t = NodeJS | Es6 | Es6_global

let default = NodeJS

(* ocamlopt could not optimize such simple case..*)
let compatible ~dep t =
  match t with
  | NodeJS -> dep = NodeJS
  | Es6 -> dep = Es6
  | Es6_global -> dep = Es6_global || dep = Es6
(* As a dependency Leaf Node, it is the same either [global] or [not] *)

(* in runtime lib, [es6] and [es6] are treated the same way *)
let runtime_dir = function NodeJS -> "js" | Es6 | Es6_global -> "es6"
let runtime_package_path js_file = (Literals.package_name ^ ".js") // js_file

let to_string = function
  | NodeJS -> "commonjs"
  | Es6 -> "es6"
  | Es6_global -> "es6-global"

let of_string_exn = function
  | "commonjs" -> NodeJS
  | "es6" -> Es6
  | "es6-global" -> Es6_global
  | s -> raise (Arg.Bad ("invalid module system " ^ s))

let of_string s =
  match of_string_exn s with t -> Some t | exception Arg.Bad _ -> None
