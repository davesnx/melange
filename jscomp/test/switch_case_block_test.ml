let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

type t = A of int * int | B of string | C of int list | D of int

let make t =
  match t with
  | A (x, y) ->
      let s = x + y in
      fun k -> k * s
  | B s ->
      let n = String.length s in
      fun k -> k + n
  | C l ->
      let n = List.length l in
      fun k -> k - n
  | D d -> fun k -> k + d

let () =
  let fs = List.map make [ A (1, 2); B "abcd"; C [ 1; 2 ]; D 5 ] in
  eq __LOC__ (List.map (fun f -> f 10) fs) [ 30; 14; 8; 15 ]

let () = Mt.from_pair_suites __MODULE__ !suites
