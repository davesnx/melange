let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

type t = Leaf | Node of int * t

let deep () = Node (1, Node (2, Node (3, Node (4, Node (5, Leaf)))))
let list () = [ 1; 2; 3; 4; 5; 6; 7 ]
let rec sum = function Leaf -> 0 | Node (n, t) -> n + sum t

let () =
  eq __LOC__ (sum (deep ())) 15;
  eq __LOC__ (List.fold_left ( + ) 0 (list ())) 28;
  eq __LOC__ (deep () == deep ()) false;
  eq __LOC__ (deep () = deep ()) true;
  eq __LOC__ (Printf.sprintf "%s: %d|%5d" "a" 1 2) "a: 1|    2"

let () = Mt.from_pair_suites __MODULE__ !suites
