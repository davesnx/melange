let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let f1 a = let a = a * 10 in fun b -> let b = a + b in fun c -> b * 10 + c
let f2 a b = let s = a * 10 + b in fun c d -> ((s * 10) + c) * 10 + d
let f3 a b c = let s = (a * 10 + b) * 10 + c in fun d -> s * 10 + d
let fs1 = ref [| f1 |]
let fs2 = ref [| f2 |]
let fs3 = ref [| f3 |]
let app3 f a b c = f a b c
let app4 f a b c d = f a b c d
let app5 f a b c d e = f a b c d e

let () =
  eq __LOC__ (app3 !fs1.(0) 1 2 3) 123;
  eq __LOC__ (app4 !fs2.(0) 1 2 3 4) 1234;
  eq __LOC__ (app4 !fs3.(0) 1 2 3 4) 1234;
  eq __LOC__ ((app3 !fs2.(0) 1 2 3) 4) 1234;
  eq __LOC__ (app5 (fun a -> let a = a + 1 in fun b c d e -> a + b + c + d + e) 1 2 3 4 5) 16;
  let g ~x ~y = let s = x - y in fun z -> s * z in
  eq __LOC__ (app3 (fun x y -> g ~x ~y) 5 2 3) 9

let () = Mt.from_pair_suites __MODULE__ !suites
