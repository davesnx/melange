let suites : Mt.pair_suites ref = ref []
let test_id = ref 0
let eq loc (a, b) = Mt_global.collect_eq test_id suites loc a b
let log = ref []
let note s = log := s :: !log

let run f =
  log := [];
  let r =
    match f () with
    | s -> "ok " ^ s
    | exception e -> Printexc.to_string e
  in
  r ^ " [" ^ String.concat "; " (List.rev !log) ^ "]"

let pa () x =
  note ("a " ^ x);
  "<" ^ x ^ ">"

let () =
  eq __LOC__
    (run (fun () -> Printf.sprintf "<%(%d-%s%)>%d" "%d=%s" 1 "x" 2), "ok <1=x>2 []");
  eq __LOC__
    (run (fun () -> Printf.sprintf "<%(%(%d%)%)>" "[%(%d%)]" "%5d" 7), "ok <[    7]> []");
  eq __LOC__ (run (fun () -> Printf.sprintf "%(%c%)" "%0c" 'x'), "CamlinternalFormat.Type_mismatch/1 []");
  eq __LOC__
    (run (fun () -> Printf.sprintf "%(%(%d%)%)" "%{%d%}%d" "%d" 3), "CamlinternalFormat.Type_mismatch/1 []");
  eq __LOC__
    (run (fun () -> Printf.sprintf "%(%c%c%)" "%_d%c%!%0c" 'a' 'b'), "CamlinternalFormat.Type_mismatch/1 []");
  eq __LOC__
    ( run (fun () ->
          let f = Printf.sprintf "%d%(%c%)" in
          note "format";
          let g = f 1 in
          note "int";
          let h = g "%0c" in
          note "arg";
          h 'z'),
      "CamlinternalFormat.Type_mismatch/1 [format; int]" );
  eq __LOC__ (run (fun () -> Format.asprintf "%(%c%)" "%0c" 'x'), "CamlinternalFormat.Type_mismatch/1 []");
  eq __LOC__
    ( run (fun () -> Printf.sprintf "a%_db"),
      "Invalid_argument(Printf: bad conversion %_) []" );
  eq __LOC__
    ( run (fun () -> Printf.sprintf "%a%_d%a" pa "1" pa "2"),
      "Invalid_argument(Printf: bad conversion %_) [a 1]" );
  eq __LOC__
    ( run (fun () -> Printf.sprintf "<%_(%(%d%)%a%)%d>" "%d" 3 pa "x" 4),
      "Invalid_argument(Printf: bad conversion %_) []" );
  eq __LOC__
    ( run (fun () ->
          Printf.ikfprintf (fun () -> note "k") () "%_(%d%s%)%d" 1 "s" 2;
          ""),
      "ok  [k]" )

let () = Mt.from_pair_suites __MODULE__ !suites
