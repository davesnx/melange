let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let sp fmt = (Sys.opaque_identity Printf.sprintf) fmt
let ksp k fmt = (Sys.opaque_identity Printf.ksprintf) k fmt
let asp fmt = (Sys.opaque_identity Format.asprintf) fmt
let pp () x = "<" ^ string_of_int x ^ ">"
let tt () = "T"
let wrap s = "[" ^ s ^ "]"

let () =
  let s = "a\"b\\c\n\tz\200\255" in
  let n = -1234567 and f = 1. /. 3. in
  eq __LOC__ (Printf.sprintf "plain %% text%!@]") (sp "plain %% text%!@]");
  eq __LOC__ (Printf.sprintf "%s: %d %i" s n n) (sp "%s: %d %i" s n n);
  eq __LOC__ (Printf.sprintf "%d%d" 1 2) (sp "%d%d" 1 2);
  eq __LOC__
    (Printf.sprintf "%5d|%-5d|%05d|%+d|% d|%x|%#X|%o|%u|%.3d|%8.3x" n 7 (-7) 7 7
       255 255 8 (-1) 7 255)
    (sp "%5d|%-5d|%05d|%+d|% d|%x|%#X|%o|%u|%.3d|%8.3x" n 7 (-7) 7 7 255 255 8
       (-1) 7 255);
  eq __LOC__
    (Printf.sprintf "%*d|%-*s|%.*f" (-6) 3 4 "ab" 2 f)
    (sp "%*d|%-*s|%.*f" (-6) 3 4 "ab" 2 f);
  eq __LOC__
    (Printf.sprintf "%f %e %E %g %G %.0f %010.2f %+f" f 1e21 f 1e-7 f 2.5 nan
       neg_infinity)
    (sp "%f %e %E %g %G %.0f %010.2f %+f" f 1e21 f 1e-7 f 2.5 nan neg_infinity);
  eq __LOC__
    (Printf.sprintf "%b %B %6b %c %c" true false true 'a' '\233')
    (sp "%b %B %6b %c %c" true false true 'a' '\233');
  eq __LOC__
    (Printf.sprintf "%S %10S %C %C %C" s "q" '\'' '"' '\200')
    (sp "%S %10S %C %C %C" s "q" '\'' '"' '\200');
  eq __LOC__ (Printf.sprintf "%a %t %a" pp 1 tt pp 2) (sp "%a %t %a" pp 1 tt pp 2);
  eq __LOC__
    (Printf.sprintf "%ld %Lx %5Ld" (-5l) 255L Int64.min_int)
    (sp "%ld %Lx %5Ld" (-5l) 255L Int64.min_int);
  eq __LOC__ (Printf.sprintf {js|é %s → %d|js} "x" 3) (sp {js|é %s → %d|js} "x" 3);
  eq __LOC__ (Printf.sprintf "%F %h %#d" f f 1234) (sp "%F %h %#d" f f 1234);
  eq __LOC__ (Printf.ksprintf wrap "%s=%d" "k" 1) (ksp wrap "%s=%d" "k" 1);
  eq __LOC__ (Format.asprintf "%s=%d" "k" 1) (asp "%s=%d" "k" 1);
  eq __LOC__ (Format.asprintf "@[%s@ %d@]" "k" 1) (asp "@[%s@ %d@]" "k" 1);
  eq __LOC__
    (String.concat "," (List.map (Printf.sprintf "m%d") [ 1; 2 ]))
    "m1,m2";
  eq __LOC__ (Printf.sprintf {js|é@"%d|js} 1) (sp {js|é@"%d|js} 1);
  eq __LOC__ (Printf.sprintf {js|é@\n%d|js} 1) (sp {js|é@\n%d|js} 1);
  eq __LOC__ (Printf.sprintf {js|@\n%d|js} 1) (sp {js|@\n%d|js} 1);
  eq __LOC__ (Printf.sprintf {js|%d\%%é|js} 1) (sp {js|%d\%%é|js} 1);
  eq __LOC__ (Printf.sprintf {js|%d@\té|js} 1) (sp {js|%d@\té|js} 1);
  eq __LOC__ (Printf.sprintf {js|é@\\%d|js} 1) (sp {js|é@\\%d|js} 1);
  eq __LOC__
    (Printf.ksprintf wrap {js|→@"%d|js} 1)
    (ksp wrap {js|→@"%d|js} 1);
  eq __LOC__
    (Printf.ksprintf wrap {js|%d\%%é@\n|js} 1)
    (ksp wrap {js|%d\%%é@\n|js} 1);
  eq __LOC__ (Format.asprintf {js|%d\%%é|js} 1) (asp {js|%d\%%é|js} 1)

let () =
  let log = ref [] in
  let note x = log := x :: !log in
  let arg x () = note x; x in
  let pp_note () x = note ("pp " ^ x); x in
  let r =
    Printf.sprintf "%s %a %s" (arg "a" ()) pp_note (arg "b" ()) (arg "c" ())
  in
  eq __LOC__ (r, List.rev !log) ("a b c", [ "a"; "b"; "c"; "pp b" ]);
  log := [];
  let r =
    match Printf.sprintf "%a %s" pp_note "x" (failwith "arg") with
    | s -> s
    | exception Failure m -> m
  in
  eq __LOC__ (r, List.rev !log) ("arg", [])

let () = Mt.from_pair_suites __MODULE__ !suites
