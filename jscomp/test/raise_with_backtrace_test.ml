exception User of string

let result f =
  match f () with
  | v -> "ok " ^ string_of_int v
  | exception Fun.Finally_raised (Failure m) -> "Finally_raised " ^ m
  | exception User m -> "User " ^ m
  | exception Not_found -> "Not_found"
  | exception e -> "other " ^ Printexc.to_string e

let () =
  Mt.from_pair_suites __MODULE__
    [
      ( "raise_with_backtrace",
        fun () ->
          Mt.Eq
            ( "User bt",
              result (fun () ->
                  Printexc.raise_with_backtrace (User "bt")
                    (Printexc.get_raw_backtrace ())) ) );
      ( "Fun.protect re-raises",
        fun () ->
          Mt.Eq
            ( "Not_found",
              result (fun () ->
                  Fun.protect ~finally:ignore (fun () -> raise Not_found)) ) );
      ( "Fun.protect Finally_raised",
        fun () ->
          Mt.Eq
            ( "Finally_raised fin",
              result (fun () ->
                  Fun.protect ~finally:(fun () -> failwith "fin") (fun () -> 1))
            ) );
    ]
