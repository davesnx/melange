let () =
  let ran = ref [] in
  at_exit (fun () -> ran := "A" :: !ran);
  at_exit (fun () -> ran := "B" :: !ran);
  Stdlib.do_at_exit ();
  let first = List.rev !ran in
  Stdlib.do_at_exit ();
  let second = List.rev !ran in
  Mt.from_pair_suites __MODULE__
    [
      ("callbacks run newest first", fun () -> Mt.Eq ([ "B"; "A" ], first));
      ("callbacks run once", fun () -> Mt.Eq ([ "B"; "A" ], second));
    ]
