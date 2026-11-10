open Import

exception Fallback

type pad = No_pad | Lit_pad of int * int | Arg_pad of int
type prec = No_prec | Lit_prec of int | Arg_prec

type conv =
  | Str
  | Bool
  | Char
  | Int of string * string
  | Caml_string
  | Caml_char
  | Alpha
  | Theta
  | Float of string * string

type piece = Text of string * bool | Hole of conv * pad * prec

type callee =
  | Printf_sprintf
  | Printf_ksprintf
  | Format_sprintf
  | Format_kasprintf

let level =
  match Sys.getenv_opt "MELANGE_LOWER_PRINTF" with
  | None -> 5
  | Some s -> ( match int_of_string_opt s with Some n -> n | None -> 5)

let enabled c =
  level >= 5
  &&
  match Sys.getenv_opt "MELANGE_LOWER_PRINTF_PARTS" with
  | None -> c <> 'I'
  | Some s -> String.contains s c

let view (c : Lam.Constant.t) =
  match c with
  | Const_int
      { comment = Pt_constructor { name = { name; as_modifier = None }; _ }; _ }
    ->
      (name, [])
  | Const_block (_, Blk_constructor { name; attributes = []; _ }, args) ->
      (name, args)
  | _ -> raise Fallback

let int_of (c : Lam.Constant.t) =
  match c with
  | Const_int { i; comment = None } -> Int32.to_int i
  | _ -> raise Fallback

let padty c =
  match view c with
  | "Left", [] -> 0
  | "Right", [] -> 1
  | "Zeros", [] -> 2
  | _ -> raise Fallback

let pad c =
  match view c with
  | "No_padding", [] -> No_pad
  | "Lit_padding", [ t; w ] -> Lit_pad (padty t, int_of w)
  | "Arg_padding", [ t ] -> Arg_pad (padty t)
  | _ -> raise Fallback

let prec c =
  match view c with
  | "No_precision", [] -> No_prec
  | "Arg_precision", [] -> Arg_prec
  | "Lit_precision", [ p ] -> Lit_prec (int_of p)
  | _ -> raise Fallback

let iconv c =
  match view c with
  | "Int_d", [] -> "%d"
  | "Int_pd", [] -> "%+d"
  | "Int_sd", [] -> "% d"
  | "Int_i", [] -> "%i"
  | "Int_pi", [] -> "%+i"
  | "Int_si", [] -> "% i"
  | "Int_x", [] -> "%x"
  | "Int_Cx", [] -> "%#x"
  | "Int_X", [] -> "%X"
  | "Int_CX", [] -> "%#X"
  | "Int_o", [] -> "%o"
  | "Int_Co", [] -> "%#o"
  | "Int_u", [] -> "%u"
  | _ -> raise Fallback

let fconv (c : Lam.Constant.t) =
  match c with
  | Const_block (0, Blk_tuple, [ flag; kind ]) ->
      let flag =
        match view flag with
        | "Float_flag_", [] -> "%"
        | "Float_flag_p", [] -> "%+"
        | "Float_flag_s", [] -> "% "
        | _ -> raise Fallback
      in
      let kind =
        match view kind with
        | "Float_f", [] -> "f"
        | "Float_e", [] -> "e"
        | "Float_E", [] -> "E"
        | "Float_g", [] -> "g"
        | "Float_G", [] -> "G"
        | _ -> raise Fallback
      in
      Float (flag, kind)
  | _ -> raise Fallback

let text (c : Lam.Constant.t) =
  match c with
  | Const_string { s; unicode } -> Text (s, unicode)
  | _ -> raise Fallback

let formatting_lit c =
  match view c with
  | "Close_box", [] -> Text ("@]", false)
  | "Close_tag", [] -> Text ("@}", false)
  | "FFlush", [] -> Text ("@?", false)
  | "Force_newline", [] -> Text ("@\n", false)
  | "Flush_newline", [] -> Text ("@.", false)
  | "Escaped_at", [] -> Text ("@@", false)
  | "Escaped_percent", [] -> Text ("@%", false)
  | "Break", [ s; _; _ ] | "Magic_size", [ s; _ ] -> text s
  | "Scan_indic", [ Const_char ch ] -> Text ("@" ^ String.make 1 ch, false)
  | _ -> raise Fallback

let pieces ~printf c =
  let rec walk c acc =
    match view c with
    | "End_of_format", [] -> List.rev acc
    | "String_literal", [ s; rest ] -> walk rest (text s :: acc)
    | "Char_literal", [ Const_char ch; rest ] ->
        walk rest (Text (String.make 1 ch, false) :: acc)
    | "String", [ p; rest ] -> walk rest (Hole (Str, pad p, No_prec) :: acc)
    | "Bool", [ p; rest ] -> walk rest (Hole (Bool, pad p, No_prec) :: acc)
    | "Char", [ rest ] -> walk rest (Hole (Char, No_pad, No_prec) :: acc)
    | "Int", [ ic; p; pr; rest ] ->
        walk rest
          (Hole (Int (iconv ic, "caml_format_int"), pad p, prec pr) :: acc)
    | (("Int32" | "Nativeint" | "Int64") as k), [ ic; p; pr; rest ]
      when enabled 'L' ->
        let spec = iconv ic in
        let letter, prim =
          match k with
          | "Int32" -> ("l", "caml_int32_format")
          | "Nativeint" -> ("n", "caml_nativeint_format")
          | _ -> ("L", "caml_int64_format")
        in
        let n = String.length spec - 1 in
        let spec =
          String.sub spec ~pos:0 ~len:n ^ letter ^ String.sub spec ~pos:n ~len:1
        in
        walk rest (Hole (Int (spec, prim), pad p, prec pr) :: acc)
    | "Caml_string", [ p; rest ] when enabled 'S' ->
        walk rest (Hole (Caml_string, pad p, No_prec) :: acc)
    | "Caml_char", [ rest ] when enabled 'S' ->
        walk rest (Hole (Caml_char, No_pad, No_prec) :: acc)
    | "Alpha", [ rest ] when printf && enabled 'P' ->
        walk rest (Hole (Alpha, No_pad, No_prec) :: acc)
    | "Theta", [ rest ] when printf && enabled 'P' ->
        walk rest (Hole (Theta, No_pad, No_prec) :: acc)
    | "Float", [ fc; p; pr; rest ] ->
        walk rest (Hole (fconv fc, pad p, prec pr) :: acc)
    | "Flush", [ rest ] when printf -> walk rest acc
    | "Formatting_lit", [ fl; rest ] when printf ->
        walk rest (formatting_lit fl :: acc)
    | _ -> raise Fallback
  in
  match view c with
  | "Format", [ fmt; Const_string _ ] -> walk fmt []
  | _ -> raise Fallback

let is_plain_int (spec, prim) =
  prim = "caml_format_int" && (spec = "%d" || (spec = "%i" && not (enabled 'I')))

let stage1 = function
  | Text _ | Hole (Str, No_pad, No_prec) -> true
  | Hole (Int (spec, prim), No_pad, No_prec) -> is_plain_int (spec, prim)
  | Hole _ -> false

let is_printer = function Hole ((Alpha | Theta), _, _) -> true | _ -> false

let arity = function
  | Text _ -> 0
  | Hole (Alpha, _, _) -> 2
  | Hole (_, p, pr) -> (
      1
      + (match p with Arg_pad _ -> 1 | No_pad | Lit_pad _ -> 0)
      + match pr with Arg_prec -> 1 | No_prec | Lit_prec _ -> 0)

let rec merge = function
  | Text (a, ua) :: Text (b, ub) :: rest when ua = ub ->
      merge (Text (a ^ b, ua) :: rest)
  | p :: rest -> p :: merge rest
  | [] -> []

let callee (f : Lam.t) =
  match f with
  | Lprim
      {
        primitive = Pfield (_, Fld_module { name });
        args = [ Lglobal_module { id; dynamic_import = false } ];
        _;
      } -> (
      match (Ident.name id, name) with
      | "Stdlib__Printf", "sprintf" -> Printf_sprintf
      | "Stdlib__Printf", "ksprintf" when level >= 3 -> Printf_ksprintf
      | "Stdlib__Format", ("sprintf" | "asprintf") when level >= 4 ->
          Format_sprintf
      | "Stdlib__Format", "kasprintf" when level >= 4 -> Format_kasprintf
      | _ -> raise Fallback)
  | _ -> raise Fallback

let rec flatten (lam : Lam.t) acc =
  match lam with
  | Lapply { ap_func; ap_args; _ } -> flatten ap_func (ap_args @ acc)
  | f -> (f, acc)

let format_constant tbl (l : Lam.t) =
  match l with
  | Lconst c -> c
  | Lvar v -> (
      match Ident.Hashtbl.find tbl v with
      | Lam_id_kind.Constant c -> c
      | _ | (exception Not_found) -> raise Fallback)
  | _ -> raise Fallback

let str s unicode = Lam.const (Const_string { s; unicode })
let int n = Lam.const (Const_int { i = Int32.of_int n; comment = None })

let lower_exn tbl (lam : Lam.t) =
  let ap_info =
    match lam with Lapply { ap_info; _ } -> ap_info | _ -> raise Fallback
  in
  let loc = ap_info.ap_loc in
  let f, args = flatten lam [] in
  let kind = callee f in
  let k, fmt, args =
    match (kind, args) with
    | (Printf_sprintf | Format_sprintf), fmt :: args -> (None, fmt, args)
    | (Printf_ksprintf | Format_kasprintf), k :: fmt :: args ->
        (Some k, fmt, args)
    | _ -> raise Fallback
  in
  let printf =
    match kind with
    | Printf_sprintf | Printf_ksprintf -> true
    | Format_sprintf | Format_kasprintf -> false
  in
  let pieces = pieces ~printf (format_constant tbl fmt) in
  if level < 2 && not (List.for_all ~f:stage1 pieces) then raise Fallback;
  if
    List.fold_left ~f:(fun n p -> n + arity p) ~init:0 pieces
    <> List.length args
  then raise Fallback;
  let ccall name args =
    Lam.prim ~primitive:(Pccall { prim_name = name }) ~args ~loc
  in
  let js_call ~scopes name x =
    Lam.prim
      ~primitive:
        (Pjs_call
           {
             prim_name = name;
             arg_types = [ Melange_ffi.External_arg_spec.dummy ];
             ffi =
               Js_call
                 { name; external_module_name = None; variadic = false; scopes };
             dynamic_import = false;
           })
      ~args:[ x ] ~loc
  in
  let simple (l : Lam.t) =
    match l with
    | Lconst _ | Lvar _ | Lmutvar _ | Lfunction _ -> true
    | _ -> false
  in
  let binds = ref [] in
  let bind (l : Lam.t) =
    if simple l then l
    else
      let v = Ident.create_local "arg" in
      binds := (v, l) :: !binds;
      Lam.var v
  in
  let k, args =
    let rec first_printer pos = function
      | [] -> None
      | p :: ps ->
          if is_printer p then Some (pos + arity p)
          else first_printer (pos + arity p) ps
    in
    match first_printer 0 pieces with
    | Some after
      when List.exists
             ~f:(fun a -> not (simple a))
             (List.filteri ~f:(fun i _ -> i >= after) args) ->
        let k = Option.map ~f:bind k in
        (k, List.map ~f:bind args)
    | _ -> (k, args)
  in
  let rest = ref args in
  let next () =
    match !rest with
    | a :: tl ->
        rest := tl;
        a
    | [] -> raise Fallback
  in
  let part = function
    | Text (s, u) -> str s u
    | Hole (conv, p, pr) -> (
        let w = match p with Arg_pad _ -> Some (next ()) | _ -> None in
        let pv = match pr with Arg_prec -> Some (next ()) | _ -> None in
        let x = next () in
        let prec_arg () =
          match (pr, pv) with
          | Lit_prec n, _ -> int n
          | Arg_prec, Some v -> v
          | _ -> raise Fallback
        in
        let s =
          match conv with
          | Str -> x
          | Bool -> Lam.if_ x (str "true" false) (str "false" false)
          | Char -> js_call ~scopes:[ "String" ] "fromCharCode" x
          | Caml_string -> ccall "caml_printf_caml_string" [ x ]
          | Caml_char -> ccall "caml_printf_caml_char" [ x ]
          | Alpha ->
              let v = next () in
              Lam.apply x [ Lam.unit; v ]
                {
                  ap_loc = loc;
                  ap_inlined = Default_inline;
                  ap_status = App_na;
                }
          | Theta ->
              Lam.apply x [ Lam.unit ]
                {
                  ap_loc = loc;
                  ap_inlined = Default_inline;
                  ap_status = App_na;
                }
          | Int (spec, prim) -> (
              let s =
                if is_plain_int (spec, prim) then js_call ~scopes:[] "String" x
                else ccall prim [ str spec false; x ]
              in
              match pr with
              | No_prec -> s
              | Lit_prec _ | Arg_prec ->
                  ccall "caml_printf_iprec" [ prec_arg (); s ])
          | Float (flag, kind) -> (
              match pr with
              | No_prec ->
                  ccall "caml_format_float"
                    [ str (flag ^ ".6" ^ kind) false; x ]
              | Lit_prec n ->
                  ccall "caml_format_float"
                    [ str (flag ^ "." ^ string_of_int (abs n) ^ kind) false; x ]
              | Arg_prec ->
                  ccall "caml_printf_float"
                    [ str flag false; prec_arg (); str kind false; x ])
        in
        match (p, w) with
        | No_pad, _ -> s
        | Lit_pad (t, n), _ -> ccall "caml_printf_pad" [ int t; int n; s ]
        | Arg_pad t, Some w -> ccall "caml_printf_pad" [ int t; w; s ]
        | Arg_pad _, None -> raise Fallback)
  in
  let body =
    match List.map ~f:part (merge pieces) with
    | [] -> str "" false
    | p :: ps ->
        List.fold_left
          ~f:(fun acc p -> Lam.prim ~primitive:Pstringadd ~args:[ acc; p ] ~loc)
          ~init:p ps
  in
  let body =
    match k with
    | None -> body
    | Some k ->
        Lam.apply k [ body ]
          { ap_loc = loc; ap_inlined = Default_inline; ap_status = App_na }
  in
  List.fold_left
    ~f:(fun body (v, l) -> Lam.let_ Strict v l body)
    ~init:body !binds

let lower tbl (lam : Lam.t) =
  match lam with
  | Lapply _ when level > 0 -> (
      match lower_exn tbl lam with l -> Some l | exception Fallback -> None)
  | _ -> None
