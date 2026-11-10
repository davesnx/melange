open Import

let max_depth = 3

let rec depth (c : Lam.Constant.t) =
  match c with
  | Const_block (_, _, xs) ->
      1 + List.fold_left xs ~init:0 ~f:(fun acc x -> max acc (depth x))
  | Const_some x -> depth x
  | Const_int64 _ | Const_float_array _ -> 1
  | _ -> 0

let split (c : Lam.Constant.t) =
  let binds = ref [] in
  let rec cut (c : Lam.Constant.t) d =
    match c with
    | Const_block (tag, info, xs) when depth c > max_depth + 1 - d ->
        let args =
          List.map xs ~f:(fun x ->
              if d >= max_depth && depth x > 0 then (
                let inner = cut x 1 in
                let id = Ident.create_tmp () in
                binds := (id, inner) :: !binds;
                Lam.var id)
              else cut x (d + 1))
        in
        Lam.prim
          ~primitive:(Pmakeblock (tag, info, Immutable))
          ~args ~loc:Location.none
    | c -> Lam.const c
  in
  let body = cut c 1 in
  List.fold_left !binds ~init:body ~f:(fun acc (id, lam) ->
      Lam.let_ Strict id lam acc)

let program lam =
  let rec walk ~in_fn (lam : Lam.t) =
    match lam with
    | Lfunction f ->
        Lam.function_ ~attr:f.attr ~arity:f.arity ~params:f.params
          ~body:(walk ~in_fn:true f.body)
    | Lconst c when in_fn && depth c > max_depth -> split c
    | _ -> Lam.inner_map lam (walk ~in_fn)
  in
  walk ~in_fn:false lam
