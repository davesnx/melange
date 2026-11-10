val program : Lam.t -> Lam.t
(** Inside function bodies, rebuild constant blocks nested deeper than 3 levels
    so that no emitted literal is deeper than 3: V8 allocates deeper constant
    literals through a slow runtime path. Each evaluation still allocates. *)
