(* FIXME don't ask for that much *)
module Make (G : Graph.Sig.G) = struct
  module H = Hashtbl.Make (G.V)

  let index g =
    let h = H.create (G.nb_vertex g) in
    let a =
      G.fold_vertex (fun v (i, acc) ->
        H.replace h v i;
        i + 1, v :: acc
      ) g (0, []) |>
      snd |>
      List.rev |>
      Array.of_list
    in
    h, a

  let output_dense kind comments ppf g =
    let _, a = index g in
    let get g i j =
      try
        G.E.label (G.find_edge g a.(i) a.(j))
      with Not_found ->
        Matrixmarket.(zero kind.field)
    in
    let n = G.nb_vertex g in
    Matrixmarket.output_dense kind get n n comments ppf g

  let output_sparse field comments ppf g =
    let h, a = index g in
    let n, n' = G.nb_vertex g, G.nb_edges g in
    let s = ref (0, G.succ_e g a.(0)) in
    let rec f pos =
      match !s with
      | i, e :: es ->
        s := i, es;
        let j = H.find h (G.E.dst e) in
        if G.is_directed || i >= j then
          Some (i + 1, j + 1, G.E.label e)
        else
          f pos
      | i, [] ->
        let i = i + 1 in
        if i = n then
          None
        else (
          s := i, G.succ_e g a.(i);
          f pos
        )
    in
    if G.is_directed then
      Matrixmarket.output_sparse_s field General n n comments n' ppf (Stream.from f)
    else
      Matrixmarket.output_sparse_s field Symmetric n n comments n' ppf (Stream.from f)
end
