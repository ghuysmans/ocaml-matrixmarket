module G = Graph.Pack.Graph
module E = Matrixmarket_graph.Export.Make (G)

let () =
  let g = G.create () in
  let a = G.V.create 1 in
  let b = G.V.create 2 in
  let c = G.V.create 3 in
  G.add_edge_e g (G.E.create a 1 b);
  G.add_edge_e g (G.E.create b 1 c);
  G.add_edge_e g (G.E.create c 1 a);
  Format.printf "%a@." (E.output_sparse Matrixmarket.Integer []) g
