open Matrixmarket.Matrix

let () =
  match Sys.argv with
  | [| _; "reformat" |] ->
    parse stdin |> fun (W d, comments) ->
    (match d.kind.format with
     | Array ->
       let get m i j = m.(i - 1).(j - 1) in
       to_array d |> fun a ->
       Format.printf "%a@." (output_dense d.kind get d.rows d.columns comments) a
     | Coordinate ->
       Format.printf "%a@."
         (output_sparse_s d.kind.field d.rows d.columns comments (List.length d.data))
         (Stream.of_list d.data))
  | [| _ |] ->
    parse stdin |> fun (W description, comments) ->
    comments |> List.iter prerr_endline;
    let to_string = string_of_value description.kind.field in
    to_array description |>
    Array.iter (fun row ->
      row |> Array.iter (fun x -> Printf.printf "%s\t" (to_string x));
      Printf.printf "\n";
    )
  | _ ->
    Printf.eprintf "usage: %s [reformat]\n" Sys.argv.(0);
    exit 1
