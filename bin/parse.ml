open Matrixmarket.Matrix

let () =
  parse stdin |> fun (W description, comments) ->
  comments |> List.iter prerr_endline;
  let to_string = string_of_value description.kind.field in
  to_array description |>
  Array.iter (fun row ->
    row |> Array.iter (fun x -> Printf.printf "%s\t" (to_string x));
    Printf.printf "\n";
  )
