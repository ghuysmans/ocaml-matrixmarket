open Matrixmarket

let () =
  parse stdin |> fun (W description, _comments) ->
  let to_string = string_of_value description.kind.field in
  to_array description |>
  Array.iter (fun row ->
    row |>
    Array.map to_string |>
    Array.to_list |>
    String.concat "," |>
    Printf.printf "%s\n"
  )
