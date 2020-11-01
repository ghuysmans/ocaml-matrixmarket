type ('typ, 'data) format =
  | Coordinate : ('typ, (int * int * 'typ) list) format
  | Array : ('typ, 'typ array) format

type 'a field =
  | Complex : Complex.t field
  | Integer : int field
  | Pattern : bool field
  | Real : float field

let string_of_value : type a. a field -> a -> string = function
  | Complex -> fun Complex.{re; im} -> Printf.sprintf "%f + %fi" re im
  | Integer -> string_of_int
  | Real -> string_of_float
  | Pattern -> function
    | false -> " "
    | true -> "*"

type 'typ symmetry =
  | General : 'typ symmetry
  | Symmetric : 'typ symmetry
  | Hermitian : Complex.t symmetry
  | Skew_symmetric : 'typ symmetry

type ('typ, 'data) kind = {
  format: ('typ, 'data) format;
  field: 'typ field;
  symmetry: 'typ symmetry;
}

type ('typ, 'data) description = {
  kind: ('typ, 'data) kind;
  rows: int;
  columns: int;
  data: 'data;
}

let zero : type a. a field -> a = function
  | Complex -> Complex.zero
  | Integer -> 0
  | Pattern -> false
  | Real -> 0.

let get_symmetric : type a. a field -> a -> a symmetry -> a = fun f x -> function
  | General | Symmetric -> x
  | Hermitian -> Complex.conj x
  | Skew_symmetric ->
    match f with
    | Complex -> Complex.neg x
    | Integer -> -x
    | Pattern -> not x
    | Real -> -. x

module type S = sig
  type 'a t
  val make : int -> int -> 'a -> 'a t
  val set : 'a t -> int -> int -> 'a -> unit
end

let first_for_symmetry : type a. j:int -> a symmetry -> int = fun ~j -> function
  | General -> 1
  | Skew_symmetric -> j + 1
  | _ -> j

module Make (M : S) = struct
  let build : type typ data. (typ, data) description -> typ M.t = fun t ->
    let m = M.make t.rows t.columns (zero t.kind.field) in
    let put i j x =
      M.set m i j x;
      match t.kind.symmetry with
      | General -> ()
      | sym -> M.set m j i (get_symmetric t.kind.field x sym)
    in
    match t.kind.format with
    | Coordinate ->
      t.data |> List.iter (fun (i, j, x) -> put i j x);
      m
    | Array ->
      let pos = ref 0 in
      for j = 1 to t.columns do
        for i = first_for_symmetry ~j t.kind.symmetry to t.rows do
          put i j t.data.(!pos);
          incr pos
        done
      done;
      m
end

module A = Make (struct
  type 'a t = 'a array array
  let make = Array.make_matrix
  let set t i j x = t.(i - 1).(j - 1) <- x
end)

let to_array = A.build

let%test_module _ = (module struct
  let ex1_d = {
    kind = {
      format = Coordinate;
      field = Real;
      symmetry = General;
    };
    rows = 5;
    columns = 5;
    data = [
      1, 1, 1.0;
      2, 2, 10.5;
      4, 2, 250.5;
      3, 3, 0.015;
      1, 4, 6.0;
      4, 4, -280.0;
      4, 5, 33.32;
      5, 5, 12.0;
    ];
  }

  let ex1 = [|
    [| 1.; 0.; 0.; 6.; 0. |];
    [| 0.; 10.5; 0.; 0.; 0. |];
    [| 0.; 0.; 0.015; 0.; 0. |];
    [| 0.; 250.5; 0.; -280.; 33.32 |];
    [| 0.; 0.; 0.; 0.; 12. |];
  |]

  let%test _ = to_array ex1_d = ex1

  let c re im = Complex.{re; im}

  let ex2_d = {
    kind = {
      format = Coordinate;
      field = Complex;
      symmetry = Hermitian;
    };
    rows = 5;
    columns = 5;
    data = [
      1, 1, c 1.0 0.;
      2, 2, c 10.5 0.;
      4, 2, c 250.5 22.22;
      3, 3, c 1.5e-2 0.;
      4, 4, c (-2.8e2) 0.;
      5, 5, c 12. 0.;
      5, 4, c 0. 33.32;
    ];
  }

  let r re = Complex.{re; im = 0.}
  let i im = Complex.{re = 0.; im}

  let ex2 = [|
    [| r 1.; r 0.; r 0.; r 0.; r 0. |];
    [| r 0.; r 10.5; r 0.; c 250.5 (-22.22); r 0. |];
    [| r 0.; r 0.; r 0.015; r 0.; r 0. |];
    [| r 0.; c 250.5 22.22; r 0.; r (-280.); i (-33.32) |];
    [| r 0.; r 0.; r 0.; i 33.32; r 12. |];
  |]

  let%test _ = to_array ex2_d = ex2

  let ex3_d = {
    kind = {
      format = Array;
      field = Real;
      symmetry = General;
    };
    rows = 4;
    columns = 3;
    data = [|
      1.0;
      2.0;
      3.0;
      4.0;
      5.0;
      6.0;
      7.0;
      8.0;
      9.0;
      10.0;
      11.0;
      12.0;
    |];
  }

  let ex3 = [|
    [| 1.0; 5.0; 9.0 |];
    [| 2.0; 6.0; 10.0 |];
    [| 3.0; 7.0; 11.0 |];
    [| 4.0; 8.0; 12.0 |];
  |]

  let%test _ = to_array ex3_d = ex3

  let ext3_d = {
    kind = {
      format = Array;
      field = Integer;
      symmetry = Skew_symmetric;
    };
    rows = 3;
    columns = 3;
    data = [|
      1;
      2;
      3;
    |];
  }

  let ext3 = [|
    [| 0; -1; -2 |];
    [| 1; 0; -3 |];
    [| 2; 3; 0 |];
  |]

  let%test _ = to_array ext3_d = ext3
end)


let parse_coordinate : type a. string -> a field -> int * int * a = fun s -> function
  | Complex -> Scanf.sscanf s "%d %d %f %f" (fun i j re im -> i, j, Complex.{re; im})
  | Integer -> Scanf.sscanf s "%d %d %d" (fun i j x -> i, j, x)
  | Real -> Scanf.sscanf s "%d %d %f" (fun i j x -> i, j, x)
  | Pattern -> Scanf.sscanf s "%d %d" (fun i j -> i, j, true)

(* FIXME compose scanf? *)
let parse_value : type a. string -> a field -> a = fun s -> function
  | Complex -> Scanf.sscanf s "%f %f" (fun re im -> Complex.{re; im})
  | Integer -> Scanf.sscanf s "%d" (fun x -> x)
  | Real -> Scanf.sscanf s "%f" (fun x -> x)
  | Pattern -> failwith "unsupported pattern array"

let expected_array_length : type a. int -> int -> a symmetry -> int = fun r c -> function
  | General -> r * c
  | _ when r <> c -> failwith "symmetric non-square matrix"
  | Skew_symmetric -> r * (r - 1) / 2
  | _ -> r * (r + 1) / 2

type k = K : ('typ, 'data) kind -> k

(*
FIXME I'd like to encode this, maybe polymorphic variants would make it shorter?
[coordinate|array] [real|integer|complex] [general|symmetric|skew-symmetric]
[coordinate|array] complex Hermitian
coordinate pattern [general|symmetric]
*)
let parse_kind l =
  Scanf.sscanf l "%%%%MatrixMarket matrix %s %s %s" (fun f t s ->
    let lo = String.lowercase_ascii in
    match lo f, lo t, lo s with
    | "coordinate", "real", "general" ->
      K {format = Coordinate; field = Real; symmetry = General}
    | "coordinate", "real", "symmetric" ->
      K {format = Coordinate; field = Real; symmetry = Symmetric}
    | "coordinate", "real", "skew-symmetric" ->
      K {format = Coordinate; field = Real; symmetry = Skew_symmetric}
    | "coordinate", "integer", "general" ->
      K {format = Coordinate; field = Integer; symmetry = General}
    | "coordinate", "integer", "symmetric" ->
      K {format = Coordinate; field = Integer; symmetry = Symmetric}
    | "coordinate", "integer", "skew-symmetric" ->
      K {format = Coordinate; field = Integer; symmetry = Skew_symmetric}
    | "coordinate", "complex", "general" ->
      K {format = Coordinate; field = Complex; symmetry = General}
    | "coordinate", "complex", "symmetric" ->
      K {format = Coordinate; field = Complex; symmetry = Symmetric}
    | "coordinate", "complex", "skew-symmetric" ->
      K {format = Coordinate; field = Complex; symmetry = Skew_symmetric}
    | "array", "real", "general" ->
      K {format = Array; field = Real; symmetry = General}
    | "array", "real", "symmetric" ->
      K {format = Array; field = Real; symmetry = Symmetric}
    | "array", "real", "skew-symmetric" ->
      K {format = Array; field = Real; symmetry = Skew_symmetric}
    | "array", "integer", "general" ->
      K {format = Array; field = Integer; symmetry = General}
    | "array", "integer", "symmetric" ->
      K {format = Array; field = Integer; symmetry = Symmetric}
    | "array", "integer", "skew-symmetric" ->
      K {format = Array; field = Integer; symmetry = Skew_symmetric}
    | "array", "complex", "general" ->
      K {format = Array; field = Complex; symmetry = General}
    | "array", "complex", "symmetric" ->
      K {format = Array; field = Complex; symmetry = Symmetric}
    | "array", "complex", "skew-symmetric" ->
      K {format = Array; field = Complex; symmetry = Skew_symmetric}
    | "coordinate", "complex", "hermitian" ->
      K {format = Coordinate; field = Complex; symmetry = Hermitian}
    | "array", "complex", "hermitian" ->
      K {format = Array; field = Complex; symmetry = Hermitian}
    | "coordinate", "pattern", "general" ->
      K {format = Coordinate; field = Pattern; symmetry = General}
    | "coordinate", "pattern", "symmetric" ->
      K {format = Coordinate; field = Pattern; symmetry = Symmetric}
    | _ ->
      failwith @@ Printf.sprintf "unsupported format: %s %s %s" f t s
  )

type w = W : ('typ, 'data) description -> w

let parse ch =
  let K kind = parse_kind (input_line ch) in
  let rec collect_comments comments =
    let l = String.trim (input_line ch) in
    if l = "" then
      collect_comments comments
    else if String.get l 0 = '%' then
      collect_comments (String.(sub l 1 (length l - 1)) :: comments)
    else
      List.rev comments, l
  in
  let comments, l = collect_comments [] in
  match kind.format with
  | Coordinate ->
    let rows, columns, n_entries = Scanf.sscanf l "%d %d %d" (fun x y z -> x, y, z) in
    let rec read data = function
      | 0 -> W {kind; rows; columns; data}, comments
      | n ->
        let l = String.trim (input_line ch) in
        if l = "" then
          read data n
        else
          read (parse_coordinate l kind.field :: data) (n - 1)
    in
    read [] n_entries
  | Array ->
    let rows, columns = Scanf.sscanf l "%d %d" (fun x y -> x, y) in
    let data =
      Array.init (expected_array_length rows columns kind.symmetry) (fun _ ->
        let l = String.trim (input_line ch) in
        parse_value l kind.field
      )
    in
    W {kind; rows; columns; data}, comments


let pp_value : type a. a field -> Format.formatter -> a -> unit = fun f ppf x ->
  match f with
  | Complex -> Format.fprintf ppf "%f\t%f" x.re x.im
  | Integer -> Format.fprintf ppf "%d" x
  | Real -> Format.fprintf ppf "%f" x
  | Pattern -> failwith "no dense output for patterns" (* FIXME type *)

let pp_field : type a. Format.formatter -> a field -> unit = fun ppf f ->
  Format.pp_print_string ppf (
    match f with
    | Complex -> "complex"
    | Integer -> "integer"
    | Pattern -> "pattern"
    | Real -> "real"
  )

let pp_symmetry : type a. Format.formatter -> a symmetry -> unit = fun ppf s ->
  Format.pp_print_string ppf (
    match s with
    | General -> "general"
    | Symmetric -> "symmetric"
    | Hermitian -> "hermitian"
    | Skew_symmetric -> "skew-symmetric"
  )

let output_dense kind get rows columns comments ppf m =
  let _ = expected_array_length rows columns kind.symmetry in (* test! *)
  Format.fprintf ppf "@[<v>%%%%MatrixMarket matrix array %a %a"
    pp_field kind.field
    pp_symmetry kind.symmetry;
  comments |> List.iter (fun c -> Format.fprintf ppf "@;%%%s" c);
  Format.fprintf ppf "@;%d %d" rows columns;
  for j = 1 to columns do
    for i = 1 to rows do
      if i >= first_for_symmetry ~j kind.symmetry then
        let x = get m i j in
        if kind.symmetry = General ||
           get m j i = get_symmetric kind.field x kind.symmetry then
          Format.fprintf ppf "@;%a" (pp_value kind.field) x
        else
          failwith "symmetry violation"
    done
  done;
  Format.fprintf ppf "@]"
