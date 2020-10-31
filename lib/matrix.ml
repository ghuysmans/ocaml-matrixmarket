type ('typ, 'data) format =
  | Coordinate : ('typ, (int * int * 'typ) list) format
  | Array : ('typ, 'typ array) format

type 'a typ =
  | Complex : Complex.t typ
  | Integer : int typ
  | Pattern : bool typ
  | Real : float typ

type 'typ structure =
  | General : 'typ structure
  | Symmetric : 'typ structure
  | Hermitian : Complex.t structure
  | Skew_symmetric : 'typ structure

type ('typ, 'data) description = {
  format: ('typ, 'data) format;
  typ: 'typ typ;
  structure: 'typ structure;
  rows: int;
  columns: int;
  data: 'data;
}

let zero : type a. a typ -> a = function
  | Complex -> Complex.zero
  | Integer -> 0
  | Pattern -> false
  | Real -> 0.

let neg : type a. a typ -> a -> a = function
  | Complex -> Complex.neg
  | Integer -> (~-)
  | Pattern -> (not)
  | Real -> (~-.)

let value_of_string : type a. a typ -> string -> a = function
  | Complex -> failwith "TODO"
  | Integer -> int_of_string
  | Real -> float_of_string
  | Pattern -> fun s ->
    if s = "0" then
      false
    else if s = "1" then
      true
    else
      failwith "invalid bool representation"

module type S = sig
  type 'a t
  val make : int -> int -> 'a -> 'a t
  val set : 'a t -> int -> int -> 'a -> unit
end

module Make (M : S) = struct
  let build : type typ data. (typ, data) description -> typ M.t = fun t ->
    let m = M.make t.rows t.columns (zero t.typ) in
    let put i j x =
      M.set m i j x;
      match t.structure with
      | General -> ()
      | Hermitian -> M.set m j i (Complex.conj x)
      | Symmetric -> M.set m j i x
      | Skew_symmetric -> M.set m j i (neg t.typ x)
    in
    match t.format with
    | Coordinate ->
      t.data |> List.iter (fun (i, j, x) -> put i j x);
      m
    | Array ->
      match t.structure with
      | General ->
        t.data |> Array.iteri (fun index x ->
          M.set m (index mod t.rows + 1) (index / t.rows + 1) x
        );
        m
      | _ ->
        let pos = ref 0 in
        for j = 1 to t.columns do
          for i = j to t.rows do
            M.set m i j t.data.(!pos);
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
    format = Coordinate;
    typ = Real;
    structure = General;
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
    format = Coordinate;
    typ = Complex;
    structure = Hermitian;
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
    format = Array;
    typ = Real;
    structure = General;
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
end)


type w = W : ('typ, 'data) description -> w

(* TODO parse files *)
