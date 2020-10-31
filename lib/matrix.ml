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
      t.data |> List.iter (fun (row, col, x) ->
        let i = row - 1 in
        let j = col - 1 in
        put i j x
      );
      m
    | Array ->
      failwith "TODO"
end

module A = Make (struct
  type 'a t = 'a array array
  let make = Array.make_matrix
  let set t i j x = t.(i).(j) <- x
end)

let to_array = A.build


type w = W : ('typ, 'data) description -> w

(* TODO parse files *)
