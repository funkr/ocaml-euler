(* all possible orderings, no repeated elements *)
let rec permute lst =
  let insert_all_positions x xs =
    let rec aux pre suf acc =
      match suf with
      | [] -> List.rev (x :: pre) :: acc
      | hd :: tl ->
          aux (hd :: pre) tl (List.rev_append pre (x :: hd :: tl) :: acc)
    in
    aux [] xs []
  in
  match lst with
  | [] -> [ [] ]
  | x :: xs -> List.flatten (List.map (insert_all_positions x) (permute xs))

(* in sorted order, no repeated elements *)
let rec choose k list : int list list =
  if k = 0 then [ [] ]
  else
    match list with
    | [] -> []
    | x :: xs ->
        let with_x = List.map (fun ys -> x :: ys) (choose (k - 1) xs) in
        let without_x = choose k xs in
        with_x @ without_x

(* Quite costly operation *)
let rec digits_of_int n =
  if n < 10 then [ n ] else (n mod 10) :: digits_of_int (n / 10)

let is_prime (n : int) : bool =
  if n = 2 || n = 3 then true
  else if n < 2 || 0 = n mod 2 then false
  else if n < 9 then true
  else if 0 = n mod 3 then false
  else
    let f = 5 in
    let r = int_of_float (ceil (sqrt (float_of_int n))) in
    let rec step (p : int) (r : int) (f : int) : bool =
      if f <= r then
        if 0 = p mod f then false
        else if 0 = p mod (f + 2) then false
        else step p r (f + 6)
      else true
    in
    step n r f

(* faculty function from https://rosettacode.org/wiki/Factorial#OCaml *)
let factorial (n : Bignum.t) : Bignum.t =
  let open Bignum in
  let rec loop (i : t) (acc : t) =
    if i > n then acc else loop (i + of_int 1) (acc * i)
  in
  loop (of_int 1) (of_int 1)

let binomial n k =
  let rec aux n k acc =
    if k = 0 then acc else aux (n - 1) (k - 1) (acc * n / k)
  in
  aux n k 1

(* Calculates combination C(n, r) using Bignum.t for large integers -> n!/(r!(n−r)!) *)
let combination (n : Bignum.t) (r : Bignum.t) : Bignum.t =
  let open Bignum in
  factorial n / (factorial r * factorial (n - r))

module PascalTriangle = struct
  let pascal_triangle = ref [ [| 1 |] ]

  let _get_row_val row i =
    if i = 0 || Array.length row = i then 1 else row.(i - 1) + row.(i)

  let rec _grow_pascal_triangle pt n =
    let pt_length = List.length pt in
    if pt_length >= n then pt
    else
      let prev_row = List.hd pt in
      let new_row =
        Array.init (pt_length + 1) (fun i -> _get_row_val prev_row i)
      in
      _grow_pascal_triangle (new_row :: pt) n

  let _get_binom_val pt n k : int =
    let ptl = List.length pt - 1 in
    Array.get (List.nth pt (ptl - n)) k

  let combination n k : int =
    if n < k then raise (Invalid_argument "n<k not allowed");
    pascal_triangle := _grow_pascal_triangle !pascal_triangle (n + 1);
    _get_binom_val !pascal_triangle n k
end

module type T = sig
  type t

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
end

module type BINOMIAL = sig
  type t

  val cNr : int -> int -> t
end

module PT (M : BINOMIAL) = struct
  type t = M.t

  let cNr = M.cNr
end

module PascalTriangleGeneric (M : T) = struct
  type t = M.t

  let zero = M.zero
  let one = M.one
  let ( + ) = M.( + )
  let pascal_triangle = ref [ [| one |] ]

  let _get_row_val row (i : int) =
    if i = 0 || Array.length row = i then one
    else Array.get row (Int.sub i 1) + Array.get row i

  let rec grow_pascal_triangle pt n =
    let pt_length = List.length pt in
    if pt_length >= n then pt
    else
      let prev_row = List.hd pt in
      let new_row =
        Array.init (Int.add pt_length 1) (fun i -> _get_row_val prev_row i)
      in
      grow_pascal_triangle (new_row :: pt) n

  let get_binom_val pt n k : t =
    let ptl = List.length pt - 1 in
    Array.get (List.nth pt (ptl - n)) k

  let cNr n k : t =
    if n < k then raise (Invalid_argument "n<k not allowed");
    pascal_triangle := grow_pascal_triangle !pascal_triangle (Int.add n 1);
    get_binom_val !pascal_triangle n k
end

(* Type Int *)
module IntPascTri : T with type t = int = struct
  type t = int

  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
end

module BinomialInt = PT (PascalTriangleGeneric (IntPascTri))

(* Type Float *)
module FloatPascTri : T with type t = float = struct
  type t = float

  let zero = 0.0
  let one = 1.0
  let ( + ) = Stdlib.( +. )
end

module BinomialFloat = PT (PascalTriangleGeneric (FloatPascTri))

(* Type Bignum *)
module BignumPascTri : T with type t = Bignum.t = struct
  type t = Bignum.t

  let zero = Bignum.zero
  let one = Bignum.one
  let ( + ) = Bignum.( + )
end

module BinomialBignum = PT (PascalTriangleGeneric (BignumPascTri))