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
  let rec loop (i : Bignum.t) (acc : Bignum.t) =
    if i > n then acc
    else loop (Bignum.( + ) i (Bignum.of_int 1)) (Bignum.( * ) acc i)
  in
  loop (Bignum.of_int 1) (Bignum.of_int 1)

let binomial n k =
  let rec aux n k acc =
    if k = 0 then acc else aux (n - 1) (k - 1) (acc * n / k)
  in
  aux n k 1

(* n over r = n!/(r!(n−r)!) *)
let combination (n : Bignum.t) (r : Bignum.t) =
  Bignum.( / ) (factorial n)
    (Bignum.( * ) (factorial r) (factorial (Bignum.( - ) n r)))