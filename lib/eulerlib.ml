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

(*
                            module PascalTriangle1 = struct
                            (* Represents Pascal's Triangle as a list of rows, each row is an int array *)
                            type t = int array list

                            (* Calculate the value at row `n` and position `k` in Pascal's Triangle *)
                            let get_value (pt : t) n k : int =
                            let pt_length = List.length pt in
                            if n < 0 || k < 0 || n >= pt_length then
                            raise (Invalid_argument "Invalid row or position");

                            pt
                            |> List.nth (pt_length - n - 1)  (* Get the corresponding row *)
                            |> Array.get k                   (* Get the value at position `k` *)

                            (* Generate the next row of Pascal's Triangle based on the previous row *)
                            let generate_next_row (prev_row : int array) : int array =
                            let row_length = Array.length prev_row in
                            let next_row = Array.make (row_length + 1) 1 in
                            for i = 1 to row_length - 1 do
                            next_row.(i) <- prev_row.(i - 1) + prev_row.(i);
                            done;
                            next_row

                            (* Generate Pascal's Triangle up to row `n` *)
                            let generate_triangle (n : int) : t =
                            let rec generate_rows (rows : t) (count : int) : t =
                            if count >= n then rows
                            else
                            let next_row = generate_next_row (List.hd rows) in
                            generate_rows (next_row :: rows) (count + 1)
                            in
                            if n <= 0 then
                            []
                            else
                            [[|1|]] |> generate_rows [] 1

                            (* Calculate the combination C(n, k) *)
                            let combination (n : int) (k : int) : int =
                            if n < k || n > 65 then
                            raise (Invalid_argument "Invalid values for n and k");

                            let triangle = generate_triangle (n + 1) in
                            get_value triangle n k
                            end

                           *)