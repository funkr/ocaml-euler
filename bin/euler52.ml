(** Permuted Multiples
    Problem 52

    It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits,
    but in a different order.

    Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

*)

(* I need function are-digits-equal; a multiplication factor list (duplicate digits allowed?) *)

(* Make a list out of a string *)
let get_digit_list number =
  List.sort Char.compare (string_of_int number |> String.to_seq |> List.of_seq)

(* Make a comparable list out of the number *)
let generate_digit_lists number =
  number
  |> (fun x -> List.map (fun y -> x * y) [ 2; 3; 4; 5; 6 ])
  |> List.map get_digit_list

(* Compare all five digits lists *)
let are_digits_equal (digit_list : char list list) =
  match digit_list with
  | [] | [ _ ] -> true
  | head :: tail -> List.for_all (fun lst -> lst = head) tail

let has_same_digits_for_13456 number =
  number |> generate_digit_lists |> are_digits_equal

let euler52 =
  match
    Seq.ints 1
    |> Seq.filter has_same_digits_for_13456
    |> Seq.take 1 |> List.of_seq
  with
  | [] -> failwith "No solution found!"
  | head :: _ -> string_of_int head
