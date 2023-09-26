open Eulerlib

let has_duplicate_digits n =
  (* true if some digits are duplicate *)
  let a = n |> List.sort_uniq Stdlib.compare |> List.length in
  a < List.length n

let is_prime_with_duplicate_digits x =
  is_prime x && has_duplicate_digits (digits_of_int x)

let digits_with_positions n =
  (* scan the value and return the positions for each digit *)
  let s = string_of_int n in
  let len = String.length s in
  let tbl = Hashtbl.create 10 in
  for i = 0 to len - 1 do
    let digit = s.[i] in
    let digit_as_int = Char.code digit - Char.code '0' in
    match Hashtbl.find_opt tbl digit_as_int with
    | Some positions -> Hashtbl.replace tbl digit_as_int (i :: positions)
    | None -> Hashtbl.add tbl digit_as_int [ i ]
  done;
  (string_of_int n, Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) tbl [])

let char_replace old_val to_replace replacement =
  (* replace a char in a string (I don't know why I have to write such a function) *)
  String.map (fun c -> if c = to_replace then replacement else c) old_val

let generate_prime_family (number_template : string) =
  let is_prime_family number_template (digit : int) : bool =
    let nc = (string_of_int digit).[0] in
    is_prime
      (int_of_string
         (String.map (fun c -> if c = '_' then nc else c) number_template))
  in

  let prime_family : int list =
    List.init 10 (fun i -> i) |> List.filter (is_prime_family number_template)
  in

  let leading_zero_removed =
    if number_template.[0] = '_' then List.filter (fun x -> x <> 0) prime_family
    else prime_family
  in

  ( number_template,
    List.length leading_zero_removed,
    char_replace number_template '_'
      (char_of_int (48 + List.hd leading_zero_removed)) )

(* I wonder why do I have to define this by my self*)
let ( >> ) f g x = g (f x)

let generate_template
    ((number_str : string), (positions : (int * int list) list)) =
  List.map
    (fun (digit, _) -> char_replace number_str (char_of_int (48 + digit)) '_')
    positions

let generate_multi_digit_templates = digits_with_positions >> generate_template

let find_max_family lst =
  let compare_max (tr, max_so_far, ss) (t, current, s) =
    if current < max_so_far then (tr, max_so_far, ss) else (t, current, s)
  in
  match lst with
  | [] -> None
  | head :: tail ->
      let _, max_val, _ =
        List.fold_left (fun acc x -> compare_max acc x) head tail
      in
      Some max_val

let euler51 =
  match
    Seq.ints 1
    |> Seq.filter is_prime_with_duplicate_digits
    |> Seq.map generate_multi_digit_templates
    |> Seq.map (fun x -> List.map (fun y -> generate_prime_family y) x)
    |> Seq.filter (fun a ->
           match find_max_family a with None -> false | Some x -> x = 8)
    |> Seq.take 1 |> List.of_seq |> List.flatten
  with
  | [] -> "no result"
  | (_, _, z) :: _ -> z
