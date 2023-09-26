open Printf
open Euler_lib

let has_double_digits n =
  (* true if some digits are double *)
  let a = n |> List.sort_uniq Stdlib.compare |> List.length in
  a < List.length n

let is_prime_with_double_digits x =
  is_prime x && has_double_digits (digits_of_int x)

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
  (* replace a char in a string (I don't know why I have to write this function) *)
  String.map (fun c -> if c = to_replace then replacement else c) old_val

let find_prime_doubles (number_template : string) =
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

let gen_tem = digits_with_positions >> generate_template

let find_max_middle_element lst =
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

let () =
  let start_time = Unix.gettimeofday () in

  let result =
    match
      Seq.ints 1
      |> Seq.filter is_prime_with_double_digits
      |> Seq.map gen_tem
      |> Seq.map (fun x -> List.map (fun y -> find_prime_doubles y) x)
      |> Seq.filter (fun a ->
             match find_max_middle_element a with
             | None -> false
             | Some x -> x = 8)
      |> Seq.take 1 |> List.of_seq |> List.flatten
    with
    | [] -> "no result"
    | (_, _, z) :: _ -> z
  in

  printf "Result: %s \n" result;
  let end_time = Unix.gettimeofday () in
  print_endline
    (sprintf "The operation took %.5f seconds.\n" (end_time -. start_time))
