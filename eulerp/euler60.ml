(** The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and
    concatenating them in any order the result will always be prime. For example,
    taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792,
    represents the lowest sum for a set of four primes with this property.

    Find the lowest sum for a set of five primes for which any two primes concatenate
    to produce another prime. *)

(* The idea is to iterate through prime numbers.
   Every prime number (except 5) is starting element of 5-tuple which holds the
   property that every combination of two elements is a prime number:
   (3,7) -> 37, 73

   Has a terrible runtime*)

(* Find in primes mirroring primes and store them as mirror candidates*)

open Base

let is_prime (n : int) : bool =
  if n = 2 || n = 3 then true
  else if n < 2 || 0 = n % 2 then false
  else if n < 9 then true
  else if 0 = n % 3 then false
  else
    let f = 5 in
    let r = Int.of_float (Float.round_up (Float.sqrt (Float.of_int n))) in
    let rec step (p : int) (r : int) (f : int) : bool =
      if f <= r then
        if 0 = p % f then false
        else if 0 = p % (f + 2) then false
        else step p r (f + 6)
      else true
    in
    step n r f

let digits_of_int n =
  let rec aux n acc =
    if n < 10 then n :: acc else aux (n / 10) ((n % 10) :: acc)
  in
  aux n []

let primes =
  Sequence.filter ~f:is_prime
    (Sequence.unfold ~init:0 ~f:(fun i -> Some (i, i + 1)))

let get_prime n =
  assert (n > 0) ;
  Sequence.hd_exn (Sequence.drop primes (n - 1))

let is_mirror_prime p1 p2 =
  let a, b =
    Int.of_string (Printf.sprintf "%i%i" p1 p2),
    Int.of_string (Printf.sprintf "%i%i" p2 p1)
  in
  is_prime a && is_prime b

let try_mirroring mirror_primes p =
  if List.for_all ~f:(fun x -> is_mirror_prime x p) mirror_primes then
    let _a = Stdio.printf "New mirror: %i" in
    p :: mirror_primes
  else mirror_primes

type stage =
  | Searching
  | Solution
  | MinimalSolution

let has_solutions r =
  List.filter ~f:(fun x -> List.length x >= 5) r

let mirror (mirrored_primes: int list list) (p:int) : (int list list) =
  List.map ~f:(fun x -> try_mirroring x p) ([p] :: mirrored_primes )


(** The assumed minimal sum for one possible 5-tuple*)
let calc_stop_limit partial_5_tuple current_prime =
  (List.fold ~init:0 ~f:( + ) partial_5_tuple)
  +  (5 - (List.length partial_5_tuple)) * current_prime

let calc_stop_limit' current_prime partial_5_tuple =
  (List.fold ~init:0 ~f:( + ) partial_5_tuple)
  +  (5 - (List.length partial_5_tuple)) * current_prime

let minimum_stop_sum partial_5_tuples curr_prime =
  let sums = List.map ~f:(calc_stop_limit' curr_prime) partial_5_tuples in
  match List.reduce sums ~f:(fun acc x -> min acc x) with
  | Some x -> x
  | None -> -1

let max_sol_tuple tpl =
  assert(List.length tpl > 0);
  match List.map  ~f:(fun x -> List.length x) tpl
        |> List.reduce ~f:(fun acc x -> max acc x) with
  | Some x -> x
  | None -> -1


(* predefined result set, mainly to get rid of 5
   because it cannot build mirror primes x5 *)
let r_sets = [[7;3]; [7]; [3]]

let state = Searching

let a_list_to_string to_string lst sep =
  List.map ~f:to_string lst
  |> String.concat ~sep:sep

(** The search terminates when there is at least one 5-tuple
    and all other incomplete tuple have a higher stop sum as
    the one 5-tuple *)
let main (): int  =
  let rec search r primes' =
    let p = (Sequence.hd_exn primes') in

    let r' = mirror r p in
    let s = has_solutions r' in
    Stdio.printf "%i=%i\n" p 7;
    if p > (Int.max_value - 100000) then 88
    else
    if List.length s > 0 && (minimum_stop_sum s p) = (minimum_stop_sum r' p) then
      minimum_stop_sum s p
    else
      search r' (Sequence.drop primes' 1)

  in
  search r_sets (Sequence.drop primes 4)

let slice_number n =
  let split_at str n =
    let len = String.length str in
    if n < 0 || n > len then invalid_arg "split_at";
    (Int.of_string (String.sub str ~pos:0 ~len:n)),
    (Int.of_string (String.sub str ~pos:n ~len:(len - n)))
  in

  let str = (Printf.sprintf "%i" n) in
  let split_at' = split_at str in
  let splits = List.map ~f:split_at' (List.range 1 (String.length str)) in
  splits

(********************************************************************************)
let euler60 () =
  let result = main () in
  Printf.sprintf "%i" result

(**************************************************)
let%test "Check test is working" =
  0 = 0

let%test "Get the 5.th primes" =
  11 = get_prime 5

let%test "Split a int to a list of digits" =
  Poly.( = ) [6; 7; 3] (digits_of_int 673)

let%test "Is mirror prime" =
  Bool.( = ) true (is_mirror_prime 109 673)

let%test "Try mirroring with [3] 7" =
  Poly.( = ) [7; 3] (try_mirroring [3] 7)

let%test "Try mirroring with 3;7 109" =
  Poly.( = ) [109; 7; 3] (try_mirroring [7; 3] 109)

let%test "Check if resolution has a 5 tuple" =
  ( = ) 1 (List.length (has_solutions [[677;673;109;7;3];[7;3]; [7]; [3]]))

let%test "Calc a fictive limit" =
  ( = ) 403 (calc_stop_limit [3] 100)

let%test "Calc the limit for a solution (5-tuple)" =
  ( = ) 8669 (calc_stop_limit [3; 7; 109; 673; 7877] 100000)

let%test "Push a new prime to the result set and mirror it" =
  let r = mirror [[7; 3]; [11]; [7]; [3];] 109 in
  Stdio.printf "%i" (List.nth_exn (List.nth_exn r 2) 0);
  ( = ) 5 (List.length r)

let%test "Push a new prime to the result set and mirror it" =
  let r = mirror [[109]; [109;7; 3]; [11]; [109;7]; [109;3];] 1009 in
  Stdio.printf "%i" (List.nth_exn (List.nth_exn r 2) 0);
  ( = ) 6 (List.length r)

let%test "Print a list of integers" =
  String.equal  "1;2;3;4" (a_list_to_string Int.to_string [1;2;3;4] ";")

let%test "Create mirror tuples" =
  let a,b = List.nth_exn (slice_number 123456) 4 in
  Stdio.printf "Mirror: %i:%i\n" a b ;
  ( = ) 5 (List.length (slice_number 123456))
