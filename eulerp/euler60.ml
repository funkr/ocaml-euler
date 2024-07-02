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

let is_mirror_prime p1 p2 =
  let a, b =
    Int.of_string (Printf.sprintf "%i%i" p1 p2),
    Int.of_string (Printf.sprintf "%i%i" p2 p1)
  in
  is_prime a && is_prime b

let get_prime n =
  assert (n > 0) ;
  Sequence.hd_exn (Sequence.drop primes (n - 1))

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
  assert (not (List.is_empty lst));
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
  let str = Printf.sprintf "%i" n in
  let len = String.length str in

  let split_at str n : int list option  =
    if n < 0 || n > len then invalid_arg "split_at";
    let a,b = (Int.of_string (String.sub str ~pos:0 ~len:n)),
              (Int.of_string (String.sub str ~pos:n ~len:(len - n))) in
    if is_prime a && is_prime b && (is_mirror_prime a b) then
      if a < b then
        Some( [a; b] )
      else
        Some( [b; a] )
    else
      None
  in

  let res = List.filter_map ~f:(split_at str) (List.range 1 (String.length str)) in
  if List.is_empty res then None else Some res

let merge_prime_tuple result_v pt =

  let comp' res_el pt =
    match List.find res_el ~f:(fun x -> x = List.hd_exn pt),
          List.find res_el ~f:(fun x -> x = List.nth_exn pt 1) with
    | None, None -> Some res_el
    | Some a, None -> Some (try_mirroring res_el a)
    | None, Some b -> Some (try_mirroring res_el b)
    | Some a, Some b -> None
  in

  List.filter_map ~f:(fun x -> comp' x pt)


let print_list lst =
  List.map ~f:(fun x -> Stdio.printf "%s\n" (a_list_to_string Int.to_string x ";")) lst

module IntList = struct
  type t = int list
  let compare = [%compare: int list]
  let hash = Hashtbl.hash
  let sexp_of_t = [%sexp_of: int list]
end

let create_mirror_prime_checker () =
  let table = ref (Hashtbl.create (module IntList)) in
  let is_new_mirror_prime n =
    match Hashtbl.add !table ~key:n ~data:() with
    | `Ok -> true
    | `Duplicate -> false
  in
  let reset_table () =
    table := Hashtbl.create (module IntList)
  in
  (is_new_mirror_prime, reset_table)

let primes' =
  (Sequence.filter_map ~f:slice_number
     (Sequence.filter ~f:is_prime
        (Sequence.unfold ~init:0 ~f:(fun i -> Some (i, i + 1)))))

let is_new_mirror_prime, reset_mirror_prime = create_mirror_prime_checker ()

let rec get_next p =
  match (Sequence.next p) with
  | Some x -> let a,b = x in
    let a' = (List.filter ~f:is_new_mirror_prime a) in
    if List.is_empty a' then get_next b else a',b
  | None -> [], p

let take_n p n =
  (* Collect n candidates *)
  let rec take' acc p n =
    match n with
    | 0 -> acc
    | _ -> let a,b = get_next p in
      take' (List.append acc a) b (n-1)
  in
  take' [] p n

(* Comparison function for int lists *)
let compare_int_lists l1 l2 =
  let rec compare_aux l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x1 :: xs1, x2 :: xs2 ->
        let c = Int.compare x1 x2 in
        if c <> 0 then c else compare_aux xs1 xs2
  in
  compare_aux l1 l2

(********************************************************************************)
let euler60 () =
  let result = main () in
  Printf.sprintf "%i" result

let is_in_list' lst el =
  match (List.find ~f:(fun y -> Poly.( = ) el y) lst) with
  | Some _ -> true
  | None -> false

let is_in_list lst el =
  match (List.find ~f:(fun y -> Poly.( = ) el y) lst) with
  | Some _ -> lst
  | None -> el::lst
(**************************************************)
let%test "Get 100 Candiates" =
  let a = take_n primes' 1000 in
  Stdio.printf "Get10: %i\n" (List.length a);
  let _ = print_list (List.sort ~compare:compare_int_lists a) in
  ( = ) (List.length a) 1028

let%test "Get 3 Candiates" =
  let a = take_n primes' 3 in
  Stdio.printf "Get3: %i" (List.length a);
  ( = ) (List.length a) 3


let%test "Calculate candidates" =
  reset_mirror_prime ();
  let _y,p = get_next primes'  in
  let _y,p = get_next p  in
  let _y,p = get_next p  in
  (* let _ = print_list _y in*)
  Poly.( = ) [[3;17]] _y

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
  (*Stdio.printf "%i" (List.nth_exn (List.nth_exn r 2) 0);*)
  ( = ) 5 (List.length r)

let%test "Push a new prime to the result set and mirror it" =
  let r = mirror [[109]; [109;7; 3]; [11]; [109;7]; [109;3];] 1009 in
  (*Stdio.printf "%i\n" (List.nth_exn (List.nth_exn r 2) 0);*)
  ( = ) 6 (List.length r)

let%test "Print a list of integers" =
  String.equal  "1;2;3;4" (a_list_to_string Int.to_string [1;2;3;4] ";")

let%test "Create mirror prime tuples" =
  let q = (slice_number 673109)  in
  (*let _ = match q with
    | Some x -> Stdio.printf "C:%i\n" (List.length x)
    | None -> Stdio.printf "C:nix\n" in*)
  let a' = List.nth_exn (Option.value q  ~default:[[888;88]]) 0 in
  let a,b = List.nth_exn a' 0, List.nth_exn a' 1  in
  Stdio.printf "Mirror: %i:%i %i\n" a b  (List.length a');
  ( = ) 2 (List.length a')

let%test "already seen" =
  let is_new_mirror_prime, _ = create_mirror_prime_checker () in
  (is_new_mirror_prime [1;2]) && (* false *)
  (is_new_mirror_prime [2]) && (* false *)
  not (is_new_mirror_prime [1;2]) && (* true *)
  (is_new_mirror_prime [3]) && (* false *)
  not (is_new_mirror_prime [2]) (* true *)

let%test "already seen 5" =
  let is_new_mirror_prime, _ = create_mirror_prime_checker () in
  (is_new_mirror_prime [1;2]) && (* false *)
  not (is_new_mirror_prime [1;2]) && (* true *)
  let is_new_mirror_prime, _ = create_mirror_prime_checker () in
  (is_new_mirror_prime [1;2])  (* true *)


let%test "jkjkjk" =
  ( = ) (List.length (is_in_list [[8; 9]; [1; 2]; [3; 4]; [5; 6]] [8; 9])) 4
