(**
   Starting with and spiralling anticlockwise in the following way, a square
   spiral with side length 7 is formed.

   37 36 35 34 33 32 31
   38 17 16 15 14 13 30
   39 18  5  4  3 12 29
   40 19  6  1  2 11 28
   41 20  7  8  9 10 27
   42 21 22 23 24 25 26
   43 44 45 46 47 48 49

   It is interesting to note that the odd squares lie along the bottom right
   diagonal, but what is more interesting is that 8 out of 13 the numbers lying
   along both diagonals are prime; that is, a ratio of 8/13 ≃ 62%

   If one complete new layer is wrapped around the spiral above, a square
   spiral with side length 9 will be formed. If this process is continued,
   what is the side length of the square spiral for which the ratio of primes
   along both diagonals first falls below 10%?
*)

(*
   x^2 creates a new layer y
   a = x-1 length of a side
   Diagonal corners are [y; y-a; y-2a; y-3a]
   x' = x+2
*)

open Base

let a_list_to_string to_string lst sep =
  assert (not (List.is_empty lst));
  List.map ~f:to_string lst
  |> String.concat ~sep:sep

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

let primes =
  Sequence.filter ~f:is_prime
    (Sequence.unfold ~init:0 ~f:(fun i -> Some (i, i + 1)))

let naturals =
  Sequence.unfold ~init:3 ~f:(fun i -> Some (i, i + 2))


let calc_corners x =
  let y = x * x in
  let a = x - 1 in
  [y; y-a; y-2*a; y-3*a]

let main =
  Sequence.fold_until naturals
    ~init:(0,1,3)
    ~f:(fun (p_cnt, np_cnt, x) x' ->
        let diag_corners = calc_corners x in
        let prime_numbers, regular = List.partition_tf diag_corners ~f:is_prime in
        let a,b = p_cnt + (List.length prime_numbers) , np_cnt + (List.length diag_corners) in
        if ((compare_float ((Float.of_int a) /. (Float.of_int b))  0.1) = -1)
        then Stop x'
        else Continue ((a,b,(x' + 2)))
      )
    ~finish:(fun (pcnt, npcnt, x) -> x)

let euler58 () =
  Printf.sprintf "%i" main



let%test "diagonal corner for 3" =
  (* let _ = Stdio.printf "diag:%s\n" (a_list_to_string Int.to_string (calc_corners 3) ";") in *)
  Poly.(=) [9;7;5;3] (calc_corners 3)

let%test "diagonal corner for 5" =
  (* let _ = Stdio.printf "diag:%s\n" (a_list_to_string Int.to_string (calc_corners 5) ";") in *)
  Poly.(=) [25;21;17;13] (calc_corners 5)

let%test "diagonal corner for 7" =
  (* let _ = Stdio.printf "diag:%s\n" (a_list_to_string Int.to_string (calc_corners 7) ";") in *)
  Poly.(=) [49;43;37;31] (calc_corners 7)
