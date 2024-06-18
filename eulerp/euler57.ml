(**
   It is possible to show that the square root of two can be expressed as an infinite continued fraction.

   By expanding this for the first four iterations, we get:

   1 + 1/2 = 3/2
   1 + 1 / (2 + 1/2) = 5/7




   The next three expansions are
   ,
   , and
   , but the eighth expansion,

   , is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

   In the first one-thousand expansions, how many fractions contain a numerator with more digits than the denominator?
 *)

(**
   A sequence start 1/2 + 2 ^-1
   divert current value -> check for number of digits in the numerator exceeds the number of digits in the denominator
   next iteration step with step counter
 *)

open Base
open Q

let iterate q = Q.one / (Q.of_int(2) + q)

let one_half = Q.one / Q.of_int(2)

let is_denominator_greater ( q : Q.t ) : bool =
  Int.( > ) (String.length(Z.to_string (Q.num q)))  (String.length (Z.to_string (Q.den q)))

let count_denominator_greater ( v : Q.t ) ( cnt : int ) =
  match is_denominator_greater v with
  | true -> (Int.(+) cnt 1)
  | false -> cnt


let run_filter  =
  let i = List.init 1000 ~f:(fun i -> Int.(+) i 1) in
  let _, j = List.fold i ~init:(one_half,0)
               ~f:(fun (n, cnt) _ ->
                 let n' = iterate n in
                 let cnt' = count_denominator_greater (Q.one + n') cnt in
                 (n', cnt')) in
  j


(* ************************************************** *)
let euler57 () : string =
  let v = run_filter in
  Printf.sprintf "%i" v


(* ************************************************** *)
let%test "Check length of denominator" =
  Bool.( = ) false (is_denominator_greater (Q.of_ints 1 2 ))

let%test "Check length of denominator" =
  Bool.( = ) true (is_denominator_greater (Q.of_ints 1393 985 ))

let%test "Check Bignum fractions" =
  Q.( = ) (Q.of_ints 1 2 ) one_half

