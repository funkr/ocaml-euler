(**
There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, (5/3) = 10.

In general, (n/r) = n!/(r!(n−r)!),
where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.

It is not until n= 23, that a value exceeds one-million: (23/10) = 1144066.

How many, not necessarily distinct, values of (n/r)
for 1 ≤ n ≤ 100, are greater than one-million?
*)

open Eulerlib

(* This a simplistic solution for Euler 53 *)
(* Pascal triangle would be an idea*)
(* Dont calculate the value, just check if its greater than 1 million*)
(* Use the symmetry (23/10) = (23/13) -> (n - r1) - r2 + 1 for one n value  *)
let euler53 =
  let count = ref 0 in
  for n = 1 to 100 do
    for r = 1 to n do
      if combination (Bignum.of_int n) (Bignum.of_int r) > Bignum.of_int 1000000
      then incr count
    done
  done;
  Printf.sprintf "%d" !count