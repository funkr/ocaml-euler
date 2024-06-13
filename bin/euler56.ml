(**

   A googol (10^10) is a massive number: one followed by one-hundred zeros;
   100^100 is almost unimaginably large: one followed by two-hundred zeros.
   Despite their size, the sum of the digits in each number is only.

   Considering natural numbers of the form,
   a^b, where a,b < 100 what is the maximum digital sum?

 *)

open Base
open Eulerlib

module ExptBI = ExptBigint

(**
   -> choose current max e.g 99⁹⁹
   -> calculate max digit sum
   -> calculate min digit count as threshold to count down eg. 9⁹⁹ does not reach minimun digit count
   
 *)

(* convert the number n to a length of list of digits *)
let digit_count n = List.length (ExptBigint.digits_of n) 

let digit_sum (n : int list) :int=
  List.fold n ~init:0 ~f:(fun acc x -> acc + x)

(* Maximal number which digit count can reach  *)
let max_digit_sum n = n * 9 

let calc (current_max : int) (lower_threshold : int) (a : int) (b : int) =
  let open Bigint in
  let result = (ExptBI.expt (of_int a) (of_int b) ) in
  let dc = digit_count result in
  let digits_of_result = List.map (ExptBI.digits_of result) ~f:(fun x -> to_int_exn x) in 
  let digit_sum_result = (digit_sum digits_of_result) in
  let max_val, threshold = (Int.max digit_sum_result  current_max),
                           (Int.min lower_threshold (max_digit_sum dc)) in
  max_val, threshold


(* runs trough b *)
let run_b current_max lower_threshold a =
  let cm, lt = calc current_max lower_threshold a 99 in
  let rec calc' (current_max : int) (lower_threshold : int) (a : int) (b : int) =
    let cm, lt = calc current_max lower_threshold a b in
    if lt <= cm then
      cm, lt
    else
      calc' cm lt a (b - 1)
  in
  
  calc' cm lt a 98


(* Runs through all a *)
let run_a current_max lower_threshold =
  let a = List.rev (List.init 99 ~f:(fun i -> i + 1)) in
  List.fold a ~init:(current_max, lower_threshold)
    ~f:(fun (current_max, lower_threshold) x -> run_b current_max lower_threshold x)


(* ************************************************** *)
let euler56 () =
  let cm, _ = run_a 0 20000 in
  
  Printf.sprintf "%i" cm
