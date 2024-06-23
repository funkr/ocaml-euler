(** The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating
    them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097
    are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes
    with this property.

    Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime. *)

(* The idea is to iterate through prime numbers.
   Every prime number (except 5) is starting element of 5-tuple which holds the property that
   every combination of two elements is a prime number. (3,7) -> 37, 73*)

open Base

let primes =
  Sequence.of_list [2; 3; 5; 7; 9; 11; 13; 17]

let getp  = Sequence.hd_exn (Base.Sequence.drop primes 5)

let primes'' = (Sequence.drop (Sequence.init 1 ~f:(fun x -> Int.(+) x 1)) 699) 

let primes' = Sequence.length primes''

let primes''' = Sequence.hd_exn primes''


let euler60 () =
  Stdio.printf "No Result!";
  "NA"

let%test "Check test is working" =
  ( = ) 0 0


let%test "Get the 5.th primes" = 
  ( = ) 11 getp

let%test "Get the 5.th element of sequence" =
  Stdio.printf "%i" primes''';
  ( = ) 1 primes'
