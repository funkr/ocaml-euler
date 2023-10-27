open OUnit2
open Eulerlib

let is_prime_1 _ = assert_equal false (is_prime 1)
let is_prime_2 _ = assert_equal true (is_prime 2)
let is_prime_3 _ = assert_equal true (is_prime 3)
let is_prime_4 _ = assert_equal false (is_prime 4)
let is_prime_5 _ = assert_equal true (is_prime 5)
let is_prime_6 _ = assert_equal false (is_prime 6)
let is_prime_7 _ = assert_equal true (is_prime 7)
let is_prime_8 _ = assert_equal false (is_prime 8)
let is_prime_9 _ = assert_equal false (is_prime 9)
let is_prime_10 _ = assert_equal false (is_prime 10)
let is_prime_11 _ = assert_equal true (is_prime 11)

let digits_of_int_542333 _ =
  assert_equal [ 3; 3; 3; 2; 4; 5 ] (digits_of_int 542333)

let suite =
  "Example Test Suite"
  >::: [
    "is_prime_1" >:: is_prime_1;
    "is_prime_2" >:: is_prime_2;
    "is_prime_3" >:: is_prime_3;
    "is_prime_4" >:: is_prime_4;
    "is_prime_5" >:: is_prime_5;
    "is_prime_6" >:: is_prime_6;
    "is_prime_7" >:: is_prime_7;
    "is_prime_8" >:: is_prime_8;
    "is_prime_9" >:: is_prime_9;
    "is_prime_10" >:: is_prime_10;
    "is_prime_11" >:: is_prime_11;
    "digits_of_int_542333" >:: digits_of_int_542333;
  ]

let () = run_test_tt_main suite
