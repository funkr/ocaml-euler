open Euler51
open Printf

let () =
  let start_time = Unix.gettimeofday () in

  let result = euler51 in

  let end_time = Unix.gettimeofday () in
  printf "Result: %s \n" result;
  print_endline
    (sprintf "The operation took %.5f seconds." (end_time -. start_time))
