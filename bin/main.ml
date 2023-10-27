open Euler51
open Euler52
open Euler53
open Euler54
open Euler55
open Printf

let () =
  let argc = Array.length Sys.argv in
  let euler_nr = if argc > 1 then int_of_string Sys.argv.(1) else 1 in
  let start_time = Unix.gettimeofday () in

  let result =
    match euler_nr with
    | 51 -> euler51
    | 52 -> euler52
    | 53 -> euler53
    | 54 -> euler54
    | 55 -> euler55
    | _ -> failwith "not (yet) programmed!"
  in

  Unix.sleep 1;
  let end_time = Unix.gettimeofday () in
  printf "Result: %s \n" result;
  print_endline
    (sprintf "The operation took %.5f seconds." (end_time -. start_time -. 1.))
