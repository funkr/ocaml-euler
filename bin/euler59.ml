(**
   Each character on a computer is assigned a unique code and the preferred standard
   is ASCII (American Standard Code for Information Interchange). For example,

   uppercase A = 65, asterisk \(\*\) = 42, and lowercase k = 107.
   A modern encryption method is to take a text file, convert the bytes to ASCII,
   then XOR each byte with a given value, taken from a secret key. The advantage with
   the XOR function is that using the same encryption key on the cipher text, restores
   the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

   For unbreakable encryption, the key is the same length as the plain text message,
   and the key is made up of random bytes. The user would keep the encrypted message
   and the encryption key in different locations, and without both "halves", it is
   impossible to decrypt the message.

   Unfortunately, this method is impractical for most users, so the modified method
   is to use a password as a key. If the password is shorter than the message, which
   is likely, the key is repeated cyclically throughout the message. The balance
   for this method is using a sufficiently long password key for security, but short
   enough to be memorable.

   Your task has been made easy, as the encryption key consists of three lower case
   characters. Using 0059_cipher.txt (right click and 'Save Link/Target As...'),
   a file containing the encrypted ASCII codes, and the knowledge that the plain
   text must contain common English words, decrypt the message and find the sum of
   the ASCII values in the original text.
 *)

open Base


let file = "bin/0059_cipher.txt"

let keys_0 = List.init 256 ~f:(fun i -> i)
let keys_1 = List.init 256 ~f:(fun i -> i)
let keys_2 = List.init 256 ~f:(fun i -> i)

let read_cypher_text :string option =
  try
    let ic = Stdio.In_channel.create file in
    let line = Stdio.In_channel.input_line ic in
    Stdio.In_channel.close ic;
    line
  with e ->
    Stdio.print_endline (Printf.sprintf "Can't read file! %s" (Exn.to_string e));
    None

let cypher_line : string =
  match read_cypher_text with
  | None -> ""
  | Some x -> x

let cypher_ints (cypher_line : string) : int list =
  String.split cypher_line ~on:','
  |> List.map ~f:(fun i -> Int.of_string i)


(* Function to partition a list into three lists based on their positions *)
let part_into_three lst =
  let rec aux i (acc1, acc2, acc3) = function
    | [] -> (List.rev acc1, List.rev acc2, List.rev acc3)
    | x :: xs ->
       match i % 3 with
       | 0 -> aux (i + 1) (x :: acc1, acc2, acc3) xs
       | 1 -> aux (i + 1) (acc1, x :: acc2, acc3) xs
       | _ -> aux (i + 1) (acc1, acc2, x :: acc3) xs
  in
  aux 0 ([], [], []) lst


(* Function to convert a list of integers to a string *)
let int_list_to_string lst =
  lst
  |> List.map ~f:(fun c -> c |> Base.Char.of_int_exn |> Printf.sprintf "%c")
  |> String.concat

(* The resulting character must be between 32 and 122*)
let is_in_xor_range (encrypted : int) (key : int) : bool  =
  let r : int = encrypted lxor key in
  32 <= r && r <= 122

(* filter out keys which produce chars outside of legible ascii chars*)
let riddle_keys cypher keys =  
  List.filter keys ~f:(fun k -> is_in_xor_range cypher k)

let haba cypher_list keys =
  let (_c0, _c1, _c2) = part_into_three cypher_list in
  let (_k0, _k1, _k2) = keys in

  List.fold _c0 ~init:_k0 ~f:(fun k cypher -> riddle_keys cypher k),
  List.fold _c1 ~init:_k1 ~f:(fun k cypher -> riddle_keys cypher k),
  List.fold _c2 ~init:_k2 ~f:(fun k cypher -> riddle_keys cypher k)


(* function to decode the text *)
let decode (cypher : int list) (keys : int list) : int list =
  let take_keys = (Fn.flip Sequence.take) (List.length cypher) in
  let encryption_keys = Sequence.cycle_list_exn keys
                  |> take_keys
                  |> Sequence.to_list in
  
  match List.zip cypher encryption_keys with
  | Ok x -> List.map x ~f:(fun (a,b) -> a lxor b)
  | _ -> []

(**********************************************************************)
let euler59 () : string  =
  let cypher = (cypher_line |> cypher_ints) in
  let _j0, _j1, _j2 = haba cypher (keys_0, keys_1, keys_2) in
  let _q0 = decode cypher [101 ; 120; 112] in
  Printf.sprintf "Keys: %i %i %i \n%s\nResult: %i"
    (Array.of_list _j0).(1)
    (Array.of_list _j1).(0)
    (Array.of_list _j2).(0)
    (int_list_to_string _q0)
    (List.fold _q0 ~init:0 ~f:(fun acc i -> acc + i))

