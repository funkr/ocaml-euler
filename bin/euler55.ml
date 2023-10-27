(**
If we take 47, 47 + 74 = 121 reverse and add,

, which is palindromic.

Not all numbers produce palindromes so quickly. For example,

349 + 943 = 1292,
1292 + 2921 = 4213
4213 + 3124 = 7337

That is 349, took three iterations to arrive at a palindrome.

Although no one has proved it yet, it is thought that some numbers, like
, never produce a palindrome. A number that never forms a palindrome through
the reverse and add process is called a Lychrel number. Due to the theoretical
nature of these numbers, and for the purpose of this problem, we shall assume
that a number is Lychrel until proven otherwise. In addition you are given that
for every number below ten-thousand, it will either (i) become a palindrome in
less than fifty iterations, or, (ii) no one, with all the computing power that
exists, has managed so far to map it to a palindrome. In fact, 10667 is the first
number to be shown to require over fifty iterations before producing a
palindrome: 4668731596684224866951378664 ( 53 iterations, 28 -digits).

Surprisingly, there are palindromic numbers that are themselves Lychrel numbers;
the first example is 4994.

How many Lychrel numbers are there below ten-thousand?

NOTE: Wording was modified slightly on 24 April 2007 to emphasise the
      theoretical nature of Lychrel numbers.

*)

let reverse_number n =
  let rec extract_digits acc n =
    if n = 0 then
      acc
    else
      let digit = n mod 10 in
      extract_digits (digit :: acc) (n / 10)
  in

  let rec compose_number acc = function
    | [] -> acc
    | digit :: rest ->
      let new_acc = acc * 10 + digit in
      compose_number new_acc rest
  in

  let digits = extract_digits [] n in

  compose_number 0 (List.rev digits)

let is_palindrome n =
  n = reverse_number n

let is_lychrel n =
  let rec is_lychrel_aux n iter =
    if iter >= 50 then
      true
    else
      let reversed = reverse_number n in
      let sum = n + reversed in
      if is_palindrome sum then
        false
      else
        is_lychrel_aux sum (iter + 1)
  in
  is_lychrel_aux n 0

let euler55 =
    let lychrel_cnt =
    Seq.ints 1
    |> Seq.take 10000
    |> Seq.filter is_lychrel
    |> Seq.length in
    Printf.sprintf "%d" lychrel_cnt