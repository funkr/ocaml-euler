(**
   euler 54

   In the card game poker, a hand consists of five cards and are ranked, from lowest to highest,
   in the following way:

    High Card: Highest value card.
    One Pair: Two cards of the same value.
    Two Pairs: Two different pairs.
    Three of a Kind: Three cards of the same value.
    Straight: All cards are consecutive values.
    Flush: All cards of the same suit.
    Full House: Three of a kind and a pair.
    Four of a Kind: Four cards of the same value.
    Straight Flush: All cards are consecutive values of same suit.
    Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

   The cards are valued in the order:
   2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

   If two players have the same ranked hands then the rank made up of the highest value wins;
   for example, a pair of eights beats a pair of fives (see example 1 below).
   But if two ranks tie, for example, both players have a pair of queens,
   then highest cards in each hand are compared (see example 4 below);
   if the highest cards tie then the next highest cards are compared, and so on.

   Consider the following five hands dealt to two players:
   Hand	 	Player 1	 	Player 2	 	Winner
   1	 	5H 5C 6S 7S KD          2C 3S 8S 8D TD          Player 2
                Pair of Fives           Pair of Eights

   2	 	5D 8C 9S JS AC          2C 5C 7D 8S QH	 	Player 1
                Highest card Ace        Highest card Queen

   3	 	2D 9C AS AH AC	 	3D 6D 7D TD QD          Player 2
                Three Aces              Flush with Diamonds

   4	 	4D 6S 9H QH QC          3D 6D 7H QD QS 	 	Player 1
                Pair of Queens          Pair of Queens
                Highest card Nine       Highest card Seven

   5	 	2H 2D 4C 4D 4S          3C 3D 3S 9S 9D 	 	Player 1
                Full House              Full House
                With Three Fours        with Three Threes


                The file, poker.txt, contains one-thousand random hands dealt to two players.
                Each line of the file contains ten cards (separated by a single space):
                the first five are Player 1's cards and the last five are Player 2's cards.
                You can assume that all hands are valid (no invalid characters or repeated cards),
                each player's hand is in no specific order, and in each hand there is a clear winner.

   How many hands does Player 1 win?

*)



open Card
open Poker
let play (cards1: Card.t array) (cards2: Card.t array)  :int =

  let calc_score (cards: Card.t array ) =
    cards |> Array.sort Card.compare;
    let is_flush' = Poker.is_flush cards in
    let is_straight', straight_rank = Poker.is_straight cards in
    let rank_distribution = Poker.analyse_ranks cards in
    let consolidated_n_tuples = rank_distribution
                                |> Array.to_list 
                                |> List.filter (fun c -> c > 0)
                                |> List.sort Frolib.invert_compare in

    Poker.get_score consolidated_n_tuples is_straight' straight_rank is_flush' rank_distribution
  in

  let _player_1_score, _player_1_rank = calc_score cards1 in
  let _player_2_score, _player_2_rank = calc_score cards2 in
  Poker.print_hand cards1; Printf.printf " -> "; Printf.printf "%s" (Poker.print_score _player_1_score); Printf.printf " : "; Poker.print_rank _player_1_rank;
  print_newline ();
  Poker.print_hand cards2; Printf.printf " -> "; Printf.printf "%s" (Poker.print_score _player_2_score); Printf.printf " : "; Poker.print_rank _player_2_rank;
  print_newline ();
  Poker.score (_player_1_score, _player_1_rank) (_player_2_score, _player_2_rank)                   


let read_file_line_by_line filename =
  let channel = open_in filename in
  try
    let rec read_lines acc =
      try
        let line = input_line channel in
        read_lines (line :: acc)
      with End_of_file ->
        List.rev acc
    in
    let lines = read_lines [] in
    close_in channel;
    lines
  with e ->
    close_in_noerr channel;
    raise e


let process_line (line : string) =
  let cards =  line
               |> String.split_on_char ' '
               |> List.map Card.string_to_card
               |> List.to_seq in
  let player_1 = Array.of_seq (Seq.take 5 cards) in
  let player_2 = Array.of_seq (Seq.drop 5 cards) in

  let winner = (play player_1 player_2) in
  Printf.printf "player %i won\n" winner;
  print_newline ();
  winner

let euler54 =

  (* read file line by line *)
  (* convert to cards *)
  (* split cards into player 1 and 2 *)
  (* compare two hands *)

  let _rf = [| {Card.rank = Ten   ; suit= Hearts};
               {Card.rank = Queen ; suit= Hearts};
               {Card.rank = Jack  ; suit= Hearts};
               {Card.rank = King  ; suit= Hearts};
               {Card.rank = Ace  ; suit= Hearts} |] in

  let _straight_flush = [| {Card.rank = Eight   ; suit= Hearts};
                           {Card.rank = Ten ; suit= Hearts};
                           {Card.rank = Six  ; suit= Hearts};
                           {Card.rank = Nine  ; suit= Hearts};
                           {Card.rank = Seven  ; suit= Hearts} |] in

  let _fk = [| {Card.rank = Ten   ; suit= Clubs};
               {Card.rank = Ten   ; suit= Hearts};
               {Card.rank = Jack  ; suit= Hearts};
               {Card.rank = Ten  ; suit= Spades};
               {Card.rank = Ten  ; suit= Diamonds} |] in

  let _4k = [| {Card.rank = Jack   ; suit= Clubs};
               {Card.rank = Jack  ; suit= Hearts};
               {Card.rank = Queen  ; suit= Hearts};
               {Card.rank = Jack  ; suit= Spades};
               {Card.rank = Jack  ; suit= Diamonds} |] in

  let _3k = [| {Card.rank = Jack   ; suit= Clubs};
               {Card.rank = Jack  ; suit= Hearts};
               {Card.rank = Queen  ; suit= Hearts};
               {Card.rank = King  ; suit= Spades};
               {Card.rank = Jack  ; suit= Diamonds} |] in

  let _fh = [| {Card.rank = Jack   ; suit= Clubs};
               {Card.rank = Jack  ; suit= Hearts};
               {Card.rank = King  ; suit= Hearts};
               {Card.rank = King  ; suit= Spades};
               {Card.rank = Jack  ; suit= Diamonds} |] in

  let _fhl = [| {Card.rank = Jack   ; suit= Clubs};
                {Card.rank = Jack  ; suit= Hearts};
                {Card.rank = Two  ; suit= Hearts};
                {Card.rank = Two  ; suit= Spades};
                {Card.rank = Jack  ; suit= Diamonds} |] in

  let _2k = [| {Card.rank = Jack   ; suit= Clubs};
               {Card.rank = Jack  ; suit= Hearts};
               {Card.rank = Queen  ; suit= Hearts};
               {Card.rank = King  ; suit= Spades};
               {Card.rank = Ace  ; suit= Diamonds} |] in

  let _2p = [| {Card.rank = Jack   ; suit= Clubs};
               {Card.rank = Jack  ; suit= Hearts};
               {Card.rank = Queen  ; suit= Hearts};
               {Card.rank = Queen  ; suit= Spades};
               {Card.rank = Ace; suit= Diamonds} |] in

  let _hc = [| {Card.rank = Two   ; suit= Clubs};
               {Card.rank = Four  ; suit= Hearts};
               {Card.rank = King  ; suit= Hearts};
               {Card.rank = Six  ; suit= Spades};
               {Card.rank = Jack  ; suit= Diamonds} |] in

  let _hc1= [| {Card.rank = Three   ; suit= Clubs};
               {Card.rank = Four  ; suit= Hearts};
               {Card.rank = Queen  ; suit= Hearts};
               {Card.rank = Six  ; suit= Spades};
               {Card.rank = Jack  ; suit= Diamonds} |] in


  
  (* **************************************************** *)


  let lines = read_file_line_by_line "bin/0054_poker.txt" in

  let a, b = List.fold_left (fun (a,b) l  ->
      let winner = process_line l in
      if winner = 1 then (a+1, b) else (a,b+1))
      (0,0) lines in

  Printf.sprintf "Player 1 won:%i Player 2 won:%i" a b

