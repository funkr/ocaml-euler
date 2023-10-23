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

(* read file line by line *)
(* convert to cards *)
(* split cards into player 1 and 2 *)
(* compare two hands *)

open Card
type scores = High_Card | One_Pair | Two_Pairs | Three_of_a_Kind | Straight | Flush | Full_House | Four_of_a_Kind | Straight_Flush | Royal_Flush

let print_score s =
  match s with
  | High_Card -> Printf.printf "High Card"
  | One_Pair -> Printf.printf "One Pair"
  | Two_Pairs -> Printf.printf "Two Pairs"
  | Three_of_a_Kind -> Printf.printf "Three of a Kind"
  | Straight -> Printf.printf "Straight"
  | Flush -> Printf.printf "Flush"
  | Full_House -> Printf.printf "Full House"
  | Four_of_a_Kind -> Printf.printf "Four of a Kind"
  | Straight_Flush -> Printf.printf "Straight Flush"
  | Royal_Flush -> Printf.printf "Royal Flush"


let compare_score (l:scores) (r:scores) : int =
  if l < r then -1 else if l = r then 0 else 1

let find_minimum arr =
  Array.fold_left (fun min_element current_element ->
      if current_element < min_element then current_element else min_element
    ) arr.(0) arr

let find_maximum arr =
  Array.fold_left (fun min_element current_element ->
      if current_element > min_element then current_element else min_element
    ) arr.(0) arr

(** n cards of the same suit. *)
let is_n_of_a_suit (n:int) (cards : Card.t array) : bool =
  let _suit_counts = Array.make 4 0 in
  Array.iter (fun (c:Card.t) -> _suit_counts.(Card.suit_to_int c.suit) <- (_suit_counts.(Card.suit_to_int c.suit) + 1) ) cards;
  Array.exists (fun c -> c=n) _suit_counts

(** All cards are of the same suit *)
let is_flush (cards: Card.t array) =
  is_n_of_a_suit 5 cards


(** All cards are consecutive values *)
let is_straight (cards : Card.t array) : bool * Card.rank =
  let start = find_minimum cards in

  let _straight =
    Card.rank_order |> List.to_seq
    |> Seq.drop_while (fun c -> c <> start.rank)
    |> Seq.take 5 |> List.of_seq in

  (cards |>
   Array.for_all
     (fun (c:Card.t) ->
        List.exists
          (fun (r:Card.rank) -> c.rank = r)
          _straight) && (List.length _straight) = 5)  , cards.(4).rank


(** Rank analysis*)
let analyse_ranks (cards : Card.t array) : int array =
  cards
  |> Array.fold_left
    (fun counts (card :Card.t) ->
       let rank_int = Card.rank_to_int card.rank in
       counts.(rank_int) <- counts.(rank_int) + 1;
       counts)
    (Array.make 13 0)

let invert_compare a b  = (compare a b) * -1


let print_array arr =
  Array.iter (fun c -> Printf.printf "%i " c) arr

let print_rank lst =
  List.iter (fun c -> Printf.printf "%s " (Card.rank_to_string c)) lst

let print_hand (cards: Card.t array) =
  Array.iter (fun c -> Printf.printf "%s " (Card.to_string c)) cards


let straights is_straight straight_rank is_flush a =
  match is_straight, is_flush with
  | true, true -> 
    let ranks =  straight_rank in
    Straight_Flush, [ranks]

  | true, false -> 
    let ranks =  straight_rank in
    Straight, [ranks]

  | false, true -> 
    let ranks =  straight_rank in
    Flush, [ranks]

  | false, false ->  
    let ranks = List.mapi (fun i e -> if e = 1 then Some (Card.int_to_rank i) else None)  (Array.to_list a) |> List.rev in
    High_Card, List.filter_map (fun e -> e) ranks          


let get_score _consolidated_n_tuples _is_straight _straight_rank _is_flush a =
  match _consolidated_n_tuples with
  | [1;1;1;1;1] -> straights _is_straight _straight_rank _is_flush a

  | [4;1] -> 
    let _ranks = List.mapi (fun i e -> if e = 4 then Some (Card.int_to_rank i) else None)  (Array.to_list a) in
    let _ranks = List.filter_map (fun e -> e) _ranks in
    Four_of_a_Kind, _ranks

  | [3;2] -> 
    let _3ranks = List.mapi (fun i e -> if e = 3 then Some (Card.int_to_rank i) else None)  (Array.to_list a) in
    let _2ranks = List.mapi (fun i e -> if e = 2 then Some (Card.int_to_rank i) else None)  (Array.to_list a) in
    Full_House, List.filter_map (fun e -> e) (List.append _3ranks  _2ranks)

  | [3;1;1] -> 
    let _ranks =  List.mapi (fun i e -> if e = 3 then Some (Card.int_to_rank i) else None)  (Array.to_list a) in
    let _1ranks = List.mapi (fun i e -> if e = 1 then Some (Card.int_to_rank i) else None)  (Array.to_list a) |> List.rev  in
    Three_of_a_Kind, List.filter_map (fun e -> e) (List.append _ranks  _1ranks)

  | [2;2;1] -> 
    let _ranks = List.mapi (fun i e -> if e = 2 then Some (Card.int_to_rank i) else None)  (Array.to_list a) |> List.rev in
    let _1ranks = List.mapi (fun i e -> if e = 1 then Some (Card.int_to_rank i) else None)  (Array.to_list a) |> List.rev in
    Two_Pairs, List.filter_map (fun e -> e) (List.append _ranks  _1ranks)

  | [2;1;1;1] -> 
    let _ranks = List.mapi (fun i e -> if e = 2 then Some (Card.int_to_rank i) else None)  (Array.to_list a) in
    let _1ranks = List.mapi (fun i e -> if e = 1 then Some (Card.int_to_rank i) else None)  (Array.to_list a) |> List.rev  in
    One_Pair, List.filter_map (fun e -> e) (List.append _ranks  _1ranks)

  | _ -> failwith "Count of cards seems wrong!"


let score (pl1_score, rank1) (pl2_score, rank2) : int =

  if pl1_score < pl2_score then 2 else
  if pl1_score > pl2_score then 1 else
  if rank1 < rank2  then 2
  else if rank1 > rank2 then 1
  else failwith "Really"

let play (cards1: Card.t array) (cards2: Card.t array)  :int =

  let calc_score (cards: Card.t array ) =
    cards |> Array.sort Card.compare;
    let _is_flush = is_flush cards in
    let _is_straight, _straight_rank = is_straight cards in
    let _rank_distribution = analyse_ranks cards in
    let _consolidated_n_tuples = Array.to_list _rank_distribution
                                 |> List.filter (fun c -> c > 0)
                                 |> List.sort invert_compare in

    get_score _consolidated_n_tuples _is_straight _straight_rank _is_flush _rank_distribution
  in

  let _player_1_score, _player_1_rank = calc_score cards1 in
  let _player_2_score, _player_2_rank = calc_score cards2 in
  print_hand cards1; Printf.printf " -> "; print_score _player_1_score; Printf.printf " : "; print_rank _player_1_rank;
  print_newline ();
  print_hand cards2; Printf.printf " -> "; print_score _player_2_score; Printf.printf " : "; print_rank _player_2_rank;
  print_newline ();
  score (_player_1_score, _player_1_rank) (_player_2_score, _player_2_rank)                   


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
  let cards =  String.split_on_char ' ' line |> List.map Card.string_to_card |> List.to_seq in
  let player_1 = Array.of_seq (Seq.take 5 cards) in
  let player_2 = Array.of_seq (Seq.drop 5 cards) in

  (* *************** *)

  let w = (play player_1 player_2) in
  Printf.printf "player %i won\n" w;
  print_newline ();
  w

let euler54 =

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


  let _flush = [| {Card.rank = Two   ; suit= Hearts};
                  {Card.rank = Four  ; suit= Hearts};
                  {Card.rank = Queen  ; suit= Hearts};
                  {Card.rank = Six  ; suit= Hearts};
                  {Card.rank = Jack  ; suit= Hearts} |] in
  (* **************************************************** *)


  let lines = read_file_line_by_line "bin/0054_poker.txt" in

  let _a, _b = List.fold_left (fun (a,b) l  ->
      let winner = process_line l in
      if winner = 1 then (a+1, b) else (a,b+1))
      (0,0) lines in

  let __b, _ = is_straight _fk in
  Printf.sprintf "a:%i b:%i" _a _b

