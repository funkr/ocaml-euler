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

module Card = struct
type suit = Hearts | Diamonds | Clubs | Spades

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

type t = { rank: rank; suit: suit }

let rank_order = [
    Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace
  ] 
  
let compare_cards card1 card2 =
  
  let suit_order = [
    Hearts; Diamonds; Clubs; Spades
  ] in
  let compare_rank card1 card2 =
    let rank1 = List.find_opt (fun c -> card1.rank=c) rank_order in
    let rank2 = List.find_opt (fun c -> card2.rank=c) rank_order in
    compare rank1 rank2
  in
  let compare_suit card1 card2 =
    let suit1 = List.find_opt (fun c -> card1.suit=c) suit_order in
    let suit2 = List.find_opt (fun c -> card2.suit=c) suit_order in
    compare suit1 suit2
  in
  match compare_rank card1 card2 with
  | 0 -> compare_suit card1 card2
  | result -> result


  let ( = ) card_left card_right=
    compare_cards card_left card_right = 0

  let ( != ) card_left card_right=
    compare_cards card_left card_right != 0

  let ( > ) card_left card_right=
    compare_cards card_left card_right > 0

  let ( >= ) card_left card_right=
    compare_cards card_left card_right >= 0

  let ( <= ) card_left card_right=
    compare_cards card_left card_right <= 0

  let ( < ) card_left card_right=
    compare_cards card_left card_right < 0


  let suit_to_int (s:suit) =
    match s with
    | Hearts -> 0
    | Diamonds -> 1
    | Clubs -> 2
    | Spades -> 3

  let rank_to_int (r:rank) =
    match r with
    | Two -> 0
    | Three -> 1
    | Four -> 2
    | Five -> 3
    | Six -> 4
    | Seven -> 5
    | Eight -> 6
    | Nine -> 7
    | Ten -> 8
    | Jack -> 9
    | Queen -> 10
    | King -> 11
    | Ace -> 12
end

let find_minimum arr =
  Array.fold_left (fun min_element current_element ->
    if current_element < min_element then current_element else min_element
  ) arr.(0) arr


let is_royal_flush  (cards : Card.t array) : bool =
  (* Ten, Jack, Queen, King, Ace, in same suit. *)
  let _rf_suit = (Array.get cards 0).suit in
  cards
  |> Array.for_all
             (fun (c:Card.t) ->
               List.exists
                 (fun (r:Card.rank) -> c.rank = r && c.suit = _rf_suit)
                 [ Ten; Jack; Queen; King; Ace ]) 


(** All cards are consecutive values of same suit. *)
let is_straight_flush (cards : Card.t array) : bool =
  let _sf_suit = (Array.get cards 0).suit in
  let start = find_minimum cards in
  
  if start.rank >= Ten then
    false
  else
    let _flush =
      Card.rank_order |> List.to_seq
      |> Seq.drop_while (fun c -> c <> start.rank)
      |> Seq.take 5 |> List.of_seq in
    
    cards
    |> Array.for_all
         (fun (c:Card.t) ->
               List.exists
                 (fun (r:Card.rank) -> c.rank = r && c.suit = _sf_suit)
                 _flush) 

(** Four cards of the same value. *)
let is_four_of_a_suit (cards : Card.t array) : bool =
  let _rank_counts = Array.make 13 0 in
  Array.iter (fun (c:Card.t) -> _rank_counts.(Card.rank_to_int c.rank) <- (_rank_counts.(Card.rank_to_int c.rank) + 1) ) cards;
  Array.exists (fun c -> c=4) _rank_counts
  
  
let euler54 = 

  let rf = [| {Card.rank = Ten   ; suit= Clubs};
              {Card.rank = Queen ; suit= Hearts};
              {Card.rank = Jack  ; suit= Hearts};
              {Card.rank = King  ; suit= Hearts};
              {Card.rank = Nine  ; suit= Hearts} |] in

  
  Array.sort Card.compare_cards rf;
  let b = is_straight_flush rf in
  Printf.sprintf "%b" b
