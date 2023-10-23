module Card = struct
  type suit = Hearts | Diamonds | Clubs | Spades

  type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

  type t = { rank: rank; suit: suit }

  let rank_order = [
    Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace
  ]

  let compare card1 card2 =

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
    compare card_left card_right = 0

  let ( != ) card_left card_right=
    compare card_left card_right != 0

  let ( > ) card_left card_right=
    compare card_left card_right > 0

  let ( >= ) card_left card_right=
    compare card_left card_right >= 0

  let ( <= ) card_left card_right=
    compare card_left card_right <= 0

  let ( < ) card_left card_right=
    compare card_left card_right < 0


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

  let int_to_rank (i:int) =
    match i with
    | 0 -> Two
    | 1 -> Three
    | 2 -> Four
    | 3 -> Five
    | 4 -> Six
    | 5 -> Seven
    | 6 -> Eight
    | 7 -> Nine
    | 8 -> Ten
    | 9 -> Jack
    | 10 -> Queen
    | 11 -> King
    | 12 -> Ace
    | _ -> Ace

  let rank_to_string (r:rank) =
    match r with
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"

  let to_string (c : t):string=
    match c.suit with
    | Hearts -> Printf.sprintf "%s%s" (rank_to_string c.rank) "H"
    | Diamonds -> Printf.sprintf "%s%s" (rank_to_string c.rank) "D"
    | Clubs -> Printf.sprintf "%s%s" (rank_to_string c.rank) "C"
    | Spades -> Printf.sprintf "%s%s" (rank_to_string c.rank) "S"


  let string_to_card (card_s:string) : t =
    let rank_str = String.sub card_s 0 1 in
    let suit_str = String.sub card_s 1 1 in
    
    let rank = match rank_str with
    | "2" -> Two
    | "3" -> Three
    | "4" -> Four
    | "5" -> Five
    | "6" -> Six
    | "7" -> Seven
    | "8" -> Eight
    | "9" -> Nine
    | "T" -> Ten
    | "J" -> Jack
    | "Q" -> Queen
    | "K" -> King
    | "A" -> Ace
    | _ -> failwith "Wrong character for Ranks 2|3|4|5|6|7|8|9|10|J|Q|K|A" in

    let suit = match suit_str with
    | "H" -> Hearts
    | "D" -> Diamonds
    | "S" -> Spades
    | "C" -> Clubs
    | _ -> failwith "Wrong character for Suites H|S|D|C" in

    { rank; suit }

end
