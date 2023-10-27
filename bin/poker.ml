open Card
module Poker = struct
  type scores = High_Card | One_Pair | Two_Pairs | Three_of_a_Kind | Straight | Flush | Full_House | Four_of_a_Kind | Straight_Flush | Royal_Flush

  let print_score s =
    match s with
    | High_Card -> Printf.sprintf "High Card"
    | One_Pair -> Printf.sprintf "One Pair"
    | Two_Pairs -> Printf.sprintf "Two Pairs"
    | Three_of_a_Kind -> Printf.sprintf "Three of a Kind"
    | Straight -> Printf.sprintf "Straight"
    | Flush -> Printf.sprintf "Flush"
    | Full_House -> Printf.sprintf "Full House"
    | Four_of_a_Kind -> Printf.sprintf "Four of a Kind"
    | Straight_Flush -> Printf.sprintf "Straight Flush"
    | Royal_Flush -> Printf.sprintf "Royal Flush"

  (** n cards of the same suit. *)
  let is_n_of_a_suit (n : int) (cards : Card.t array) : bool =
    let _suit_counts = Array.make 4 0 in
    Array.iter (fun (c:Card.t)
                 -> _suit_counts.(Card.suit_to_int c.suit) <- (_suit_counts.(Card.suit_to_int c.suit) + 1) ) cards;
    Array.exists (fun c -> c = n) _suit_counts

  (** All cards are of the same suit *)
  let is_flush (cards: Card.t array) =
    is_n_of_a_suit 5 cards

  let find_minimum arr =
    Array.fold_left (fun min_element current_element ->
        if current_element < min_element then current_element else min_element
      ) arr.(0) arr

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

  let print_array arr =
    Array.iter (fun c -> Printf.printf "%i " c) arr

  let print_rank lst =
    List.iter (fun c -> Printf.printf "%s " (Card.rank_to_string c)) lst

  let print_hand (cards: Card.t array) =
    Array.iter (fun c -> Printf.printf "%s " (Card.to_string c)) cards


  let get_ranks_by_cnt rank_distribution rank_cnt =
    rank_distribution
    |> Array.to_list 
    |> List.mapi (fun i e -> if e = rank_cnt then Some (Card.int_to_rank i) else None)
    |> List.rev
    |> List.filter_map Fun.id


  let straights is_straight straight_rank is_flush rank_distribution =
    match is_straight, is_flush with
    | true, true -> 
      Straight_Flush, [straight_rank]

    | true, false -> 
      Straight, [straight_rank]

    | false, true -> 
      Flush, [straight_rank]

    | false, false ->  
      High_Card, get_ranks_by_cnt rank_distribution 1


  let get_score _consolidated_n_tuples _is_straight _straight_rank _is_flush a =
    match _consolidated_n_tuples with
    | [1;1;1;1;1] -> straights _is_straight _straight_rank _is_flush a

    | [4;1] -> 
      Four_of_a_Kind, get_ranks_by_cnt a 4

    | [3;2] -> 
      Full_House, List.append (get_ranks_by_cnt a 3) (get_ranks_by_cnt a 2) 

    | [3;1;1] -> 
      Three_of_a_Kind, List.append (get_ranks_by_cnt a 3) (get_ranks_by_cnt a 1)

    | [2;2;1] ->     
      Two_Pairs, List.append (get_ranks_by_cnt a 2) (get_ranks_by_cnt a 1)

    | [2;1;1;1] -> 
      One_Pair, List.append (get_ranks_by_cnt a 2) (get_ranks_by_cnt a 1) 

    | _ -> failwith "Count of cards seems wrong!"


  let score (pl1_score, rank1) (pl2_score, rank2) : int =

    if pl1_score < pl2_score then 2 else
    if pl1_score > pl2_score then 1 else
    if rank1 < rank2  then 2
    else if rank1 > rank2 then 1
    else failwith "Really"

end
