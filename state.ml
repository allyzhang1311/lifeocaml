(* TYPES/EXCEPTIONS *)

type state = {
  players : Player.player list;
  careers : Game.career list;
  salaries : Game.salary list;
  houses : Game.house list;
  tiles : Game.tile list;
  stocks : Game.stock list;
  owned_stocks : (int * string) list
}

exception Bad_state
exception Player_not_found
exception Target_insufficient_bal

(* INIT/ACCESSOR/SHUFFLE FUNCTIONS *)

(** [compare_tagged_cards x y] is 1 if x's second component is
    greater than y's, 0 if the two components are equal,
    and -1 otherwise.

    Requires: x and y's second component are comparable with
    [Pervasives.compare]. *)
let compare_tagged_cards x y =
  if snd x > snd y then 1 else if snd x < snd y then -1 else 0

(** [shuffle lst] is a list containing the same elements as [lst] but in
    pseudo-random order. *)
let shuffle lst =
  Random.self_init ();
  let lst_tagged = List.map (fun card -> (card, Random.bits ())) lst in 
  let lst_sorted = List.sort compare_tagged_cards lst_tagged in 
  List.map fst lst_sorted

(** [stringlst_to_players lst] is the list of players with names
    specified by [lst]. The order of players will be the reverse of the order
    of names. *)
let rec stringlst_to_players lst = 
  let rec helper lst acc =
    match lst with
    | [] -> acc 
    | h::t -> helper t ((Player.init_player h)::acc)
  in helper lst []

let init_state (pl : string list) =
  {
    players = shuffle(stringlst_to_players pl);
    careers = shuffle Game.careers;
    salaries = shuffle Game.salaries;
    houses = shuffle Game.houses;
    tiles = shuffle Game.tiles;
    stocks = shuffle Game.stocks;
    owned_stocks = []
  }

let get_player_list st = st.players

let get_nonret_player_list st =
  List.filter (fun p -> not (Player.get_retired p)) st.players

let get_player_names st = List.map (fun p -> Player.get_name p) st.players

let get_player st pname =
  let rec helper plst pname =
    match plst with
    | [] -> raise Player_not_found
    | h::t -> if String.lowercase_ascii (Player.get_name h) = pname then h
      else helper t pname
  in helper st.players (String.lowercase_ascii pname)

(** [get_tile st] is [Some t] if [st] has at least one untaken
    tile, [None] otherwise. *)
let get_tile st =
  match st.tiles with 
  | [] -> st, None
  | h::t -> {st with tiles = t}, Some h

(* MUTATOR FUNCTIONS *)

(** [update_players st p] is [st] with the player whose name matches [p]'s
    name (case sensitive) changed to a copy of [p].
    Raises: [Player_not_found] if there is no such player. *)
let update_players st p =
  let rec helper plst p acc =
    match plst with
    | [] -> raise Player_not_found
    | h::t -> if Player.get_name h = Player.get_name p then 
        {st with players = (List.rev acc) @ (p::t)}
      else helper t p (h::acc)
  in helper st.players p []

let select_career_salary st p college =
  let career_choices = if college then st.careers else
      List.filter (fun (card : Game.career) -> not card.degree) st.careers in 
  let salary_choices = if college then st.salaries else
      List.filter (fun (card : Game.salary) -> not card.degree) st.salaries in  
  let rand_career1 = List.hd career_choices in
  let rand_career2 = List.hd (List.tl career_choices) in
  let rand_salary = List.hd salary_choices in 
  let p2 = Player.set_salary p rand_salary in 
  let st2 = update_players st p2 in 
  {st2 with
   salaries =
     List.filter (fun (card : Game.salary) -> card.amt <> rand_salary.amt)
       st.salaries;
  }, p2, rand_career1, rand_career2

let set_career st p (c : Game.career) =
  let p2 = Player.set_career p c in
  let st2 = update_players st p2 in
  {st2 with
   careers = 
     shuffle(List.filter
               (fun (card : Game.career) -> card.name <> c.name)
               st.careers)}, p2

let set_car_insure st p =
  if Player.get_car_insure_bool p then raise Bad_state
  else
    let p2 = Player.set_car_insure p in 
    let st2 = update_players st p2 in 
    st2, p2

let set_home_insure st p =
  if Player.get_home_insure_bool p || Player.get_house p = None then 
    raise Bad_state
  else
    let p2 = Player.set_home_insure p in 
    let st2 = update_players st p2 in 
    st2, p2 

let set_no_college st p =
  let p2 = Player.set_no_college p in 
  update_players st p2, p2

let set_retired st p =
  let p2 = Player.set_retired p in 
  update_players st p2, p2

let add_tile st p =
  let (st2, tile) = get_tile st in
  match tile with 
  | Some(t) ->
    let p2 = Player.add_tile p t in 
    update_players st2 p2, p2
  | None -> st2, p

let add_fam st p fam =
  let p2 = Player.add_fam p fam in
  add_tile (update_players st p2) p2

let make_loan st p amt_loan =
  if (amt_loan < 20000) then raise Bad_state else
    let p2 = (Player.set_loans p
                ((Player.get_loans p) + amt_loan/20000)) in 
    update_players st p2, p2

let repay_loan st p amt =
  if (amt < 25000) then raise Bad_state else
    let p2 = (Player.set_loans p
                ((Player.get_loans p) - amt/25000)) in 
    update_players st p2, p2

let move_player st p num =
  if Player.get_retired p then raise Bad_state
  else
    let p2 = Player.move p num in 
    update_players st p2, p2, (Player.get_square p2)



let switch_salary st pa pb =
  if Player.get_retired pb || Player.get_retired pa ||
     pa = pb then raise Bad_state
  else if pa = pb then raise Bad_state
  else
    let sa_opt = Player.get_salary pa in 
    let sb_opt = Player.get_salary pb in
    match sa_opt, sb_opt with 
    | None, _ | _, None -> raise Bad_state
    | Some sa, Some sb ->
      let pa2 = Player.set_salary pa sb in 
      let pb2 = Player.set_salary pb sa in 
      update_players (update_players st pa2) pb2, pa2

let switch_career st p =
  let old_career_opt = Player.get_career p in 
  let old_salary_opt = Player.get_salary p in 
  let college = Player.get_college p in
  match old_career_opt, old_salary_opt with 
  | None, _ | _ , None -> raise Bad_state
  | Some old_career, Some old_salary ->
    let career_choices = if college then st.careers else
        List.filter (fun (card : Game.career) -> not card.degree) st.careers in 
    let salary_choices = if college then st.salaries else
        List.filter (fun (card : Game.salary) -> not card.degree) st.salaries in  
    let rand_career = List.hd career_choices in
    let rand_salary = List.hd salary_choices in 
    let p2 = Player.set_career (Player.set_salary p rand_salary) rand_career in
    let st2 = update_players st p2 in
    {
      st2 with 
      careers = shuffle (old_career :: st2.careers);
      salaries = shuffle (old_salary :: st2.salaries);
    }, p2

let update_balance st src tgt_string amt add =
  if tgt_string = String.lowercase_ascii (Player.get_name src)
  || amt < 0 then raise Bad_state
  else 
  if tgt_string = "bank" then
    let src2 = Player.update_balance src amt add in 
    update_players st src2, src2
  else
    let tgt_player = get_player st tgt_string in 
    if Player.get_retired tgt_player then raise Bad_state else 
      let src2 = Player.update_balance src amt add in 
      let tgt2 = Player.update_balance tgt_player amt (not add) in       
      update_players (update_players st src2) tgt2, src2

let set_house st p = 
  let rand_house = List.hd st.houses in 
  let p2 = Player.set_house p rand_house in 
  let (st3, p3) = update_balance st p2 "bank" rand_house.price false in
  {st3 with
   houses = List.tl st.houses;}, p3

let stock_owner st stock = 
  let rec helper stockls stock =
    match (stockls) with
    | (int, pname) :: t -> if int = stock then Some pname
      else helper t stock
    | [] -> None
  in helper st.owned_stocks stock

let stock_owned st stock =
  match (stock_owner st stock) with
  |Some pname -> true
  |None -> false

let remove_stock st stock = 
  let rec helper stockls acc stock =
    match stockls with
    |(st,pl)::t -> if (st = stock) then (helper t acc stock) else
        (helper t ((st,pl)::acc) stock)
    |[] -> acc
  in {st with owned_stocks = (helper st.owned_stocks [] stock)}

let rec update_stock st p str = 
  match str with
  |"boom" -> begin
      let stock = List.hd st.stocks in
      let p2 = (Player.set_stocks p (stock::(Player.get_stocks p))) in
      let st2 = update_players st p2 in 
      {st2 with
       stocks = List.tl st2.stocks;
       owned_stocks = (stock, Player.get_name p) :: st2.owned_stocks}, p2
    end
  |"bust" -> begin
      let stock_lost = List.hd (Player.get_stocks p) in
      let stocks_remaining = List.tl (Player.get_stocks p) in
      let p2 = Player.set_stocks p stocks_remaining in
      let st2 = update_players st p2 in 
      {st2 with
       stocks = stock_lost :: st2.stocks;
       owned_stocks = List.filter
           (fun (x,y) -> x <> stock_lost) st.owned_stocks}, p2
    end
  |_ -> try
      (
        let stock = int_of_string str in       
        let p2 =
          Player.set_stocks p ((stock)::Player.get_stocks p) in
        let st2 = update_players st p2 in 
        {st2 with
         stocks = List.filter (fun x -> x <> stock) st2.stocks;
         owned_stocks = (stock, Player.get_name p) :: st.owned_stocks}, p2
      )
    with Failure(_) -> raise Bad_state

let sell_house st p =
  match Player.get_house p with 
  | None -> failwith "Bad board"
  | Some old_house ->
    let (st2, p2) = set_house st p in 
    let (st3, p3) = update_balance st2 p2 "bank" (old_house.price) true in
    {st3 with
     houses = shuffle(old_house :: st2.houses)}, p3

let save_of_state st players_left =
  let save_of_players players = begin
    let rec helper players acc = (
      match players with
      | [] -> acc
      | h::t -> helper t ((Player.save_of_player h) ^ acc))
    in
    helper players ""
  end in
  let save_of_careers st = begin
    let rec helper (careers : Game.career list) acc =
      match careers with
      | [] -> acc
      | h::t ->
        helper t (
          ("{\n\"name\": \"" ^ h.name ^ "\",\n"
           ^ "\"effect\": \"" ^ h.effect ^ "\",\n"
           ^ "\"degree\": \"" ^ (string_of_bool h.degree) ^ "\"\n},") ^ acc)
    in
    helper st.careers ""
  end in
  let save_of_salaries st = begin
    let rec helper (salaries : Game.salary list) acc =
      match salaries with
      | [] -> acc
      | h::t ->
        helper t (
          ("{\n\"amt\": \"" ^ (string_of_int h.amt) ^ "\",\n"
           ^ "\"taxes\": \"" ^ (string_of_int h.taxes) ^ "\",\n"
           ^ "\"degree\": \"" ^ (string_of_bool h.degree) ^ "\"\n},") ^ acc)
    in
    helper st.salaries ""
  end in
  let save_of_houses st = begin
    let rec helper (houses : Game.house list) acc =
      match houses with
      | [] -> acc
      | h::t -> helper t (
          ("{\n\"name\": \"" ^ h.name ^ "\",\n"
           ^ "\"desc\": \"" ^ h.desc ^ "\",\n"
           ^ "\"price\": \"" ^ (string_of_int h.price) ^ "\",\n"
           ^ "\"insurance\": \""
           ^ (string_of_int h.insurance) ^ "\"\n},") ^ acc)
    in helper st.houses ""
  end in
  let save_of_tiles st = begin
    let rec helper (tiles : Game.tile list) acc =
      match tiles with
      | [] -> acc
      | h::t -> helper t (
          ("{\n\"desc\": \"" ^ h.desc ^ "\",\n"
           ^ "\"amt\": \""
           ^ (string_of_int h.amt) ^ "\"\n},") ^ acc)
    in helper st.tiles ""
  end in
  let save_of_stocks st = begin    
    let rec helper (stocks : Game.stock list) acc =
      match stocks with
      | [] -> acc
      | h::t -> helper t (string_of_int h) ^ "," ^ acc
    in helper st.stocks ""
  end in
  let save_of_owned_stocks st = begin    
    let rec helper owned_stocks acc =
      match owned_stocks with
      | [] -> acc
      | (x,y)::t -> helper t (
          ("{\n\"stock\": \"" ^ (string_of_int x) ^ "\",\n"
           ^ "\"owner\": \"" ^ y ^ "\"\n},") ^ acc)
    in helper st.owned_stocks ""
  end in
  (
    "{\n"
    ^ "\"players left\": [" ^ (save_of_players players_left) ^ "],\n"
    ^ "\"players\": [\n" ^ (save_of_players st.players) ^ "],\n"
    ^ "\"careers\": [\n" ^ (save_of_careers st) ^ "],\n"
    ^ "\"salaries\": [\n" ^ (save_of_salaries st) ^ "],\n"
    ^ "\"houses\": [\n" ^ (save_of_houses st) ^ "],\n"
    ^ "\"tiles\": [\n" ^ (save_of_tiles st) ^ "],\n" 
    ^ "\"stocks\": [\n" ^ (save_of_stocks st) ^ "]\n}"
    ^ "\"owned_stocks\": [\n" ^ (save_of_owned_stocks st) ^ "]\n}"
  )