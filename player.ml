
type player = {
  name : string;
  career : Game.career option;
  house : Game.house option;
  salary : Game.salary option;
  tiles : Game.tile list;
  family : Game.family_mem list;
  position : int;
  balance : int;
  retired : bool;
  loans : int;
  home_insure : bool;
  car_insure : bool;
  college : bool;
  stocks: int list;
}

let init_player n =
  {
    name = n;
    career = None;
    house = None;
    salary = None;
    tiles = [];
    family = [];
    position = 0;
    balance = 10000;
    retired = false;
    loans = 0;
    home_insure = false;
    car_insure = false;
    college = true;
    stocks = [];
  }

let get_name p = p.name
let get_career p = p.career
let get_salary p = p.salary

let get_career_name p =
  match p.career with
  |Some(career) -> Game.get_career_name career
  |None -> "Null"

let get_house_name p =
  match p.house with
  |Some(house) -> Game.get_house_name house
  |None -> "Null"

let get_salary_amt p =
  match p.salary with 
  | Some x -> x.amt
  | None -> 0

let print_salary p = 
  match p.salary with 
  | Some x -> Game.save_of_salary x
  | None -> "Null"

let get_house p = p.house
let get_insurance p = 
  match p.house with 
  | Some h -> h.insurance 
  | None -> 0
let get_retired p = p.retired
let get_position p = p.position
let get_square p =
  if p.college && p.position <= 10 then 
    Game.college_branch.(p.position)
  else Game.full_board.(p.position)
let get_tiles p = p.tiles

let print_tiles p =
  let rec helper tls acc =
    match tls with
    | [] -> acc
    | h::[] -> acc ^ Game.save_of_tile h
    | h::t -> (helper t (acc ^ (Game.save_of_tile h) ^ ","))
  in helper p.tiles ""

let print_family p =
  let rec helper f acc =
    match f with
    | [] -> acc
    | h::[] -> acc ^ Game.save_of_family h 
    | h::t -> (helper t (acc ^ (Game.save_of_family h) ^ ","))
  in helper p.family ""

let print_stocks p =
  begin    
    let rec helper stckls acc =
      match stckls with
      | [] -> acc
      | h::[] ->
        helper stckls (acc ^
                       ("{\n\"stock\": \"" ^ (string_of_int h) ^ "\"\n}"))
      | h::t ->
        helper stckls (acc ^
                       ("{\n\"stock\": \"" ^ (string_of_int h) ^ "\"\n},"))  
    in helper p.stocks ""
  end

let get_balance p = p.balance
let get_house p = p.house
let get_loans p = p.loans
let get_home_insure_bool p = p.home_insure
let get_car_insure_bool p = p.car_insure
let get_college p = p.college
let get_stocks p = p.stocks

let update_balance p amt (add : bool) =
  {p with balance = if add then (p.balance + amt) else (p.balance - amt)}
let set_salary p s = {p with salary = Some s}
let set_career p c = {p with career = Some c}
let set_house p h = {p with house = Some h}
let set_retired p = {p with retired = true}
let set_loans p l = {p with loans = l}
let set_car_insure p =
  {
    p with
    car_insure = true;
    balance = p.balance - 10000;
  }
let set_home_insure p =
  {
    p with
    home_insure = true;
    balance = p.balance - (get_insurance p);
  }
let set_no_college p = {p with college = false}
let set_stocks p intls = {p with stocks = intls}

let add_tile p tile = {p with tiles = tile::p.tiles}
let add_fam p fam = {p with family = fam::p.family}

(** [traverse start num college] attempts to move [num] spaces
    starting from position [start] on the board.
    If [college] is true, spaces from [college_branch] are
    substituted on the board in the appropriate places.
    Returns: [(num_actual, acc_payday)]
    where [num_actual] is the number of spaces passed,
    and [acc_payday] is the number of [Payday] spaces passed
    (note that this does not include position [start]).
    Requires: [num] is in the range \[1,10\],
    [start] is in the range \[0,[Game.board_length]\].
    Raises: [Invalid_argument("index out of bounds")] if
    either of the above requirements are not met. *)
let traverse start num college : int * int = 
  (* let rec helper b num_left acc_payday =
     match b with
     | [] -> failwith "Bad board"
     | h::t as b2 -> if num_left = 0 then (
        if h = Game.Payday then (b2, num, acc_payday+sal)
        else (b2, num, acc_payday)
      )
      else
        match h with 
        | Game.Stop(x) ->
          if num = num_left then helper t (num_left-1) acc_payday
          else (b2, num - num_left, acc_payday)
        | Game.Payday ->
          if num = num_left then helper t (num_left-1) acc_payday
          else helper t (num_left-1) (acc_payday+sal)
        | _ ->
          helper t (num_left-1) acc_payday
     in helper b num 0 *)
  let rec helper curr_num acc_payday = 
    if num = curr_num then (curr_num, acc_payday) else
      let board =
        if college && (start + curr_num) < (Array.length Game.college_branch)
        then Game.college_branch else Game.full_board in
      let curr_sq = board.(start + curr_num) in 
      match curr_sq with 
      | Game.Stop(x) -> (curr_num, acc_payday)
      | Game.Payday -> helper (curr_num + 1) (acc_payday + 1)
      | _ -> helper (curr_num + 1) acc_payday
  in helper 1 0

let move p num =
  let (num_actual, acc_payday) =
    traverse p.position num p.college in 
  {
    p with
    balance = p.balance + (acc_payday * (get_salary_amt p));
    position = p.position + num_actual;
  }

let format fmt p =
  begin
    Format.fprintf fmt "Name: %s\n"
      p.name;
    Format.fprintf fmt "Bank balance: $%d (-$%d loan)\n"
      p.balance (p.loans * 25000);
    Format.fprintf fmt "Insurance: home-%s, car-%s\n"
      (if p.home_insure then "yes" else "no")
      (if p.car_insure then "yes" else "no");
    Format.fprintf fmt "College degree: %s\n"
      (if p.college then "yes" else "no");   
    Format.fprintf fmt "Position on board: %d/%d\n"
      p.position Game.board_length;
    Format.fprintf fmt "Number of LIFE tiles: %d\n"
      (List.length p.tiles);
    Format.fprintf fmt "%a\n" Game.format_family_lst p.family;
    Format.fprintf fmt "--------------------------\n";
    Format.fprintf fmt "%a\n" Game.format_career p.career;
    Format.fprintf fmt "%a\n" Game.format_house p.house;
    Format.fprintf fmt "%a\n" Game.format_salary p.salary;
    Format.fprintf fmt "%a\n" Game.format_stocks p.stocks;
  end

let save_of_player p =
  "{\n\"name\": \"" ^ p.name^ "\",\n"
  ^ "\"career\": \"" ^ (get_career_name p) ^ "\",\n"
  ^ "\"house\": \"" ^ (get_house_name p) ^ "\",\n"
  ^ "\"salary\": \"" ^ (print_salary p) ^ "\",\n"
  ^ "\"tiles\": [" ^ (print_tiles p) ^ "],\n"
  ^ "\"family\": [" ^ (print_family p) ^ "],\n"
  ^ "\"position\": \"" ^ string_of_int p.position ^ "\",\n"
  ^ "\"balance\": \"" ^ string_of_int p.balance ^ "\",\n"
  ^ "\"retired\": \"" ^ string_of_bool p.retired ^ "\",\n"
  ^ "\"loans\": \"" ^ string_of_int p.loans ^ "\",\n"
  ^ "\"home_insure\": \"" ^ string_of_bool p.home_insure ^ "\",\n"
  ^ "\"car_insure\": \"" ^ string_of_bool p.car_insure ^ "\",\n"
  ^ "\"college\": \"" ^ string_of_bool p.college ^ "\",\n"
  ^ "\"stocks\": [" ^ (print_stocks p) ^ "]\n},"