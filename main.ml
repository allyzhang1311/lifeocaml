open Format

(* SPIN + GENERAL HELPER FUNCTIONS *)

(** [spin ()] returns a pseudorandom number in \[1,10\]. *)
let spin () = Random.self_init (); (Random.int 10)+1

(** [print_player_pos lst] outputs a textual representation of
    the position, balance, debt, and salary of each player in [lst]. *)
let rec print_player_pos lst =
  match lst with 
  | [] -> ()
  | h::t ->
    printf "%s: %d/%d squares, $%d bal. (-$%d), $%d sal.\n"
      (Player.get_name h) (Player.get_position h)
      (Game.board_length) (Player.get_balance h)
      ((Player.get_loans h) * 25000) (Player.get_salary_amt h);
    print_player_pos t

(** [choose_career (st, p, c1, c2)] prompts the user to choose
    between [c1] and [c2]. The chosen career is assigned to [p].

    Returns: [(st2, p2)] where [st2] and [p2] are the result of
    applying this change to [st] and [p]. 

    Effects: Prints [c1] and [c2] and prompts for user input.

    Requires: [p] is a player in [st], [c1] and [c2] are untaken
    careers in [st]. *)
let choose_career (st, p, c1, c2) =
  printf "\nChoose between these two careers.\n1) %a\n2) %a\n"
    Game.format_career (Some c1) Game.format_career (Some c2);
  let rec prompt_choice () =
    ANSITerminal.(print_string [red] "\nWhich one would you like? (1/2)\n> ");
    match String.trim(read_line ()) with 
    | "1" -> State.set_career st p c1
    | "2" -> State.set_career st p c2
    | _ -> print_string "\nCouldn't understand your input.\n";
      prompt_choice ()
  in prompt_choice ()

(* ENDGAME + helpers *)

(** [compare_player_bal (p1,b1) (p2,b2)] returns 1 if [b1 < b2],
    0 if [b1 = b2], and -1 otherwise. Note that this is the opposite of
    [Pervasives.compare]'s behavior.

    Requires: [b1] and [b2] can be compared with [Pervasives.compare].
    Example: [compare_player_bal (player1, 10,000) (player2, 20,000)]
    would return 1. *)
let compare_player_bal (p1,b1) (p2,b2) = 
  if b1 < b2 then 1 else if b1 = b2 then 0 else -1

(** [count_tiles p] is [p]'s current balance added to the values
    of their tiles.

    Effects: Prints out the value and description of each tile, followed
    by the player's total balance after tiles have been added,
    and prompts the player to continue. *)
let count_tiles p =
  let rec helper lst acc =
    match lst with
    | [] ->
      ANSITerminal.(printf [magenta] "\nTOTAL: $%d\n\n" acc);
      print_string "Press ENTER to continue.\n> ";
      ignore(read_line());
      acc 
    | h::t ->
      printf "%a\n" Game.format_tile h;
      helper t (acc+h.amt)
  in
  helper (Player.get_tiles p) (Player.get_balance p)

(** [endgame st] is ().

    Effects: Prints each player, their tiles and balance,
    and the winner (player with the highest balance). Exits gracefully.

    Requires: All players in [st] have retired. *)
let endgame st : unit =
  printf "\n\nWell, it seems everyone has retired. The winner is the one with
the most money... but as they say, it's the special moments in life
that are truly invaluable. So let's look at everyone's LIFE tiles and
tally up some bonus rewards!\n\n";
  let all_players = State.get_player_list st in

  (* Visit each player and print their tiles *)
  let rec count_balance st players acc =
    match players with 
    | [] -> let (winner,amt) = List.hd(List.sort compare_player_bal acc) in 
      printf "\n%s is the winner with $%d total! Congratulations, %s, 
and congratulations to everyone on their long
and happy lives.\n\nThank you for playing!\n\n" winner amt winner;
      exit 0
    | p::t ->
      let pname = Player.get_name p in 
      let loan_amt = 25000*(Player.get_loans p) in
      printf "\n%s, you have $%d right now, and are $%d in debt.
Let's see your LIFE tiles!\n"
        pname (Player.get_balance p) (loan_amt);
      match Player.get_house p with 
      | None -> failwith "Error in house assignment"
      | Some house ->
        let (st2, p2) =
          if house.price > 120000 then 
            (printf "(Your house is worth more than $120000,
so you get an extra tile.)\n"; State.add_tile st p)
          else (st,p) in 
        let ptotal = count_tiles p2 - loan_amt in 
        count_balance st2 t ((pname,ptotal)::acc)
  in count_balance st all_players []

(* SQUARE_ACTION + helpers *)

(** [get_target_player st p] switches [p]'s salary with another player's
    if the user consents.

    Returns: [(st2, p2)] where [st2] and [p2] are the result of
    applying this change to [st] and [p]. 

    Effects: Prompts for user input if [sq] is [Salary]. Prints result
    of the action. *)
let rec get_target_player st p = 
  try (
    ANSITerminal.
      (printf [red] "\nWho would you like to switch with?
(Or enter NVM to quit.)\n> ");
    let target_name = String.trim (read_line ()) in 
    if String.lowercase_ascii(target_name) = "nvm" then st, p
    else 
      let target = State.get_player st target_name in
      let (st2, p2) = State.switch_salary st p target in 
      printf "\nWoohoo! You switched salaries with %s.
Your salary is now $%d.\n"
        (Player.get_name target) (Player.get_salary_amt p2);
      st2, p2
  ) with
  | State.Player_not_found ->
    printf "\nCouldn't find that player, try again.\n";
    get_target_player st p
  | State.Bad_state ->
    printf "\nYou can't switch with that player, try again.\n";
    get_target_player st p

(** [do_switch st p sq] assigns [p] a new career/salary if [sq] is [Career],
    or switches [p]'s salary with another player's if [sq] is [Salary].

    Returns: [(st2, p2)] where [st2] and [p2] are the result of
    applying this change to [st] and [p]. 

    Effects: Prompts for user input if [sq] is [Salary]. Prints result
    of the action. *)
let rec do_switch st p (sq : Game.switch_square) = 
  match sq with 
  | Career -> (
      try (
        let (st2, p2) = State.switch_career st p in
        printf "\nYour new career is...\n%a\nYour new salary is...\n%a\n"
          Game.format_career (Player.get_career p2)
          Game.format_salary (Player.get_salary p2);
        st2, p2 
      ) with State.Bad_state ->
        printf "\nYou can't switch something you don't have!\n";
        st, p
    )
  | Salary ->
    (* Print all players' status *)
    printf "\nThis is how everyone's looking right now...\n";
    print_player_pos (State.get_player_list st);
    ANSITerminal.
      (printf [red] "\nWould you like to switch salaries? (Y/N)\n> ");
    match String.lowercase_ascii (String.trim (read_line ())) with 
    | "y" -> get_target_player st p
    | "n" -> printf "\nOkay, never mind then.\n"; st, p
    | _ -> printf "\nCouldn't understand your input.\n"; do_switch st p sq

(** [do_sell_house st p] assigns [p] a new house, deducts the price
    of the new house from [p]'s balance, and adds
    the price of their previous house to their balance.

    Returns: [(st2, p2)] where [st2] and [p2] are the result of
    applying this change to [st] and [p]. 

    Effects: Prompts for user consent to sell. Prints the result
    of the action.

    Raises: [Failure "Error in house assignment"] if [p] fails to
    receive a new house. *)
let do_sell_house st p =
  match Player.get_house p with 
  | None -> print_string "Hey, you can't sell a house you don't have!";
    st, p
  | Some h -> (
      printf "\nThis is your house.\n%a\n" Game.format_house (Some h);

      (* Prompt player to accept/reject house sale *)
      let rec prompt_sell () =
        ANSITerminal.
          (printf [red] "\nWould you like to sell? (Y/N)\n> ");
        match String.lowercase_ascii (String.trim (read_line ())) with 
        | "y" -> (
            let (st2, p2) = State.sell_house st p in
            match Player.get_house p2 with 
            | None -> failwith "Error in house assignment"
            | Some new_h -> (
                printf "\nCongrats! You sold your old house for $%d
and bought a new one!\n%a\n" h.price Game.format_house (Some new_h);
                printf "\nYou paid $%d for your new house.
Your balance is now $%d.\n" new_h.price (Player.get_balance p2);
                st2, p2
              )
          )
        | "n" ->
          print_string "\nOkay, never mind then.\n";
          st, p
        | _ -> print_string "\nCouldn't understand your input.\n";
          prompt_sell ()
      in prompt_sell ()
    )

(** [square_action st p sq] performs the action specified by [sq]
    on [p].

    Returns: [(st2, p2)] where [st2] and [p2] are the new state and
    player produced by performing the action on [p].  If [sq] is
    [Loss, Gain,] or [Payday], return [(st, p)] unchanged.

    Effects: Prints [sq] and player's position.

    Requires: [p] is currently on [sq],
    [sq] is not the starting square.

    Raises: [Failure "Bad board"] if [sq] is the starting square. *)
let square_action st p (sq : Game.square) = 
  printf "\nYou moved to position %d.\nSQUARE: %a" (Player.get_position p)
    (Game.format_square) sq;
  match sq with 
  | Start -> failwith "Bad board"
  | Loss(str,career) -> st, p
  | Gain(str) -> st, p
  | Payday -> st, p
  | Life_tile(str) -> State.add_tile st p
  | Baby(str,fam) -> State.add_fam st p fam
  | Switch(str, x) -> do_switch st p x
  | Stock(str) -> State.update_stock st p str
  | Stop(x) -> (
      match x with
      | Marriage -> State.add_fam st p Game.Spouse
      | House -> (
          let (st2, p2) = State.set_house st p in 
          let new_house_opt = Player.get_house p2 in
          match new_house_opt with
          | None -> failwith "Error in house assignment"
          | Some h -> (
              printf "%a\n" Game.format_house new_house_opt;
              printf "\nYou paid $%d for your new house.
Your balance is now $%d.\n" h.price (Player.get_balance p2);
              st2, p2
            )
        )
      | Job -> 
        let (st2, p2) = choose_career (State.select_career_salary st p true) in
        printf "\nYour career is...\n%a\nYour salary is...\n%a\n"
          Game.format_career (Player.get_career p2)
          Game.format_salary (Player.get_salary p2);
        st2, p2
      | Sell_house -> do_sell_house st p
      | Retire -> State.set_retired st p
    )

(* PARSE_INPUT + helpers *)

(** [parse_input st p] is [st2], the state produced by executing
    the user's inputted action on [st] and [p].

    Requires: [p] is a player in [st].

    Effects: Repeatedly prompts user input and
    prints the result until player inputs [Done]. If the user's input
    is [Exit], the game exits gracefully.*)
let rec parse_input st p players_left =
  ANSITerminal.(printf [red] "\nWhat would you like to do, %s?\n> "
                  (Player.get_name p));
  try (
    match Input.parse(read_line ()) with
    | View(v) -> do_viewable st p v players_left
    | Spin -> do_spin st p players_left
    | Pay(target, amt) -> do_pay st p target amt players_left
    | Take(target, amt) -> do_take st p target amt players_left
    | Borrow(amt) -> do_borrow st p amt players_left
    | Repay(amt) -> do_repay st p amt players_left
    | Insure(s) -> do_insure st p s players_left
    | Invest(stock) -> do_invest st p stock players_left
    | Help ->
      let ch = open_in "instructions.txt" in 
      let instr_str = really_input_string ch (in_channel_length ch) in 
      close_in ch; print_endline instr_str; parse_input st p players_left
    | Done -> if (Player.get_balance p < 0) then
        ((printf "\nYou can't end your turn on a negative balance!
Your current balance is $%d. Take out a loan!\n"
            (Player.get_balance p)); parse_input st p players_left) else st
    | Save ->
      print_string "\nYour game was saved to game_save.json.\n";
      let oc = open_out "game_save.json" in
      output_string oc (State.save_of_state st players_left);
      close_out oc;
      print_string "\nThanks for playing!\n\n";
      exit 0
    | Exit ->
      ANSITerminal.(printf [red] "\nAre you sure you want to quit? (Y/N)\n> ");
      match String.lowercase_ascii (String.trim (read_line ())) with 
      | "y" -> print_string "\nThanks for playing!\n\n"; exit 0
      | "n" -> print_string "\nResuming regularly scheduled programming.\n";
        parse_input st p players_left
      | _ -> print_string "\nCouldn't understand your input.\n";
        parse_input st p players_left
  )
  with Input.Bad_input ->
    print_string "\nCouldn't understand your input. Try again.\n";
    parse_input st p players_left

(** [do_viewable st p v] is [parse_input st p].

    Effects: Prints [v].

    Requires: [p] is a player in [st]. [v] must be "local",
    "global", or a player in [st]. *)
and do_viewable st p v players_left =
  try (
    (match v with 
     | "global" -> let all_players = State.get_player_list st in 
       print_string "\nAll player positions...\n";
       print_player_pos all_players
     | "local" -> let position = Player.get_position p in 
       printf "\nYou are at position %d.\n%a"
         position Game.format_board (position, Player.get_college p)
     | x -> let player = State.get_player st x in
       printf "\n%a" Player.format player
    ); parse_input st p players_left
  )
  with State.Player_not_found ->
    print_string "\nYou can't view that player.\n";
    parse_input st p players_left

(** [do_spin st p] moves [p] 1-10 spaces forward on the board,
    awards stock bonus to any players as needed,
    and takes the necessary actions required by the move.

    Effects: Prints the result of the actions (i.e. paydays and
    LIFE tiles accumulated).

    Requires: [p] is a player in [st], [p] is non-retired. *)
and do_spin st p players_left =
  let num_move = spin () in
  try (

    (* Move player to appropriate square *)
    let (st2, p2, sq) = State.move_player st p num_move in
    let balance_gain = (Player.get_balance p2) - (Player.get_balance p) in
    let num_paydays = if Player.get_salary_amt p2 = 0 then 0 else 
        balance_gain / (Player.get_salary_amt p2) in 
    printf "\nYou spun a %d!\nYou passed %d paydays and gained $%d.\n"
      num_move num_paydays balance_gain;

    (* Update state to award stock bonus *)
    let st_stock, is_curr_p = 
      match (State.stock_owner st2 num_move) with
      | None -> st, false
      | Some(pname) ->
        let is_curr_p = pname = Player.get_name p2 in 
        let pname_actual = if is_curr_p then "you"
          else pname in
        let p_update = if is_curr_p then p2
          else (State.get_player st2 pname) in
        printf "Also, %s gained $10000 in stock dividends.\n" pname_actual;
        fst (State.update_balance st2
               p_update "bank" 10000 true), is_curr_p
    in   

    (* Take action on the current square *)
    let p2_stock = if is_curr_p then State.get_player st2 (Player.get_name p2)
      else p2 in
    let st3, p3 = square_action st_stock p2_stock sq in 
    parse_input st3 p3 players_left

  ) with State.Bad_state ->
    print_string "\nYou reached the end of the board, so no more spins.\n";
    parse_input st p players_left

(** [do_pay st p target amt] transfers [amt] from [p]'s balance
    to the player whose name is [target],
    or to the bank if [target] is "bank" (case insensitive). 

    Effects: Prints result of money transfer.

    Requires: [p] is a player in [st],
    [target] is either the name of another
    player in [st] or "bank",
    [amt] is non-negative. *)
and do_pay st p target amt players_left =
  if (Player.get_balance p < amt) then
    (printf "\nYour current balance is $%d. You don't have
enough money! Take out a loan.\n"
       (Player.get_balance p);
     parse_input st p players_left)
  else
    try (
      let (st2, p2) = State.update_balance st p target amt false in 
      let target_the = if target = "bank" then "the bank" else target in
      printf "\nYou paid $%d to %s.\nYour balance is now $%d.\n"
        amt target_the (Player.get_balance p2);
      parse_input st2 p2 players_left
    ) with State.Player_not_found | State.Bad_state ->
      print_string "\nYou can't pay that player.\n";
      parse_input st p players_left

(** [do_take st p target amt] transfers [amt] to [p]'s balance
    from the player whose name is [target],
    or from the bank if [target] is "bank" (case insensitive). 

    Effects: Prints result of money transfer.

    Requires: [p] is a player in [st],
    [target] is either the name of another
    player in [st] or "bank",
    [amt] is non-negative. *)
and do_take st p target amt players_left =
  try (
    let (st2, p2) = State.update_balance st p target amt true in
    let target_the = if target = "bank" then "the bank" else target in
    printf "\nYou got $%d from %s.\nYour balance is now $%d.\n"
      amt target_the (Player.get_balance p2);
    parse_input st2 p2 players_left
  ) with State.Bad_state | State.Player_not_found ->
    print_string "\nYou can't take money from that player.\n";
    parse_input st p players_left

(** [do_borrow st p amt] issues a loan of [amt] to [p].

    Effects: Prints result of loan.

    Requires: [p] is a player in [st],
    [amt] is a multiple of 20000. *)
and do_borrow st p amt players_left =
  if ((amt mod 20000) <> 0) then
    (print_string "\nThe bank only loans in multiples of $20000!\n";
     parse_input st p players_left)
  else try
      (              
        let (st2, p2) = State.make_loan st p amt in
        let (st3, p3) = State.update_balance st2 p2 "bank" amt true
        in
        printf
          "\nYou borrowed $%d from the bank.\nYour balance is now $%d.
You now owe the bank $%d.\n"
          amt (Player.get_balance p3) ((Player.get_loans p3)*25000);                
        parse_input st3 p3 players_left
      )
    with State.Bad_state -> 
      print_endline "\nThat is an invalid loan amount.\n\n";
      parse_input st p players_left

(** [do_borrow st p amt] repays a loan of [amt] from [p].

    Effects: Prints result of repayment.

    Requires: [p] is a player in [st],
    [amt] is a multiple of 25000. *)
and do_repay st p amt players_left =
  if ((amt mod 25000) <> 0) then
    (print_string
       "\nThe bank only accepts repayments in multiples of $25000.\n";
     parse_input st p players_left)
  else if (amt > (Player.get_balance p)) then 
    (print_string "\nYou don't have enough money to repay this loan!\n";
     parse_input st p players_left) 
  else try
      (              
        let (st2, p2) = State.repay_loan st p amt in
        let (st3, p3) = State.update_balance
            st2 p2 "bank" amt false
        in
        printf
          "\nYou repaid $%d to the bank.\nYour balance is now $%d.
You now owe the bank $%d.\n"
          amt (Player.get_balance p3)
          ((Player.get_loans p3)*25000);                
        parse_input st3 p3 players_left
      )
    with State.Bad_state -> 
      print_string "\nThat is an invalid repayment.\n";
      parse_input st p players_left

(** [do_insure st p s] applies a new insurance policy to [s], where [s]
    can be either ["home"] or ["car"], belonging to [p]. 

    Returns: [(st2, p2)] where [st2] and [p2] are the result of
    applying this change to [st] and [p].

    Effects: Prints result of insurance action.

    Requires: [p] has enough balance to buy insurance. If [s] is ["home"],
    [p] has a house. *)
and do_insure st p s players_left =
  try (
    match s with 
    | "car" -> 
      (* Car insurance - stop player if they lack balance *)
      if (Player.get_balance p < 10000) then
        (printf "\nYour current balance is $%d. You don't have enough money!
Take a bank loan.\n" (Player.get_balance p);
         parse_input st p players_left)
      else
        let (st2, p2) =
          State.set_car_insure st p in
        printf "\nYou insured your car for $10000.
Your balance is now $%d.\n" (Player.get_balance p2);
        parse_input st2 p2 players_left
    | "home" ->
      (* Home insurance - stop player if they lack balance *)
      if (Player.get_balance p < (Player.get_insurance p)) then
        (printf "\nYour current balance is $%d. You don't have enough money!
Take a bank loan.\n" (Player.get_balance p);
         parse_input st p players_left)
      else 
        let (st2, p2) = State.set_home_insure st p in
        printf "\nYou insured your home for $%d.
Your balance is now $%d.\n" (Player.get_insurance p2) (Player.get_balance p2);
        parse_input st2 p2 players_left
    | _ ->
      (* Disallowed input *)
      print_string "\nYou can only insure your home or car.\n";
      parse_input st p players_left
  ) with State.Bad_state ->
    print_string "\nYou can't buy insurance if you already have it.
Also, to buy home insurance, you need a home first!\n";
    parse_input st p players_left

(** [do_invest st p stock] grants [stock] to [p] and deducts the cost
    from their balance.

    Effects: Prints result of stock action.

    Requires: [p] has enough balance to buy [stock], does not
    already own [stock], and is able to buy an additional stock.
    [stock] is a valid untaken stock. *)
and do_invest st p stock players_left =
  if (List.mem stock (Player.get_stocks p)) then
    (print_string
       "\nYou already own this stock!\n";
     parse_input st p players_left)  
  else if (List.length (Player.get_stocks p) > 0) then
    (print_string
       "\nYou can't buy more stocks!\n";
     parse_input st p players_left)
  else if (50000 > (Player.get_balance p)) then 
    (print_string "\nStocks are $50,000. You don't have enough!\n";
     parse_input st p players_left)
  else if (stock < 1 || stock > 10)
  then (print_string "\nThe stock exchange only sells stocks from 1 to 10!\n";
        parse_input st p players_left)
  else try
      ( 
        match (State.stock_owner st stock) with
        |Some(pname) ->
          (printf "\n%s already owns this stock!\n"
             pname; parse_input st p players_left)
        |None ->
          let (st2, p2) = State.update_stock st p (string_of_int stock) in
          let (st3, p3) = State.update_balance
              st2 p2 "bank" 50000 false
          in
          printf
            "\nYou paid $50000 to the bank.\nYour balance is now $%d.
You now own stock in the number %d.\n"
            (Player.get_balance p3)
            (stock);                
          parse_input st3 p3 players_left
      )
    with State.Bad_state -> (
        print_string "\nThat is an invalid investment.\n";
        parse_input st p players_left)

(* START GAME + TAKE TURNS *)

(** [take_turns st players_left] executes the turn of each player
    in [players_left].

    Requires: All players in [players_left] are players in [st]. *)
let rec take_turns st players_left =
  match players_left with
  | [] -> play_game st
  (* Restart loop once all players have gone once *)
  | h::t as players_left ->
    ANSITerminal.(printf [red] "\nIt's your turn, %s.\n"
                    (Player.get_name h));
    let st2 = parse_input st h players_left in
    take_turns st2 t

(** [play_game st] loops over the list of non-retired players,
    prompting for input from each in turn. Prints results of each
    player's actions and modifies the state for the next player.
    Once all players have retired, executes [endgame st']
    where [st'] is the final state of the game. *)
and play_game st =
  let nonret_players = State.get_nonret_player_list st in
  if nonret_players = [] then endgame st else
    take_turns st nonret_players

(** [start_game st] assigns careers and salaries to the players in
    [st] (those who do not go to college) and calls [play_game].

    Effects: Prints player careers and salaries and prompts each
    player to continue. *)
let start_game st =
  print_string "\nAs fresh-faced high school graduates,
you'll have to decide whether to go to college or
get a job. College means a $80,000 loan, but also 
the chance of a better career and salary!
It's all up to you.\n
Randomizing player order...\n";
  let colleges =
    [|"Cornell"; "Harvard"; "Ithaca College"; 
      "Yale"; "UC Berkeley"; "Princeton";
      "community college"; "MIT";
      "UT Austin (hook 'em)"; "Stanford";
      "National University of Singapore"|] in

  (* assign career/salary/college status *)
  let rec prompt_college st players =
    match players with 
    | [] ->
      (* start game once all players are assigned *)
      print_string "\nOkay, let's get started! Remember, you can 
find the instructions by typing \"help\" at any time.\n";
      play_game st 
    | p::t ->
      ANSITerminal.(printf [red]
                      "\nHello, %s! Would you like to go to college? (Y/N)\n> "
                      (Player.get_name p));
      match String.lowercase_ascii (String.trim (read_line ())) with 
      | "y" ->
        let (st2, p2) = State.make_loan st p 80000 in 
        let rand_college = colleges.(spin() - 1) in
        printf "\nHooray, you got accepted to... %s!
Buuut you have $100,000 in student debt (includes interest).\n" rand_college;
        print_string "\nPress ENTER to continue.\n> ";
        ignore(read_line());
        prompt_college st2 t
      | "n" ->
        let (st2, p2) = State.set_no_college st p in 
        let (st3, p3) = choose_career (State.select_career_salary st2 p2 false)
        in
        printf "\nYour career is...\n%a\nYour salary is...\n%a\n"
          Game.format_career (Player.get_career p3)
          Game.format_salary (Player.get_salary p3);
        print_string "\nPress ENTER to continue.\n> ";
        ignore(read_line());
        prompt_college st3 t
      | _ -> print_string "\nCouldn't understand your input.\n";
        prompt_college st players

  in prompt_college st (State.get_player_list st)

(** [parse_players str] is the initialized game state
    with player names specified in comma-separated [str] (user input).

    Effects: Will repeatedly prompt for user input until the
    inputted string obeys the below requirements.

    Requires: Number of comma-separated tokens is in the range 
    \[Game.min_players, Game.max_players\], all tokens are unique,
    and no tokens equal "bank", "global", "nvm" or "next". *)
let rec parse_players str =
  let names = Input.clean(String.split_on_char ',' str) in
  let length = List.length names in
  let reserved = ["bank";"next";"global";"nvm";] in
  if length < Game.min_players then (
    print_string "\nNot enough players. Please try again.\n> ";
    parse_players(read_line ())
  )
  else if length > Game.max_players then (
    print_string "\nToo many players. Please try again.\n> ";
    parse_players(read_line ())
  )
  else if List.sort compare (List.map String.lowercase_ascii names) <>
          List.sort_uniq compare (List.map String.lowercase_ascii names) then
    begin
      print_string
        "\nDuplicate player names not allowed. Please try again.\n> ";
      parse_players(read_line ()) 
    end
  else
    let rec check_reserved lst =
      match lst with 
      | [] -> start_game (State.init_state names)
      | h::t ->
        if List.mem h names
        then (printf "'%s' is a reserved keyword. Please try again.\n> " h;
              parse_players(read_line ()))
        else check_reserved t
    in check_reserved reserved

(**[main ()] prints the logo and authors' names to the terminal
   and prompts the user to enter player names. *)
let main () =
  ANSITerminal.(print_string [red] "
          _____ _             ____                               __ 
         |_   _| |__   ___   / ___| __ _ _ __ ___   ___    ___  / _|
           | | | '_ \\ / _ \\ | |  _ / _` | '_ ` _ \\ / _ \\  / _ \\| |_ 
           | | | | | |  __/ | |_| | (_| | | | | | |  __/ | (_) |  _|
           |_| |_| |_|\\___|  \\____|\\__,_|_| |_| |_|\\___|  \\___/|_|  
                                                            ");
  ANSITerminal.(print_string [green] "
  LLLLLLLLLLL             IIIIIIIIIIFFFFFFFFFFFFFFFFFFFFFFEEEEEEEEEEEEEEEEEEEEEE
  L:::::::::L             I::::::::IF::::::::::::::::::::FE::::::::::::::::::::E
  L:::::::::L             I::::::::IF::::::::::::::::::::FE::::::::::::::::::::E
  LL:::::::LL             II::::::IIFF::::::FFFFFFFFF::::FEE::::::EEEEEEEEE::::E
  L:::::L                 I::::I    F:::::F       FFFFFF  E:::::E       EEEEEE
  L:::::L                 I::::I    F:::::F               E:::::E             
  L:::::L                 I::::I    F::::::FFFFFFFFFF     E::::::EEEEEEEEEE   
  L:::::L                 I::::I    F:::::::::::::::F     E:::::::::::::::E   
  L:::::L                 I::::I    F:::::::::::::::F     E:::::::::::::::E   
  L:::::L                 I::::I    F::::::FFFFFFFFFF     E::::::EEEEEEEEEE   
  L:::::L                 I::::I    F:::::F               E:::::E             
  L:::::L         LLLLLL  I::::I    F:::::F               E:::::E       EEEEEE
  LL:::::::LLLLLLLLL:::::LII::::::IIFF:::::::FF           EE::::::EEEEEEEE:::::E
  L::::::::::::::::::::::LI::::::::IF::::::::FF           E::::::::::::::::::::E
  L::::::::::::::::::::::LI::::::::IF::::::::FF           E::::::::::::::::::::E
  LLLLLLLLLLLLLLLLLLLLLLLLIIIIIIIIIIFFFFFFFFFFF           EEEEEEEEEEEEEEEEEEEEEE
                                                                             ");
  ANSITerminal.(print_string [red] "
                by Whitney Denison, Marcus Ng, and Ally Zhang\n");

  (*  print_string "\nWelcome to the Game of LIFE!
      Would you like to load or start a new game? (L/N)\n";
      match String.lowercase_ascii (String.trim (read_line ())) with 
      | "l" ->
      begin
          (*parse file str into json, parse json into state, call take_turns
            on state and players_left (see above, I added it just now) *)
      end
      | "n" -> 
      begin
        print_string
          "Okay, please enter the comma-separated names of your players:\n>";
        match read_line () with
        | exception End_of_file -> ()
        | str -> parse_players str
      end
      | _ -> (print_string "\nCouldn't understand your input.\n";
            ())

      let () = main () *)

  print_string "\nWelcome to the Game of LIFE!
Please enter the comma-separated names of your players:\n> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> parse_players str

let () = main ()
