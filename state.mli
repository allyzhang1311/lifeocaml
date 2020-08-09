(** 
   Representation of dynamic game state.

   This module represents the current state of the game during a
   play session, including the list of players and the available cards/
   tiles. It also contains functions for modifying game state.

   @author Ally, Marcus
*)

(* TYPES/EXCEPTIONS *)

(** [state] is the abstract type representing the
    dynamic state of a game and its players. *)
type state

(** [Bad_state] is the exception raised when attempting to
    alter a player's position/balance in a way that violates
    game rules. (See [move_player] and
    [update_balance.]) *)
exception Bad_state

(** [Player_not_found] is the exception raised when attempting
    to access or alter a player that does not exist in the state. *)
exception Player_not_found

(* INIT/ACCESSOR FUNCTIONS *)

(** [init_state pl] is the initial state of a game that has players with
    the names specified in [pl]. *)
val init_state : string list -> state

(** [get_player_list st] is the list of players in [st].*)
val get_player_list : state -> Player.player list

(** [get_nonret_player_list st] is the list of non-retired players in [st]. *)
val get_nonret_player_list : state -> Player.player list

(** [get_player_list st] is the list of player names in [st]. *)
val get_player_names : state -> string list

(** [get_player st pname] is the player in [st] with name [pname],
    case insensitive. 

    Requires: [st] is a valid state,
    the player named [pname] is a player in [st].

    Raises: [Player_not_found] if [pname] is not a player in [st]. *)
val get_player : state -> string -> Player.player

(* MUTATOR FUNCTIONS *)

(** [select_career_salary st p] assigns a random salary from [st]
    to [p] and selects two random careers for the user to choose between.
    If [college] is [false],
    the careers and salaries are limited to those that
    do not require degrees.

    Returns: [(st2, p2)] where [st2] and [p2] are the new state and
    player produced by applying this change to [st] and [p].

    Requires: [st] has at least one untaken career and salary,
    [p] is a player in [st]. *)
val select_career_salary : state -> Player.player -> bool ->
  state * Player.player * Game.career * Game.career

(** [set_career st p c] assigns [c] to [p].

    Requires: [c] is untaken,
    [p] is a player in [st]. *)
val set_career : state -> Player.player -> Game.career ->
  state * Player.player

(** [set_house st p] assigns a random untaken
    house to [p].

    Returns: [(st2, p2)] where [st2] and [p2] are the new state and
    player produced by applying this change to [st] and [p].

    Requires: [st] has at least one untaken house,
    [p] is a player in [st].
    Raises: [Player_not_found] if [p] is not a player in [st]. *)
val set_house : state -> Player.player -> state * Player.player

(** [set_car_insure st p] grants [p] a car insurance policy.

    Returns: [(st2, p2)] where [st2] and [p2] are the new state and
    player produced by applying this change to [st] and [p].

    Requires: [p] does not already have car insurance,
    [p] is a player in [st].
    Raises: [Player_not_found] if [p] is not a player in [st],
    [Bad_state] if [p] already has car insurance. *)
val set_car_insure : state -> Player.player -> state * Player.player

(** [set_home_insure st p] grants [p] a home insurance policy.

    Returns: [(st2, p2)] where [st2] and [p2] are the new state and
    player produced by applying this change to [st] and [p].

    Requires: [p] does not already have home insurance,
    [p] has a house,
    [p] is a player in [st].

    Raises: [Player_not_found] if [p] is not a player in [st],
    [Bad_state] if [p] already has home insurance or has
    no house. *)
val set_home_insure : state -> Player.player -> state * Player.player

(** [set_no_college st p] changes [p]'s college degree status to false.

    Returns: [(st2, p2)] where [st2] and [p2] are the new state and
    player produced by applying this change to [st] and [p].

    Requires: [p] is a player in [st].

    Raises: [Player_not_found] if [p] is not a player in [st]. *)
val set_no_college : state -> Player.player -> state * Player.player

(** [set_retired st p] sets [p]'s retirement status to true.

    Returns: [(st2, p2)] where [st2] and [p2] are the new state
    and player produced by applying this change to [st] and [p].

    Requires: [p] is a player in [st].

    Raises: [Player_not_found] if [p] is not a player in [st].  *)
val set_retired : state -> Player.player -> state * Player.player

(** [add_tile st p] attempts to add a random tile from [st]'s list of
    tiles to [p]'s list of tiles.

    Returns: [(st2, p2)] where [st2] and [p2] are the new state
    and player produced by applying this change to [st] and [p].
    If [st]'s tile list is empty, returns [(st, p)] unchanged.

    Requires: [p] is a player in [st].

    Raises: [Player_not_found] if [p] is not a player in [st]. *)
val add_tile : state -> Player.player -> state * Player.player

(** [add_fam st p fam] adds [fam] to [p], and adds 
    a random tile from [st]'s list of tiles to [p]'s list of tiles.

    Returns: [(st2, p2)] where [st2] and [p2] are the new state
    and player produced by applying this change to [st] and [p].

    Requires: [p] is a player in [st].

    Raises: [Player_not_found] if [p] is not a player in [st].  *)
val add_fam : state -> Player.player -> Game.family_mem ->
  state * Player.player

(** [make_loan st p amt_loan] is the tuple of the new state and player
    after updating the number of loans of player [p] in [st]
    with loaned amount [amt_loan] in dollars.

    Requires: [p] is a player in [st].

    Raises: [Bad_state] if the loan amount is less than 20000,
    [Player_not_found] if [p] is not a player in [st]. *)
val make_loan : state -> Player.player -> int -> state * Player.player

(** [repay_loan st p amt] is the tuple of the new state and player
    after updating the number of loans of player [p] in [st]
    with repaid amount [amt] in dollars.

    Requires: [p] is a player in [st].

    Raises: [Bad_state] if the loan amount is less than 20000,
    [Player_not_found] if [p] is not a player in [st]. *)
val repay_loan : state -> Player.player -> int -> state * Player.player

(** [move_player st p num] attempts to move [p] [num] spaces in [st].
    Returns [(st2, p2, sq)] where [sq] is the square reached by [p],
    and [st2] and [p2] are the new state and player produced by
    applying this change to [st] and [p].

    Requires: [st] is a valid state,
    [p] is a non-retired player in [p].

    Raises: [Bad_state] if [p] is retired,
    [Player_not_found] if [p] is not a player in [st].  *)
val move_player : state -> Player.player ->
  int -> state * Player.player * Game.square

(** [switch_salary st pa pb] trades [pa]'s salary card with
    [pb]'s.

    Returns: [(st2, pa2)] where [st2] and [pa2] are the new
    state and player produced by applying this change to [st]
    and [pa].

    Requires: Both [pa] and [pb] have salaries, and neither
    are retired. [pa] and [pb] are not the same player.

    Raises: [Bad_state] if any of the above requirements are
    not met,
    [Player_not_found] if [pa] or [pb] are not players in [st]. *)
val switch_salary : state -> Player.player -> Player.player ->
  state * Player.player

(** [switch_career st p] assigns a random career/salary to [p]
    and makes their previous ones available to other players.

    Returns: [(st2, p2)] where [st2] and [p2] are the new
    state and player produced by applying this change to [st]
    and [p].

    Requires: [p] has an existing career and salary,
    [p] is a player in [st].

    Raises: [Bad_state] if [p] has no existing career or salary,
    [Player_not_found] if [p] is not a player in [st]. *)
val switch_career : state -> Player.player -> state * Player.player

(** [sell_house st p] assigns a random house to [p], deducts the price
    of the new house from [p]'s balance, and adds
    the price of their previous house to their balance.

    Returns: [(st2, p2)] where [st2] and [p2] are the new
    state and player produced by applying this change to [st]
    and [p].

    Requires: [p] has an existing house,
    [p] is a player in [st].

    Raises: [Failure "Bad board"] if [p] has no existing house,
    [Player_not_found] if [p] is not a player in [st]. *)
val sell_house : state -> Player.player -> state * Player.player

(** [update_balance st src tgt_string amt add] locates the 
    player [tgt] in [st] whose name is [tgt_string] (case insensitive).
    (If [tgt_string] is "bank", the target is the bank.)
    If [add] is [true], [amt] is deducted from [tgt]'s balance
    and added to [src]'s balance; otherwise, [amt] is deducted
    from [src]'s balance and added to [tgt]'s balance.

    Returns: [(st2, src2)] where [st2] and [src2] are the new state
    and player produced by applying this change to [st] and [src]. 

    Example: [update_balance st player1 "player2" 1000 true] transfers
    $1000 from player2 to player1.

    Requires:
    - [tgt_string] is a lowercase ASCII string,
    - [amt] is non-negative,
    - [tgt] exists,
    - [src] and [tgt] have different names,
    - and [src] and [tgt] are both players in [st].

    Raises: [Bad_state] if [src]'s name
    matches [tgt_string] (case insensitive), 
    if [amt] is non-negative, or if [tgt] is retired.
    [Player_not_found] if [p] is not a player in [st]. *)
val update_balance : state -> Player.player -> string -> int -> bool ->
  state * Player.player

(** [update_stock st p str] assigns a new stock to [p] if [str] is "boom",
    removes a stock if [str] is "bust", or assigns the stock named in [str]
    to [p] otherwise.

    Requires: [p] is a player in [st].

    Returns: [(st2, p2)] where [st2] and [p2] are the new state
    and player produced by applying this change to [st] and [p2]. 

    Raises: [Bad_state] if [str] is not "boom", "bust", or an integer.
*)
val update_stock : state -> Player.player -> string -> state * Player.player

val remove_stock : state -> int -> state

val sell_house : state -> Player.player -> state * Player.player
(* val save_of_stocks : (int * Player.player) list -> string *)

val save_of_state : state -> Player.player list -> string
(** [stock_owned st stock] is [true] if [stock] is owned, [false] otherwise.
*)
val stock_owned : state -> Game.stock -> bool

(** [stock_owner st stock] is [Some pname] if there is a player in
    [st] named [pname] that owns [stock],
    or None if the stock is unowned.
*)
val stock_owner : state -> Game.stock -> string option
