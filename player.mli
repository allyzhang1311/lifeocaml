(** 
   Representation of a dynamic game player.

   This module represents the current state of a player during
   a game session, including their name, cards/tiles, insurance,
   and loans. It also contains functions for modifying player state
   and printing.

   @author Ally, Marcus
*)

(** [player] is the type representing a game player. *)
type player

(* INIT/ACCESSOR FUNCTIONS *)

(**[init_player n] initializes a player with name [n]. *)
val init_player : string -> player

(**[get_name p] is [p]'s name. *)
val get_name : player -> string

(**[get_salary p] is [Some s] where [s] is [p]'s salary,
   or [None] if [p] has no salary. *)
val get_salary : player -> Game.salary option

(**[get_salary_amt p] is [p]'s salary as an integer,
   or 0 if [p] has no salary. *)
val get_salary_amt : player -> int

(**[get_career p] is [Some c] where [c] is [p]'s career,
   or [None] if [p] has no career. *)
val get_career : player -> Game.career option

(**[get_retired p] is [true] if [p] is retired, [false] otherwise. *)
val get_retired : player -> bool

(** [get_position p] is the board position of [p]. *)
val get_position : player -> int

(** [get_tiles p] is the list of tiles owned by [p]. *)
val get_tiles : player -> Game.tile list

(** [get_balance p] is the bank balance of [p]. *)
val get_balance : player -> int

(** [get_house p] is [Some h] where [h] is [p]'s house,
    or [None] if [p] has no house. *)
val get_house : player -> Game.house option

(** [get_insurance p] is [h.insurance] is [p]'s house is [Some h],
    0 otherwise. *)
val get_insurance : player -> int

(** [get_loan p] is the number of loans [p] has.*)
val get_loans : player -> int

(** [get_square p] is the square corresponding to [p]'s current
    position. 
    Requires: [p]'s position is within the array index bounds of 
    [Game.full_board].*)
val get_square : player -> Game.square

(** [get_car_insure_bool p] is [true] if [p] has purchased
    car insurance, [false] otherwise. *)
val get_car_insure_bool : player -> bool

(** [get_home_insure_bool p] is [true] if [p] has purchased
    home insurance, [false] otherwise. *)
val get_home_insure_bool : player -> bool

(** [get_college p] is [true] if [p] has elected to go to 
    college, [false] otherwise. *)
val get_college : player -> bool

(** [get_stocks p] is [p]'s current list of stocks. *)
val get_stocks : player -> int list

(* MUTATOR FUNCTIONS *)

(**[set_salary p s] is [p] with salary set to [s]. *)
val set_salary : player -> Game.salary -> player

(**[set_career p c] is [p] with career is set to [c]. *)
val set_career : player -> Game.career -> player

(**[set_house p h] is [p] with house set to [h]. *)
val set_house : player -> Game.house -> player

(**[set_retired p] is [p] with retired status set to [true]. *)
val set_retired : player -> player

(** [set_loan p l] is [p] after their number of loans is set to [l]. *)
val set_loans :  player -> int -> player

(** [set_home_insure p] is [p] with home insurance status set to [true]. *)
val set_home_insure : player -> player

(** [set_car_insure p] is [p] with car insurance status set to [true]. *) 
val set_car_insure : player -> player

(** [set_no_insure p] is [p] with college status set to [false]. *)
val set_no_college : player -> player

(** [set_stocks p intls] is [p] with stock list set to [intls]. *)
val set_stocks : player -> int list -> player

(** [add_tile p tile] is [p] with [tile] prepended
    to its list of tiles. *)
val add_tile : player -> Game.tile -> player

(**[add_fam p fam] is [p] with [fam] prepended to its list of
   family members. *)
val add_fam : player -> Game.family_mem -> player

(** [update_balance p amt add] is [p] after adding [amt] to [p]'s
    balance (if [add] is true), or subtracting [amt] from [p]'s balance
    (otherwise).

    Requires: [amt] is non-negative. *)
val update_balance : player -> int -> bool -> player

(**[move p num] attempts to move [p] [num] spaces
   forward on the board.

   Returns: [(p2, sq)] where [sq] is the square reached by
   [p], and [p2] is the new player produced by applying this
   change to [p].

   Requires: [num] is in the range \[1, 10\]. *)
val move : player -> int -> player

(** [format] is a printing function suitable for use
    with the toplevel's [#install_printer] directive.
    It outputs a textual representation of a player. *)
val format : Format.formatter -> player -> unit

val save_of_player : player -> string
