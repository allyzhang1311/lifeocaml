(** 
   Representation of the static Game of LIFE.

   This module represents the data stored in the game materials
   (board, cards, tiles). It also contains functions for displaying
   these features.

   @author Ally
*)

(* TYPES *)

(** [career] is the type representing a career card. *)
type career = {
  name : string;
  effect : string;
  degree : bool;
}

(** [house] is the type representing a house card. *)
type house = {
  name : string;
  desc : string;
  price : int;
  insurance : int;
}

(** [salary] is the type representing a salary card. *)
type salary = {
  amt : int;
  taxes : int;
  degree : bool;
}

(** [tile] is the type representing a LIFE tile. *)
type tile = {
  desc : string;
  amt: int;}

(** [stock] is the type representing a stock. *)
type stock = int

(** [family_mem] is the type representing a family member. *)
type family_mem = Spouse | Son | Daughter

(** [stop_square] is the type representing a stop square, i.e.
    a square on the board that forces the player to land there
    instead of passing it. *)
type stop_square = 
  | Marriage
  | House
  | Job
  | Sell_house
  | Retire

(** [switch_square] is the type representing a switch square on the board. *)
type switch_square = 
  | Career
  | Salary

(** [square] is the type representing a square on the board. *)
type square =
  | Start
  | Payday
  (* description, target career if any *)  
  | Loss of string * string option
  | Gain of string
  | Life_tile of string
  | Baby of string * family_mem
  | Switch of string * switch_square
  | Stop of stop_square
  | Stock of string

(** [board] is the type of a game board, i.e. an array of squares. *)
type board = square array

(* CARDS/TILES *)

(** [careers] is the list of career cards in the game. *)
val careers : career list

(** [houses] is the list of house cards in the game. *)
val houses : house list

(** [salaries] is the list of salary cards in the game. *)
val salaries : salary list 

(** [tiles] is the list of tiles cards in the game. *)
val tiles : tile list

(** [stocks] is the list of stock cards in the game. *)
val stocks : stock list

(* BOARD *)

(** [full_board] is the board used for this game. *)
val full_board : board

(** [college_branch] is the section of the board used by
    college student players. *)
val college_branch : board

(** [min_players] is the minimum number of players allowed by the game. *)
val min_players : int

(** [max_players] is the maximum number of players allowed by the game. *)
val max_players : int

(** [board_length] is the length of [full_board] excluding
    the Start square. *)
val board_length : int

val get_career_name : career -> string
val get_house_name : house -> string
val save_of_salary : salary -> string
val save_of_tile : tile -> string
val save_of_family : family_mem -> string
(* FORMATTERS *)

(**[format_career fmt career] outputs a textual representation of [career],
   a career option, on [fmt]. *)
val format_career : Format.formatter -> career option -> unit

(**[format_house fmt house] outputs a textual representation of [house],
   a house option, on [fmt]. *)
val format_house : Format.formatter -> house option -> unit

(**[format_salary fmt salary] outputs a textual representation of [salary],
   a salary option, on [fmt]. *)
val format_salary : Format.formatter -> salary option -> unit

(**[format_family_lst fmt lst] outputs a textual representation of [lst],
   a list of [family_mem]s, on [fmt]. *)
val format_family_lst : Format.formatter -> family_mem list -> unit

(**[format_tile fmt tile] outputs a textual representation of [tile]
   on [fmt]. *)
val format_tile : Format.formatter -> tile -> unit

(**[format_square fmt st] outputs a textual representation of [st],
   a list of stocks, on [fmt]. *)
val format_stocks : Format.formatter -> stock list -> unit

(**[format_square fmt sq] outputs a textual representation of [sq]
   on [fmt]. *)
val format_square : Format.formatter -> square -> unit

(**[format_board fmt (position, college)] outputs (to [fmt])
   the squares from [position] - 5 to [position] + 10.
   If [college] is true, squares from [college_branch] will
   be substituted at the appropriate places.
   Requires: [position] is a valid index of [Game.full_board]. *)
val format_board : Format.formatter -> int * bool -> unit
