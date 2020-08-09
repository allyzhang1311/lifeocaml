(**
   Parsing of keyboard input from game players.

   @author Marcus, Whitney
*)

(** [input] is the type representing a player command. Every player input
    will match one of the below constructors, as determined by the first
    token of the input string. Arguments, if any, are the strings following
    after that initial token.

    Example: input ["borrow 20000"] is equivalent to [Borrow (20000)];
    input ["pay player2 1000"] is equivalent to [Pay ("player2", 1000)]. *)
type input =
  | View of string
  | Spin
  | Pay of string * int 
  | Take of string * int 
  | Borrow of int
  | Repay of int
  | Insure of string
  | Invest of int
  | Help
  | Done
  | Save
  | Exit 

(** [Bad_input] is the exception raised when user input is empty or
    does not match the format specified by the [input] constructor for
    the given token. *)
exception Bad_input

(** [clean lst] is [lst] with empty members removed and leading/
    trailing whitespace stripped from each member. Order is preserved.

    Example: [\["  view  "; ""; "player2  "]\]
    returns [\["view";"player2"]\]. *)
val clean : string list -> string list

(** [parse str] parses user input into the [input] type. The first word 
    (consecutive non-space character sequence) determines the [input]
    constructor; any other words become the argument. Whitespace
    and capitalization are discarded.

    Example: ["     sPiN  "] returns [Spin],
    ["insure    home"] returns [Insure ("home")].

    Requires: [str] contains only ASCII characters.
    Raises: [Bad_input] if the input is empty/contains only whitespace, 
    or if it does not match any of the [input] constructors. *)
val parse : string -> input