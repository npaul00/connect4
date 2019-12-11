(**
   Parsing of player commands.
*)

(**The type [command] represents a player command. *)
type command = 
  | Go of int
  | Quit
  | One
  | Two
  | Three
  | Four
  | Help
  | AgainYes
  | AgainNo
  | Stats
  | MainMenu
  | Easy
  | Medium
  | Hard
  | Back
  | Animation
  | Settings
  | Night

(**Raised when an invalid command is entered. *)
exception Invalid

(**[execute_menu_command () mov dis] executes the player's input [i] at the 
   start menu by executing the command resulting from [parse_menu i] *)
val execute_menu_command: unit -> (State.t -> int -> State.t) -> 
  (State.board -> int -> unit) -> unit
