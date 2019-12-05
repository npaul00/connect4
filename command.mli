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

(**Raised when an invalid command is entered. *)
exception Invalid

(** [menu ()] is the display of options for the start menu. *)
val menu: unit -> unit

(**[parse str] parses the player's input into a relevant [command] for 
   during game play.
   Requires: [str] contains only (A-Z, a-z, 0-9), and space characters.
   Raises: [Invalid] if the input doesn't match [Go of int], [Quit], or [Help]
   or if the integer following go is not between 1 and 7. *)
val parse: string -> command

(**[parse_menu str] parses the player's input into a relevant [command] for 
   interacting with the start menu.
   Requires: [str] contains only (A-Z, a-z, 0-9), and space characters.
   Raises: [Invalid] if the input doesn't match [One], [Two], [Three], or 
   [Quit]. *)
val parse_menu: string -> command

(**[one_play st d ()] executes the player's input [i] during a one player 
   game by executing the command resulting from [parse i]. The game board of
   state [st] is displayed if [d] is true. If the winning condition has been 
   met, a winning message is displayed. *)
val one_play: State.t -> bool -> unit -> unit

(**[two_play st d last ()] executes the player's input [i] during a two player 
   game by executing the command resulting from [parse i]. The game board of
   state [st] is displayed if [d] is true. If the winning condition has been 
   met, a winning message is displayed. *)
val two_play: State.t -> bool -> int -> unit -> unit

(**[execute_menu_command ()] executes the player's input [i] at the start menu
   by executing the command resulting from [parse_menu i] *)
val execute_menu_command: unit -> unit
