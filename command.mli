(**The type [command] represents a player command. *)
type command = 
  | Go of int
  | Quit
  | One
  | Two
  | Help

(**Raised when an invalid command is entered. *)
exception Invalid

(**[parse str] parses the player's input into a [command].
   Requires: [str] contains only (A-Z, a-z, 0-9), and space characters.
   Raises: [Invalid] if the input doesn't match a [command] or if the int 
   following go is not between 1 and 7.  *)
val parse: string -> command

(**[execute_command st d ()] executes the command that the player's input parses 
   to. First, it displays the game board of state [st] if [d] is true. 
   Then, if the winning condition has been met, a winning message is displayed. 
   Else, the player's input [i] is read, parsed, and executed. 
   If [parse i] is [Quit], it exits the game engine.
   If [parse i] is [Go c] and c is full, [execute_command st false () is called]
   If [parse i] is [Go c] and c is not full, the move is executed to a new 
   game state [n] and [execute_command n true ()] is called.
   If [parse i] raises [Invalid], [execute_command st false ()] is called.*)
val execute_command: State.t -> bool -> unit -> unit

val execute_menu_command: unit -> unit