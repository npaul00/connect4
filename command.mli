
type command = 
  | Go of int
  | Quit

(**Raised when an invalid command is entered. *)
exception Invalid

val parse: string -> command
val execute_command: unit -> unit