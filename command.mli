(**[execute_menu_command () mov dis] executes the player's input [i] at the 
   start menu by executing the command resulting from [parse_menu i] *)
val execute_menu_command: unit -> (State.t -> int -> State.t) -> 
  (State.board -> int -> unit) -> unit
