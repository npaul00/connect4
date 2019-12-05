(**[main ()] starts the game engine and prompts the player.*)
let rec main () = 
  Command.execute_menu_command () State.move State.display

let () = main ()