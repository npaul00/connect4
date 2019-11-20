(**[main ()] starts the game engine and prompts the player.*)
let rec main () = 
  Command.menu ();
  Command.execute_menu_command ()

let () = main ()