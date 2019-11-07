(** [menu ()] is the display of options for the start menu. *)
let menu () =
  ANSITerminal.(print_string [red; Underlined] "   Connect Four   ");
  ANSITerminal.(print_endline "\n\nSelect a Game Mode:");
  ANSITerminal.(print_string [cyan] " (1) ");
  ANSITerminal.(print_endline "One Player");
  ANSITerminal.(print_string [cyan] " (2) ");
  ANSITerminal.(print_endline "Two Player");
  ANSITerminal.(print_string [cyan] " (3) ");
  ANSITerminal.(print_endline "Instructions")

(**[main ()] starts the game engine and prompts the player.*)
let rec main () = 
  menu ();
  Command.execute_menu_command ()

let () = main ()