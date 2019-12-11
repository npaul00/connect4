
let rec main () = 
  ANSITerminal.resize 90 30;
  Command.execute_menu_command () State.move State.display

let () = main ()