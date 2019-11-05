

let rec main () = 
  ANSITerminal.(print_string [blue] "Connect Four");
  print_endline (" ");
  Command.execute_command (State.init_state) ()

let () = main ()