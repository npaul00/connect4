

let rec main () = 
  ANSITerminal.(print_string [blue] "Connect Four");
  print_endline (" ");
  State.display (State.empty_board (State.empty) 1 1) 1;
  print_endline "\nBlack's turn";
  print_string "> ";
  Command.execute_command (State.init_state) ()

let () = main ()