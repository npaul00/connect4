
let rec main () = 
  ANSITerminal.(print_string [blue] "Connect Four");
  print_endline "\nEnter a command";
  print_string "> ";
  Command.execute_command ()

let () = main ()