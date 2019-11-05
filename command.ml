
type command = 
  | Go of int
  | Quit

exception Invalid

let rec words = function
  | [] -> []
  | "" :: t -> words t 
  | h :: t -> h :: words t

let parse str = 
  match words (String.split_on_char ' ' (String.lowercase_ascii str)) with
  | "quit" :: [] -> Quit
  | "go" :: i :: [] -> begin
      try 
        let num = int_of_string i in
        if (num>0 && num<8) then 
          Go num else raise(Invalid)
      with exn -> raise(Invalid)
    end   
  | _ -> raise(Invalid)

let rec execute_command st () = 
  match read_line () with
  | exception End_of_file -> ()
  | command -> 
    try match parse command with
      | Quit -> exit 0
      | Go i -> 
        let new_state = State.move st i in
        print_endline ("going " ^ (string_of_int i));
        print_string "> ";
        execute_command new_state ()
    with 
    |Invalid -> print_endline "Invalid";
      print_string "> ";
      execute_command st()