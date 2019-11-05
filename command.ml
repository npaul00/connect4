
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
      with exn -> raise (Invalid)
    end   
  | _ -> raise (Invalid)

let rec execute_command st () = 
  let last_clr = (State.other_color (State.turn st)) in
  if (State.check_win (State.board st) last_clr) then print_string 
      (State.color_to_string last_clr ^ " wins!")
  else
    match read_line () with
    | exception End_of_file -> ()
    | command -> 
      try match parse command with
        | Quit -> exit 0
        | Go i -> 
          let new_state = State.move st i in
          if new_state = st then print_endline("Invalid") 
          else begin
            State.display (State.board new_state) 1;
            print_endline ("\n" ^ (State.color_to_string (State.turn new_state)) ^ "'s turn")
          end;
          print_string "> ";
          execute_command new_state ()
      with 
      | Invalid -> print_endline "Invalid";
        print_string "> ";
        execute_command st()