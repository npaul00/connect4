
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
  let turn = State.turn st in
  let board = State.board st in
  let last_clr = State.other_color turn in
  State.display board 1;
  if State.check_win board last_clr 
  then print_endline ("\n" ^ State.color_to_string last_clr ^ " wins!")
  else begin
    print_endline ("\n" ^ (State.color_to_string turn) ^ "'s turn");
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | command -> 
      try match parse command with
        | Quit -> exit 0
        | Go i -> 
          let new_state = State.move st i in
          if new_state = st then print_endline("Invalid") 
          else execute_command new_state ()
      with 
      | Invalid -> print_endline "Invalid";
        execute_command st()
  end