
type command = 
  | Go of int
  | Quit
  | One
  | Two
  | Help

exception Invalid

(**[words lst] is a list of the words (i.e. consecutive sequences of non-space
   characters) in [lst]. *)
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
        if (num > 0 && num < 8) then Go num else raise Invalid
      with exn -> raise Invalid
    end  
  | "h" :: [] -> Help
  | _ -> raise Invalid

let parse_menu str =
  match words (String.split_on_char ' ' (String.lowercase_ascii str)) with
  | "1" :: [] -> One
  | "2" :: [] -> Two
  | "quit" :: [] -> Quit
  | _ -> raise Invalid

let rec execute_command st d () = 
  let turn = State.turn st in
  let board = State.board st in
  let last_clr = State.other_color turn in
  if d then State.display board 1;
  if State.check_win board last_clr then 
    print_endline ("\n" ^ State.color_to_string last_clr ^ " wins!")
  else begin
    if d then print_endline ("\n" ^ State.color_to_string turn ^ "'s turn");
    print_string "> ";
    try match parse (read_line()) with
      | Go i -> 
        let new_state = State.move st i in
        if new_state = st then begin
          print_endline "That column is full, try another!";
          execute_command st false ()
        end
        else execute_command new_state true ()
      | Help -> 
        print_endline "Instructions: "; 
        print_endline "xxxx";
        execute_command st false ()
      | _ -> exit 0
    with 
    | Invalid -> 
      print_endline "Invalid move! Hint: type 'go' and a column number";
      execute_command st false ()
  end

let rec execute_menu_command () =
  print_string "\n> ";
  try match parse_menu (read_line()) with
    | One -> 
      print_endline "One player mode not implemented yet, please try another."; 
      execute_menu_command ()
    | Two -> 
      ANSITerminal.(print_string [red] "Starting Two Player Mode");
      print_endline " ";
      ANSITerminal.(print_string [cyan] "Type h for help at any time");
      print_endline " ";
      execute_command State.init_state true ()
    | _ -> exit 0
  with
  | Invalid -> 
    print_string "Invalid. Please enter "; 
    ANSITerminal.(print_string [cyan] "1");
    print_string " for One Player Mode or ";
    ANSITerminal.(print_string [cyan] "2");
    print_endline " for Two Player Mode";
    execute_menu_command ()
