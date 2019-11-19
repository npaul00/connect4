
type command = 
  | Go of int
  | Quit
  | One
  | Two
  | Three
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
  | "help" :: [] -> Help
  | _ -> raise Invalid

let parse_menu str =
  match words (String.split_on_char ' ' (String.lowercase_ascii str)) with
  | "1" :: [] -> One
  | "2" :: [] -> Two
  | "3" :: [] -> Three
  | "quit" :: [] -> Quit
  | _ -> raise Invalid

let help_message () = 
  ANSITerminal.(print_string [yellow; Underlined] "Instructions: "); 
  ANSITerminal.(print_string [yellow] "\n - Type 'go' followed by a column number to drop a piece of your color in that column.");
  ANSITerminal.(print_string [yellow] "\nOnce a column is filled, you can no longer place pieces there.");
  ANSITerminal.(print_string [yellow] "\n - Game continues until one player gets four of their colored pieces in a row,"); 
  ANSITerminal.(print_string [yellow] "\neither horizontally, vertically, or diagonally.");
  ANSITerminal.(print_string [yellow] "\n - Type 'quit' at any time to exit the game,");
  ANSITerminal.(print_string [yellow] "\nor 'help' to bring up these instructions again.");
  print_endline ""

let instructions_message () =
  print_endline " ";
  ANSITerminal.(print_string [yellow; Underlined] "   Instructions   ");
  ANSITerminal.(print_string [yellow] "\n - To win, get four of your pieces in a row on the board. The sequence of four pieces can be horizontal, vertical, or diagonal.");
  ANSITerminal.(print_string [yellow] "\n - In one player mode, you play Connect Four with an A.I. Enter '1' to go to one player mode.");
  ANSITerminal.(print_string [yellow] "\n - In two player mode, two players can play Connect Four against each other. Enter '2' to go to two player mode.");
  print_endline " "

let rec two_play st d () = 
  let turn = State.turn st in
  let board = State.board st in
  let last_clr = State.other_color turn in
  if d then State.display board 1;
  if State.check_win board last_clr then 
    ANSITerminal.
      (print_string [Blink] 
         ("\n" ^ State.color_to_string last_clr ^ " wins!\n"))
  else begin
    if d then print_endline ("\n" ^ State.color_to_string turn ^ "'s turn");
    print_string "> ";
    try match parse (read_line()) with
      | Go i -> 
        let new_state = State.move st i in
        if new_state = st then begin
          print_endline "That column is full, try another!";
          two_play st false ()
        end
        else two_play new_state true ()
      | Help -> 
        help_message ();
        two_play st true ()
      | _ -> exit 0
    with 
    | Invalid -> 
      print_endline "Invalid move! Hint: type 'go' and a column number";
      two_play st false ()
  end

let rec one_play st d () = 
  let turn = State.turn st in
  let board = State.board st in
  let last_clr = State.other_color turn in
  if d then State.display board 1;
  if State.check_win board last_clr then 
    ANSITerminal.
      (print_string [Blink] 
         ("\n" ^ State.color_to_string last_clr ^ " wins!\n"))
  else begin
    if d then print_endline ("\n" ^ State.color_to_string turn ^ "'s turn");
    print_string "> ";
    try match parse (read_line()) with
      | Go i -> 
        let new_state = State.move st i in
        if new_state = st then begin
          print_endline "That column is full, try another!";
          one_play st false ()
        end
        else one_play new_state true ()
      | Help -> 
        help_message ();
        one_play st true ()
      | _ -> exit 0
    with 
    | Invalid -> 
      print_endline "Invalid move! Hint: type 'go' and a column number";
      one_play st false ()
  end

let rec execute_menu_command () =
  print_string "\n> ";
  try match parse_menu (read_line()) with
    | One -> 
      print_endline "One player mode not implemented yet, please try another."; 
      one_play State.init_state true ()
    | Two -> 
      ANSITerminal.(print_string [red] "Starting Two Player Mode");
      ANSITerminal.(print_string [cyan] "\nType 'help' for help at any time");
      print_endline " ";
      two_play State.init_state true ()
    | Three -> 
      instructions_message ();
      execute_menu_command ()
    | _ -> exit 0
  with
  | Invalid -> 
    print_string "Invalid. Please enter "; 
    ANSITerminal.(print_string [cyan] "1");
    print_string " for One Player Mode or ";
    ANSITerminal.(print_string [cyan] "2");
    print_endline " for Two Player Mode";
    execute_menu_command ()
