
type command = 
  | Go of int
  | Quit
  | One
  | Two
  | Three
  | Help
  | AgainYes
  | AgainNo
  | Stats
  | MainMenu

exception Invalid

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
  | "yes" :: [] -> AgainYes
  | "no" :: [] -> AgainNo
  | "stats" :: [] -> Stats
  | "menu" :: [] -> MainMenu
  | _ -> raise Invalid

let parse_menu str =
  match words (String.split_on_char ' ' (String.lowercase_ascii str)) with
  | "1" :: [] -> One
  | "2" :: [] -> Two
  | "3" :: [] -> Three
  | "quit" :: [] -> Quit
  | "stats" :: [] -> Stats
  | _ -> raise Invalid

let greater_wins t =
  if (State.red_wins t > State.blue_wins t) then Some "red" 
  else if (State.red_wins t < State.blue_wins t) then Some "blue"
  else None

let red_blue_stats r b = 
  let total = float_of_int (r + b) in
  let red = float_of_int r in
  let blue = float_of_int b in
  let red_stats = (red /. total *. 100.0) |> int_of_float |> string_of_int in
  let blue_stats = (blue /. total *. 100.0) |> int_of_float |> string_of_int in
  ANSITerminal.(print_string [red] ("\nRed stats:" ^ red_stats ^ "%"));
  ANSITerminal.(print_string [cyan] ("\nBlue stats:" ^ blue_stats ^ "%"))

let stats_messages () st =
  let r = State.red_wins st in
  let b = State.blue_wins st in 
  ANSITerminal.(print_string [magenta; Underlined; Bold] "   STATS   ");
  ANSITerminal.(print_string [red; Underlined] "\nRed:"); 
  print_string (" " ^ (r |> string_of_int));
  ANSITerminal.(print_string [cyan; Underlined] "\nBlue:"); 
  print_string (" " ^ (b |> string_of_int));
  red_blue_stats r b;
  match greater_wins st with
  | Some clr -> 
    ANSITerminal.(print_string [yellow; Bold] ("\n" ^ clr ^ " is winning!")); 
    print_endline ""
  | None -> 
    ANSITerminal.(print_string [yellow; Bold] "\nIt's a tie!");
    print_endline ""

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

let rec play_again () st one_two =
  print_endline "Would you like to play again?";
  ANSITerminal.(print_string [yellow] "\n   yes | no | menu | stats"); 
  print_endline "";
  try match parse (read_line ()), one_two with
    | AgainYes, i -> if i = 2 then two_play st true () else one_play st true ()
    | AgainNo, i -> exit 0
    | Quit, i -> exit 0
    | Stats, i -> stats_messages () st; print_endline ""; play_again () st i
    | MainMenu, i -> menu (); execute_menu_command () st
    | _, i -> print_endline 
                "Invalid command! Hint: type 'yes', 'no', or 'menu' for the main menu."; play_again () st i
  with
  | Invalid -> 
    print_endline "Invalid command! Hint: type 'yes', 'no', or 'menu' for the main menu."; play_again () st one_two

and two_play st d () = 
  let turn = State.turn st in
  let board = State.board st in
  let last_clr = State.other_color turn in
  if d then State.display board 1;
  if State.check_win board last_clr then 
    (ANSITerminal.(print_string [Blink] 
                     ("\n" ^ State.color_to_string last_clr ^ " wins!\n")); 
     play_again () (State.update_wins st) 2)
  else if (State.check_full board) then
    (ANSITerminal.(print_string [Blink] ("\nIt's a tie!\n")); play_again () st 2)
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
      | Stats -> stats_messages () st; 
        two_play st true ()
      | _ -> exit 0
    with 
    | Invalid -> 
      print_endline "Invalid move! Hint: type 'go' and a column number";
      two_play st false ()
  end

and one_play st d () = 
  let turn = State.turn st in
  let board = State.board st in
  let last_clr = State.other_color turn in
  if d then State.display board 1;
  let person_string = (if turn = State.Red then "Computer" else "You") in
  if State.check_win board last_clr then 
    if last_clr = State.Red then 
      (ANSITerminal.(print_string [Blink] ("\nComputer wins!\n")); play_again () (State.update_wins st) 1)
    else (ANSITerminal.(print_string [Blink] ("\nYou win!\n")); play_again () (State.update_wins st) 1)
  else if (State.check_full board) then
    (ANSITerminal.(print_string [Blink] ("\nIt's a tie!\n")); play_again () st 1)
  else begin
    if d then print_endline 
        ("\n" ^ State.color_to_string turn ^ "'s turn (" ^ person_string ^")");
    match turn with 
    | State.Red -> 
      Unix.sleepf 1.0;
      (*print_int (State.sim_game st 1 4);*)
      one_play (State.move st (State.cpu_move st)) true ();
    | State.Blue -> 
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
        | Stats -> stats_messages () st;
          one_play st true ()
        | _ -> exit 0
      with 
      | Invalid -> 
        print_endline "Invalid move! Hint: type 'go' and a column number";
        one_play st false ()
  end

and execute_menu_command () st =
  print_string "\n> ";
  try match parse_menu (read_line()) with
    | One -> 
      ANSITerminal.(print_string [red] "Starting One Player Mode");
      ANSITerminal.(print_string [cyan] "\nType 'help' for help at any time");
      print_endline " "; 
      Random.self_init ();
      one_play State.init_state true () 
    | Two -> 
      ANSITerminal.(print_string [red] "Starting Two Player Mode");
      ANSITerminal.(print_string [cyan] "\nType 'help' for help at any time");
      print_endline " ";
      two_play State.init_state true () 
    | Three -> 
      instructions_message ();
      execute_menu_command () st
    | Stats -> stats_messages () st; menu (); execute_menu_command () st
    | _ -> exit 0
  with
  | Invalid -> 
    print_string "Invalid. Please enter "; 
    ANSITerminal.(print_string [cyan] "1");
    print_string " for One Player Mode or ";
    ANSITerminal.(print_string [cyan] "2");
    print_endline " for Two Player Mode";
    execute_menu_command () st
