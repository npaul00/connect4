type command = 
  | Go of int
  | Quit
  | One
  | Two
  | Three
  | Four
  | Help
  | AgainYes
  | AgainNo
  | Stats
  | MainMenu
  | Easy
  | Medium
  | Hard
  | Back
  | Animation
  | Settings
  | Night

exception Invalid

(**Raised when a user no longer wants to quit or return to the menu. *)
exception Cancel

(** [menu ()] is the display of options for the start menu. *)
let menu () =
  ANSITerminal.(print_string [red; Underlined] "   Connect Four   ");
  ANSITerminal.(print_endline "\n\nSelect a Game Mode:");
  ANSITerminal.(print_string [cyan] " (1) ");
  ANSITerminal.(print_endline "One Player");
  ANSITerminal.(print_string [cyan] " (2) ");
  ANSITerminal.(print_endline "Two Player");
  ANSITerminal.(print_string [cyan] " (3) ");
  ANSITerminal.(print_endline "Instructions");
  ANSITerminal.(print_string [cyan] " (4) ");
  ANSITerminal.(print_endline "Settings")

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
  | "easy" :: [] -> Easy
  | "medium" :: [] -> Medium
  | "hard" :: [] -> Hard
  | "back" :: [] -> Back
  | "animation" :: [] -> Animation
  | "settings" :: [] -> Settings
  | "nightmode" :: [] | "night" :: [] | "night" :: "mode" :: [] -> Night 
  | _ -> raise Invalid

let parse_menu str =
  match words (String.split_on_char ' ' (String.lowercase_ascii str)) with
  | "1" :: [] -> One
  | "2" :: [] -> Two
  | "3" :: [] -> Three
  | "4" :: [] -> Four
  | "quit" :: [] -> Quit
  | _ -> raise Invalid

(** [parse_are_you_sure str] parses the player's input into a relevant [command] 
    for interacting with the Are you sure menu.
    Requires: [str] contains only (A-Z, a-z, 0-9), and space characters.
    Raises: [Invalid] if the input doesn't match [AgainYes] or [AgainNo]. *)
let parse_are_you_sure str = 
  match words (String.split_on_char ' ' (String.lowercase_ascii str)) with
  | "yes" :: [] -> AgainYes
  | "no" :: [] -> AgainNo
  | _ -> raise Invalid

(** [greater wins t] is who has the most wins in state [t]*)
let greater_wins t =
  if State.red_wins t > State.blue_wins t then Some "Red" 
  else if State.red_wins t < State.blue_wins t then Some "Blue"
  else None

(** [red_blue_stats r b] prints the stats between team [r] and team [b]*)
let red_blue_stats r b = 
  let total = float_of_int (r + b) in
  let red = float_of_int r in
  let blue = float_of_int b in
  let red_stats = (red /. total *. 100.0) |> int_of_float |> string_of_int in
  let blue_stats = (blue /. total *. 100.0) |> int_of_float |> string_of_int in
  print_endline "";
  print_endline "";
  ANSITerminal.(print_string [yellow; Underlined; Bold] "Win Percentage");
  ANSITerminal.(print_string [red] "\nRed: ");
  print_string (red_stats ^ "%");
  ANSITerminal.(print_string [cyan] "\nBlue: ");
  print_string (blue_stats ^ "%");
  print_endline ""

(** [stats_messages () st] prints all stats at [st]*)
let stats_messages () st =
  let r = State.red_wins st in
  let b = State.blue_wins st in 
  let t = State.num_ties st in
  ANSITerminal.(print_string [magenta; Underlined; Bold] "   STATS   ");
  print_endline "";
  ANSITerminal.(print_string [yellow; Underlined; Bold] "Score"); 
  print_endline "";
  ANSITerminal.(print_string [red] "Red:"); 
  print_endline (" " ^ string_of_int r);
  ANSITerminal.(print_string [cyan] "Blue:"); 
  print_endline (" " ^ string_of_int b);
  ANSITerminal.(print_string [yellow] "Ties:"); 
  print_string (" " ^ string_of_int t);
  red_blue_stats r b;
  print_endline "";
  match greater_wins st with
  | Some clr -> 
    ANSITerminal.(print_string [yellow; Bold] (clr ^ " is winning!")); 
    print_endline ""
  | None -> 
    ANSITerminal.(print_string [yellow; Bold] "It's a tie!");
    print_endline ""

(** [help_message ()] is the help message for during game play. *)
let help_message () = 
  ANSITerminal.(print_string [yellow; Underlined] "Instructions: "); 
  ANSITerminal.(print_string [yellow] "\n - Type 'go' followed by a column number to drop a piece of your color in that column.");
  ANSITerminal.(print_string [yellow] "\n   Once a column is filled, you can no longer place pieces there.");
  ANSITerminal.(print_string [yellow] "\n - Game continues until one player gets four of their colored pieces in a row,"); 
  ANSITerminal.(print_string [yellow] "\n   either horizontally, vertically, or diagonally.");
  ANSITerminal.(print_string [yellow] "\n - Type 'quit' at any time to exit, 'menu' to return to the menu, 'settings' to adjust the");
  ANSITerminal.(print_string [yellow] "\n   settings, 'stats' to view the score, or 'help' to bring up these instructions again.");
  print_endline ""

(** [instructions_message ()] is the instructions message for during the menu. *)
let instructions_message () =
  ANSITerminal.(print_string [yellow; Underlined] "   Instructions    ");
  ANSITerminal.(print_string [yellow] "\n Object of the Game: ");
  ANSITerminal.(print_string [yellow] "Be the first player to get four of your pieces in a row.");
  ANSITerminal.(print_string [yellow] "\n    This can be done horizontally, vertically, or diagonally.");
  ANSITerminal.(print_string [yellow] "\n One Player Mode: ");
  ANSITerminal.(print_string [yellow] "Play against the computer at easy, medium, or hard difficulty levels.");
  ANSITerminal.(print_string [yellow] "\n    Enter '1' to go to one player mode.");
  ANSITerminal.(print_string [yellow] "\n Two Player Mode: ");
  ANSITerminal.(print_string [yellow] "Two players take turns dropping one piece at a time.");
  ANSITerminal.(print_string [yellow] "\n    Enter '2' to go to two player mode.");
  print_endline " "

(** [starting_one_msg () d] is the message when one player mode is started with
    difficulty [d]. *)
let starting_one_msg () d = 
  let diff_str = if d = 1 then "Easy" else if d = 2 then "Medium" else "Hard" in 
  ANSITerminal.(print_string [red] ("Starting One Player Mode: " ^ diff_str));
  ANSITerminal.(print_string [cyan] "\nType 'help' for help or 'settings' to adjust the settings at any time");
  print_endline " "

(** [difficulty_msg ()] is the message for choosing a difficulty. *)
let difficulty_msg () =
  ANSITerminal.(print_string [yellow; Bold] "  CHOOSE DIFFICULTY  ");
  print_endline "";
  ANSITerminal.(print_string [yellow; Background Blue] "\n        Easy");
  print_endline "";
  ANSITerminal.(print_string [yellow; Background Blue] "\n       Medium");
  print_endline "";
  ANSITerminal.(print_string [yellow; Background Blue] "\n        Hard");
  print_endline "";
  print_string "\n> "

(** [play_again () st one_two mov dis] is the menu when a game ends that asks 
    the user if they want to play again, quit, return to the menu, or see stats.
    [one_two] is 1 for one player easy mode, 2 for two player, 3 for medium 
    one player, 4 for hard one player*)
let rec play_again () st one_two mov dis =
  print_endline "Would you like to play again?";
  ANSITerminal.(print_string [yellow] "\n   yes | no | menu | stats"); 
  print_endline "";
  print_string "> ";
  try match parse (read_line ()), one_two with
    | AgainYes, i -> play_again_yes i st mov dis ()
    | AgainNo, i -> play_again_no ()
    | Quit, i -> play_again_qm 0 i st mov dis ()
    | Stats, i -> play_again_stats i st mov dis ()
    | MainMenu, i -> play_again_qm 1 i st mov dis ()
    | _, i -> play_again_invalid i st mov dis ()
  with
  | Invalid -> play_again_invalid one_two st mov dis ()

(** [play_again_yes i st mov dis ()] is the result of [play_again] when the 
    user input parses to Yes. It starts a new game. *)
and play_again_yes i st mov dis () =
  if i = 1 then cpu_play st true 0 () 1 mov dis
  else if i = 3 then cpu_play st true 0 () 3 mov dis
  else if i = 4 then cpu_play st true 0 () 4 mov dis
  else two_play st true 0 () mov dis

(** [play_again_no ()] is the result of [play_again] when the user input parses
    to No. It exits the game engine. *)
and play_again_no () = 
  ANSITerminal.(print_string [magenta; Bold] "Thanks for playing!");
  print_endline "";
  exit 0

(** [play_again_stats i st mov dis ()] is the result of [play_again] when the 
    user input parses to Stats. It displays the game statistics. *)
and play_again_stats i st mov dis () = 
  stats_messages () st; 
  print_endline ""; 
  play_again () st i mov dis

(** [play_again_invalid i st mov dis ()] is the result of [play_again] when the 
    user input parses to Invalid. It asks the user for a new input. *)
and play_again_invalid i st mov dis () =
  print_endline "Invalid command! Hint: type 'yes', 'no', 'menu', or 'stats'."; 
  play_again () st i mov dis

(** [play_again_qm qm i st mov dis ()] is the result of [play_again] when the 
    user input parses to Quit or Menu. It prompts the Are you sure message for 
    quit if [qm]=0 or menu if [qm]=1. *)
and play_again_qm qm i st mov dis () = 
  if qm = 0 then
    ANSITerminal.(print_string [red] 
                    "Are you sure you want to quit? All data will be lost.")
  else ANSITerminal.(print_string [red] 
                       "Are you sure you want to exit to the main menu? All stats will be reset.");
  print_endline "";
  try are_you_sure () mov dis qm with
  | Cancel -> play_again () st i mov dis

(** [are_you_sure () mov dis qm] asks the user if they are sure they want to
    either quit the game if [qm]=0 or return to the menu if [qm]=1. If the 
    answer is no, it returns the user to where they were. *)
and are_you_sure () mov dis qm =
  print_string "> ";
  try match parse_are_you_sure (read_line ()) with
    | AgainYes ->
      if qm = 1 then execute_menu_command () mov dis
      else begin
        ANSITerminal.(print_string [magenta; Bold] "Thanks for playing!"); 
        print_endline ""; 
        exit 0
      end
    | AgainNo -> raise Cancel
    | _ -> begin 
        print_endline "Invalid command! Hint: type 'yes' or 'no'";
        are_you_sure () mov dis qm
      end
  with
  | Invalid -> begin 
      print_endline "Invalid command! Hint: type 'yes' or 'no'";
      are_you_sure () mov dis qm
    end

(** [two_play st d last () mov dis ] is the start of a two player game in state 
    [st] and displays the board if [d] is true. [last] is the column of the 
    most recent piece played, and is 0 if no pieces have been played.*)
and two_play st d last () mov dis = 
  let board = State.board st in
  let last_clr = st |> State.turn |> State.other_color in
  if d then dis board 1;
  if State.check_win board last_clr then begin
    print_endline "";
    let win_dis = select_win_dis dis in 
    win_dis last_clr board 1;
    colored_win_msg last_clr st mov dis ()
  end
  else if State.check_full board then begin
    ANSITerminal.(print_string [Blink] ("\nIt's a tie!\n")); 
    play_again () (State.update_wins st) 2 mov dis
  end
  else begin
    if d then display_last_and_turn st last last_clr ();
    print_string "> ";
    exec_two_play_cmd st last mov dis ()
  end

(** [select_win_dis dis] is the display method for the win message based on the
    night mode setting [dis]. *)
and select_win_dis dis = 
  if dis == State.display then State.display_win
  else State.display_win_d

(** [colored_win_msg clr st mov dis ()] displays a colored winning message for 
    color [clr] and then prompts the play again menu for [st] with updated wins,
    with settings [mov] and [dis].*)
and colored_win_msg clr st mov dis () = 
  if State.color_to_string clr = "Red" then
    begin
      ANSITerminal.(print_string [red; Blink] ("\nRed wins!\n"));
      print_endline "";
      play_again () (State.update_wins st) 2 mov dis
    end
  else 
    begin
      ANSITerminal.(print_string [blue; Blink; Bold] ("\nBlue wins!\n"));
      print_endline "";
      play_again () (State.update_wins st) 2 mov dis
    end

(** [display_last_and_turn st last last_clr ()] displays the last move if at 
    least one move has been taken, and then displays whose turn it is. *)
and display_last_and_turn st last last_clr () = 
  if last <> 0 then print_string 
      ( "\nLast move: " ^ (State.color_to_string last_clr) ^ " in column " 
        ^ string_of_int last);
  let turn = State.turn st in
  print_endline ("\n" ^ State.color_to_string turn ^ "'s turn")

(** [exec_two_play_cmd st last mov dis ()] parses the user's input during a 
    two player game to a command and carries out the correct action. *)
and exec_two_play_cmd st last mov dis () = 
  try match parse (read_line ()) with
    | Go col -> two_play_go col st last mov dis ()
    | Help -> 
      help_message ();
      two_play st true last () mov dis
    | Stats -> 
      stats_messages () st; 
      two_play st true last () mov dis
    | MainMenu -> two_play_qm 1 st last mov dis ()
    | Settings -> settings_menu () mov dis (two_play st true last ())
    | Quit -> two_play_qm 0 st last mov dis ()
    | _ -> two_play_invalid st last mov dis ()
  with 
  | Invalid -> two_play_invalid st last mov dis ()

(** [select_anim mov dis] selects the move method based on the animation
    setting [mov] and the night mode setting [dis]. *)
and select_anim mov dis = 
  if mov == State.move then mov
  else if dis == State.display then State.move_anim
  else State.move_anim_d

(** [two_play_go col st last mov dis ()] is the result of [exec_two_play_cmd] 
    when the user input parses to Go [col]. It places a piece in [col] if it's 
    not full. Else, it asks the user to try again. *)
and two_play_go col st last mov dis () = 
  let move = select_anim mov dis in
  let new_state = move st col in
  if new_state = st then begin
    print_endline "That column is full, try another!";
    two_play st false last () mov dis
  end
  else begin
    two_play new_state true col () mov dis
  end

(** [two_play_qm qm st last mov dis ()] is the result of [exec_two_play_cmd] 
    when the user input parses to Quit or Menu. It prompts the Are you sure 
    message for quit if [qm]=0 or menu if [qm]=1. *)
and two_play_qm qm st last mov dis () = 
  if qm = 0 then 
    ANSITerminal.(print_string [red] 
                    "Are you sure you want to quit? All data will be lost.") 
  else ANSITerminal.(print_string [red] 
                       "Are you sure you want to exit to the main menu? All stats will be reset.");
  print_endline "";
  try are_you_sure () mov dis qm
  with | Cancel -> two_play st true last () mov dis

(** [two_play_invalid st last mov dis ()] is the result of [exec_two_play_cmd] 
    when the user input parses to Invalid. It asks the user for a new input. *)
and two_play_invalid st last mov dis () = 
  print_endline "Invalid move! Hint: type 'go' and a column number"; 
  two_play st false last () mov dis

(** [one_play st d () mov dis] is the start of one player mode and asks the 
    user for a difficulty*)
and one_play st d () mov dis = 
  difficulty_msg ();
  try match parse (read_line ()) with
    | Easy -> 
      starting_one_msg () 1;
      cpu_play st d 0 () 1 mov dis
    | Medium -> 
      starting_one_msg () 2;
      cpu_play st d 0 () 3 mov dis
    | Hard -> 
      starting_one_msg () 3;
      cpu_play st d 0 () 4 mov dis
    | Quit -> one_play_quit st mov dis ()
    | _ -> one_play_invalid st mov dis ()
  with
  | Invalid -> one_play_invalid st mov dis ()

(** [one_play_quit st mov dis ()] is the result of [one_play] when the user 
    input parses to Quit. It prompts the Are you sure message.*)
and one_play_quit st mov dis () = 
  ANSITerminal.(print_string [red] 
                  "Are you sure you want to quit? All data will be lost.");
  print_endline "";
  try are_you_sure () mov dis 0
  with | Cancel -> one_play st true () mov dis

(** [one_play_invalid st mov dis ()] is the result of [one_play] when the user 
    input parses to Invalid. It asks the user for a new input.*)
and one_play_invalid st mov dis () = 
  print_endline "Invalid command! Hint: type 'easy', 'medium', or 'hard'."; 
  print_string "> ";
  one_play st true () mov dis

(** [cpu_play st d last () i mov dis] is one player mode at difficulty [i] and 
    prints if [d] is true. [last] is the column of the most recent piece played,
    and is 0 if no pieces have been played.*)
and cpu_play st d last () i mov dis = 
  let op = if i = 1 then State.cpu_move_easy
    else if i = 4 then State.cpu_move_hard
    else State.cpu_move_med in
  let turn = State.turn st in
  let board = State.board st in
  let last_clr = State.other_color turn in
  if d then dis board 1;
  if State.check_win board last_clr then 
    cpu_play_win last_clr st i mov dis ()
  else if State.check_full board then
    (ANSITerminal.(print_string [Blink] ("\nIt's a tie!\n")); 
     play_again () (State.update_wins st) i mov dis)
  else begin
    if d then cpu_play_last_and_turn last turn last_clr ();
    match turn with 
    | State.Red -> 
      print_endline "";
      let (move_col, vis) = op st in
      let move = select_anim mov dis in
      cpu_play (State.update_vis (move st move_col) vis) true (move_col) () i mov dis
    | State.Blue -> cpu_play_blue_move st d last () i mov dis
  end

(** [cpu_play_win last_clr st i mov dis ()] displays the winning message for 
    two player mode. *)
and cpu_play_win last_clr st i mov dis () = 
  print_endline "";
  if last_clr = State.Red then begin
    if dis == State.display then
      (State.display_win Red (State.board st) 1;)
    else
      State.display_win_d Red (State.board st) 1;
    ANSITerminal.(print_string [red; Blink] ("\nComputer wins!\n"));
    print_endline ""; 
    play_again () (State.update_wins st) i mov dis
  end
  else begin
    if dis == State.display then
      (State.display_win Blue (State.board st) 1;)
    else
      State.display_win_d Blue (State.board st) 1;
    ANSITerminal.(print_string [blue; Bold; Blink] ("\nYou win!\n"));
    print_endline "";
    play_again () (State.update_wins st) i mov dis
  end

(** [cpu_play_last_and_turn last turn last_clr ()] displays the last move if at 
    least one move has been taken, and then displays whose turn it is. *)
and cpu_play_last_and_turn last turn last_clr () =
  if last <> 0 then begin
    let last_person_string = 
      if turn = State.Blue then "Computer" else "You" in
    print_string ("\nLast move: " ^ (State.color_to_string last_clr) ^ 
                  " (" ^ last_person_string ^ ") in column " ^ 
                  string_of_int last)
  end;
  let person_string = 
    if turn = State.Red then "Computer" else "You" in
  print_string 
    ("\n" ^ State.color_to_string turn ^ "'s turn (" ^ person_string ^")")

(** [cpu_play_blue_move st d last () i mov dis] parses the user's input during a 
    two player game to a command and carries out the correct action. *)
and cpu_play_blue_move st d last () i mov dis = 
  print_string "\n> ";
  try match parse (read_line()) with
    | Go col -> cpu_play_go i col st last mov dis ()
    | Help -> 
      help_message ();
      cpu_play st true last () i mov dis
    | Stats -> 
      stats_messages () st;
      cpu_play st true last () i mov dis
    | MainMenu -> cpu_play_qm 1 st last i mov dis ()
    | Settings -> settings_menu () mov dis (cpu_play st true last () i)
    | Quit -> cpu_play_qm 0 st last i mov dis ()
    | _ -> cpu_play_invalid st last i mov dis ()
  with 
  | Invalid -> cpu_play_invalid st last i mov dis ()

(** [cpu_play_go i col st last mov dis ()] is the result of [cpu_play_blue_move] 
    when the user input parses to Go [col]. It places a piece in [col] if it's 
    not full. Else, it asks the user to try again. *)
and cpu_play_go i col st last mov dis () = 
  let move = select_anim mov dis in
  let new_state =  move st col in
  if new_state = st then begin
    print_string "That column is full, try another!";
    cpu_play st false last () i mov dis
  end
  else cpu_play new_state true col () i mov dis

(** [cpu_play_qm qm st last i mov dis ()] is the result of [cpu_play_blue_move] 
    when the user input parses to Quit or Menu. It prompts the Are you sure 
    message for quit if [qm]=0 or menu if [qm]=1. *)
and cpu_play_qm qm st last i mov dis () = 
  if qm = 0 then ANSITerminal.(print_string [red] "Are you sure you want to quit? All data will be lost.")
  else ANSITerminal.(print_string [red] 
                       "Are you sure you want to exit to the main menu? All stats will be reset.");
  print_endline "";
  try are_you_sure () mov dis qm
  with | Cancel -> cpu_play st true last () i mov dis

(** [cpu_play_invalid st last i mov dis ()] is the result of 
    [cpu_play_blue_move] when the user input parses to Invalid. It asks the user 
    for a new input.*)
and cpu_play_invalid st last i mov dis () = 
  print_string "Invalid move! Hint: type 'go' and a column number";
  cpu_play st false last () i mov dis

(** [settings_menu () mov dis next] is the settings menu that is used to 
    toggle animation and night mode. *)
and settings_menu () mov dis next =
  ANSITerminal.(print_string [cyan] 
                  "Enter a setting to toggle or type 'back' to go back: \n");
  print_endline "";
  let next_mov = settings_next_mov mov in
  print_endline "";
  let next_dis = settings_next_dis dis in
  print_string "\n> ";
  try match parse (read_line()) with
    | Back -> next mov dis
    | Animation -> settings_menu () next_mov dis next
    | Night -> settings_menu () mov next_dis next
    | Quit -> settings_menu_quit mov dis next ()
    | _ -> print_endline "Invalid command!";
      settings_menu () mov dis next
  with
  | Invalid -> print_endline "Invalid command!";
    settings_menu () mov dis next

(** [settings_next_mov mov] is the animation setting if [mov] is changed. *)
and settings_next_mov mov = 
  if mov == State.move_anim then begin
    ANSITerminal.(print_string [green] "  Animation "); 
    State.move end
  else begin 
    ANSITerminal.(print_string [red] "  Animation "); 
    State.move_anim end

(** [settings_next_dis dis] is the night mode setting if [dis] is changed. *)
and settings_next_dis dis = 
  if dis == State.display then begin
    ANSITerminal.(print_string [green] "  Night mode "); 
    State.display_d end
  else begin 
    ANSITerminal.(print_string [red] "  Night mode ");
    State.display end

(** [settings_menu_quit mov dis next ()] is the result of [settings_menu]
    when the user input parses to Quit. It prompts the Are you sure message. *)
and settings_menu_quit mov dis next () = 
  ANSITerminal.(print_string [red] 
                  "Are you sure you want to quit? All data will be lost.");
  print_endline "";
  try are_you_sure () mov dis 0
  with | Cancel -> settings_menu () mov dis next

and execute_menu_command () mov dis =
  print_endline "";
  menu ();
  print_string "\n> ";
  try match parse_menu (read_line()) with
    | One -> 
      Random.self_init ();
      one_play State.init_state true () mov dis
    | Two -> 
      ANSITerminal.(print_string [red] "Starting Two Player Mode");
      ANSITerminal.(print_string [cyan] 
                      "\nType 'help' for help or 'settings' to adjust the settings at any time");
      print_endline " ";
      two_play State.init_state true 0 () mov dis
    | Three -> 
      instructions_message ();
      execute_menu_command () mov dis
    | Four -> settings_menu () mov dis (execute_menu_command ())
    | Quit -> exec_menu_cmd_quit mov dis ()
    | _ -> exec_menu_cmd_invalid mov dis ()
  with
  | Invalid -> exec_menu_cmd_invalid mov dis ()

(** [exec_menu_cmd_quit mov dis ()] is the result of [execute_menu_command]
    when the user input parses to Quit. It prompts the Are you sure message. *)
and exec_menu_cmd_quit mov dis () = 
  ANSITerminal.(print_string [red] "Are you sure you want to quit? All data will be lost.");
  print_endline "";
  try are_you_sure () mov dis 0 
  with | Cancel -> execute_menu_command () mov dis

(** [exec_menu_cmd_invalid mov dis ()] is the result of [execute_menu_command]
    when the user input parses to Invalid. It asks the user for a new input.*)
and exec_menu_cmd_invalid mov dis () = 
  print_string "Invalid. Please enter "; 
  ANSITerminal.(print_string [cyan] "1");
  print_string " for One Player Mode, ";
  ANSITerminal.(print_string [cyan] "2");
  print_endline " for Two Player Mode, ";
  ANSITerminal.(print_string [cyan] "3");
  print_string " to view the instructions, or ";
  ANSITerminal.(print_string [cyan] "4");
  print_string " to adjust the settings.";
  execute_menu_command () mov dis
