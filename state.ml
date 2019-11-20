type color = Red | Blue
type position = int * int
type status = color option
type board = (position * status) list
type num_wins = int * int

type t = {
  board : board;
  turn : color;
  wins : num_wins
}

let board t =
  t.board

let turn t =
  t.turn

let wins t =
  t.wins

let empty = 
  []

let red_wins t =
  match t.wins with
  | (r, b) -> r

let blue_wins t =
  match t.wins with
  | (r, b) -> b

let rec empty_board b r c = 
  if c > 7 && r < 6 then
    empty_board b (r+1) 1
  else if c < 8 && r < 7 then
    empty_board (((c,r), None) :: b) r (c+1)
  else 
    b

let init_state = {board = (empty_board empty 1 1); turn = Blue; wins = (0,0)}

(** [bot] is the bottom row of a board *)
let bot = "1 | 2 | 3 | 4 | 5 | 6 | 7 |"

(** [line] is the line in between rows of a board*)
let line = 
  print_string "\n";
  "-----------------------------"

(** [get_team s] is the team or empty of [s] *)
let get_team s =
  match s with
  | Some Red -> ANSITerminal.(print_string [red] "O")
  | Some Blue -> ANSITerminal.(print_string [cyan] "O")
  | None -> print_string " "

(** [print_row b temp r c] is the unit that prints the piece at [r] and [c]*)
let rec print_row b temp r c =
  match temp with
  | [] -> 
    if c > 7 then begin print_string "\n"; print_string line; print_string "\n"; 
      if r < 7 then print_string "| "; end 
  | ((x,y), s) :: t -> 
    if x = c && y = r then 
      (get_team s; (print_string " | "); print_row b b r (c + 1);) 
    else print_row b t r c

let rec display b r = 
  if r = 1 then begin print_string "\n"; print_string "| "; end;
  if r < 7 then begin display b (r + 1); print_row b b r 1; end;
  if r = 1 then print_string bot

(** [horiz (x, y)] is a list of the four coordinates to the right horizontally 
    starting from (x, y). *)
let horiz (x, y) = 
  [(x, y); (x+1, y); (x+2, y); (x+3, y)] 

(** [vert (x, y)] is a list of coordinates containing (x, y) and the three 
    coordinates above (x, y). *)
let vert (x, y) = 
  [(x, y); (x, y+1); (x, y+2); (x, y+3)]

(** [right_diag (x, y)] is a list of the four coordinates in the positive 
    diagonal direction starting from (x, y). *)
let right_diag (x, y) = 
  [(x, y); (x+1, y+1); (x+2, y+2); (x+3, y+3)]

(** [left_diag (x, y)] is a list of the four coordinates in the negative 
    diagonal direction starting from (x, y). *)
let left_diag (x, y) = 
  [(x, y); (x-1, y+1); (x-2, y+2); (x-3, y+3)]

(** [pos_by_color b clr] is a list of the positions that have a piece with color 
    [clr] in board [b]. *)
let rec pos_by_color b clr = 
  match b with
  | [] -> []
  | (pos, Some c) :: t -> if c = clr then pos :: pos_by_color t clr 
    else pos_by_color t clr
  | (pos, None) :: t -> pos_by_color t clr

(** [pos_status b (x,y)] is the [Some c] if c is the color of the piece on the 
    board [b] at position (x,y), and [None] if there is no piece at (x,y). *)
let rec pos_status b (x,y) =
  match b with
  | [] -> None
  | ((a, b), Some c) :: t -> if a = x && b = y then Some c 
    else pos_status t (x,y)
  | ((a, b), None) :: t -> if a = x && b = y then None 
    else pos_status t (x, y)

(** [four_in_a_row lst b clr] is true if the pieces in the coordinate positions 
    in [lst] have the same color [clr]. *)
let rec four_in_a_row lst b clr =
  match lst with
  | [] -> true
  | pos :: t -> begin 
      match pos_status b pos with
      | None -> false
      | Some c -> if c = clr then four_in_a_row t b clr else false  
    end

(** [check_right_diag b clr lst] checks if any right diagonal in board [b] has
    four pieces of color [clr] in a row. *)
let rec check_right_diag b clr lst =
  match lst with
  | [] -> false
  | (x, y) :: t -> if (four_in_a_row (right_diag (x, y)) b clr) then true 
    else check_right_diag b clr t 

(** [check_left_diag b clr lst] checks if any left diagonal in board [b] has
    four pieces of color [clr] in a row. *)
let rec check_left_diag b clr lst = 
  match lst with
  | [] -> false
  | (x, y) :: t -> if (four_in_a_row (left_diag (x, y)) b clr) then true 
    else check_left_diag b clr t 

(** [check_horiz b clr lst] checks if any horizontal sequence in board [b] has 
    four pieces of color [clr] in a row. *)
let rec check_horiz b clr lst = 
  match lst with
  | [] -> false
  | (x, y) :: t -> if (four_in_a_row (horiz (x, y)) b clr) then true 
    else check_horiz b clr t 

(** [check_vert b clr lst] checks if any vertical sequence in board [b] has four 
    pieces of color [clr] in a row. *)
let rec check_vert b clr lst = 
  match lst with
  | [] -> false
  | (x, y) :: t -> if (four_in_a_row (vert (x, y)) b clr) then true 
    else check_vert b clr t 

let check_win b clr =
  let positions = pos_by_color b clr in
  check_right_diag b clr positions ||
  check_left_diag b clr positions ||
  check_horiz b clr positions ||
  check_vert b clr positions

let winning_player t = 
  if (check_win t.board Red) then Some Red
  else if (check_win t.board Blue) then Some Blue
  else None 

let update_wins t =
  match winning_player t, t.wins with
  | Some Red, (red, blue) -> 
    {board = (empty_board empty 1 1); turn = Blue; wins = (red + 1, blue)} 
  | Some Blue, (red, blue) -> 
    {board = (empty_board empty 1 1); turn = Blue; wins = (red, blue + 1)} 
  | None, _ -> t

let update_wins_tuple t wins =
  match winning_player t, wins with
  | Some Red, (red, blue) -> (red+1, blue)
  | Some Blue, (red, blue) -> (red, blue+1)
  | None, _ -> wins

let other_color = function
  | Red -> Blue
  | Blue -> Red

let color_to_string = function
  | Red -> "Red"
  | Blue -> "Blue"

(**[pieces_in_col c b] is a list of all the rows in column [c] on board [b] that
   contain pieces. *)
let rec pieces_in_col c b = 
  match b with
  | [] -> []
  | ((col, row), Some clr) :: t -> 
    if col = c then row :: pieces_in_col c t
    else pieces_in_col c t
  | h :: t -> pieces_in_col c t

(**[drop_height c b] is the height at which a piece would be dropped if it were 
   placed in column [c] on board [b]. If [c] is empty, [drop_height c b] is 1.*)
let drop_height c b =
  match List.rev(List.sort compare (pieces_in_col c b)) with
  | [] -> 1
  | h :: t -> h + 1

(**[update x y clr b] is [b] but with the value for the [x,y] key replaced with 
   Some [clr]. *)
let rec update x y clr = function
  | [] -> []
  | ((x', y'), _) as pair :: t -> 
    if x' = x && y' = y then ((x, y), Some clr) :: t else
      pair :: update x y clr t

let rec anim t c low high =
  if low <= high then 
    begin 
      display (update c high t.turn t.board) 1;
      Unix.sleepf 0.5;
      print_newline ();
      anim t c low (high-1);  end
  else 
    ()

let move t c = 
  let height = drop_height c t.board in
  let old_wins = t.wins in
  if height < 7 then
    {board = update c (drop_height c t.board) t.turn t.board;
     turn = other_color t.turn;
     wins = update_wins_tuple t old_wins}
  else t

(**[possible_moves_aux b c] is a list of the locations of the possible moves for
   the current turn of state [t]. *)
let possible_moves t =
  let rec possible_moves_aux b c = 
    if c <= 7 then
      if (drop_height c b ) < 7 then
        (c, drop_height c b) :: possible_moves_aux b (c+1)
      else possible_moves_aux b (c+1)
    else []
  in possible_moves_aux t.board 1

let state_w_other_color t = 
  let board = t.board in
  let turn = t.turn in
  let wins = t.wins in
  {board = board; turn = other_color turn; wins = wins}

let block_four t c =
  let state_if_red_went = move (state_w_other_color t) c in
  check_win state_if_red_went.board Blue

let moves_that_block t =
  let rec moves_that_block_aux t ret = function
    | [] -> ret
    | (x, y) :: tl -> 
      if block_four t x then moves_that_block_aux t ((x, y) :: ret) tl
      else moves_that_block_aux t ret tl
  in moves_that_block_aux t [] (possible_moves t)

let will_win t c clr =
  check_win (move t c).board clr

let moves_that_win t =
  let rec moves_that_win_aux t ret = function
    | [] -> ret
    | (x, y) :: tl -> 
      if will_win t x (t.turn) then moves_that_win_aux t ((x, y) :: ret) tl
      else moves_that_win_aux t ret tl
  in moves_that_win_aux t [] (possible_moves t)

let will_cause_four t c =
  match moves_that_win (move t c) with
  | [] -> false
  | _ -> true

let rec cpu_move_l_to_r t = function
  | (x, y) :: [] -> x
  | (x, y) :: tl -> if will_cause_four t x then cpu_move_l_to_r t tl else x
  | _ -> failwith "No possible moves"

let rec safe_moves t = function
  | [] -> []
  | (x, y) :: tl -> 
    if will_cause_four t x then safe_moves t tl 
    else (x, y) :: safe_moves t tl

(** returns true if the board is full*)
let rec check_full b =
  match b with
  | [] -> true
  | (_, None):: t -> false
  | _:: t -> check_full t

let rec cpu_choose_move t i = function
  | [] -> cpu_move_l_to_r t (possible_moves t)
  | (x, y) :: tl -> if x = i then x else cpu_choose_move t i tl

let cpu_move t =
  match moves_that_win t with
  | (x, y) :: tl -> x
  | [] -> 
    match moves_that_block t with 
    | (x, y) :: tl -> x
    | [] -> 
      let movs = safe_moves t (possible_moves t) in 
      cpu_choose_move t (Random.int (List.length movs)) movs

let rec sim_game t i moves =
  if check_win (board t) Red then
    1
  else if check_win (board t) Blue then
    0
  else if check_full (board t) then
    0
  else if (drop_height i (board t) = 7) then
    0
  else if (moves = 0) then 
    0
  else  
    let one = sim_game (move t i) 1 (moves-1) in
    let two = sim_game (move t i) 2 (moves -1) in
    let thr = sim_game (move t i) 3 (moves - 1) in
    let four = sim_game (move t i) 4 (moves - 1) in
    let five = sim_game (move t i) 5 (moves - 1) in
    let six = sim_game (move t i) 6 (moves - 1) in
    let svn = sim_game (move t i) 7 (moves - 1) in  
    one + two + thr  + four + five + six + svn  

let rec value_of_cols t c lst =
  match sim_game (move t c) 1 4  with
  | 0 -> value_of_cols t (c+1) lst
  | _ -> (c, sim_game (move t c) 1 4)::(value_of_cols t (c+1) lst)

let rec find_max lst (c, max) =
  match lst with
  | [] ->  c
  | (k, v) :: t -> begin if v > max then find_max t (c, v) 
      else find_max t (k, v) end





