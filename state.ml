type color = Red | Blue
type position = int * int
type status = color option
type board = (position * status) list
type num_wins = int * int * int
type moves_list = int list
type bool_or_pos = Truth of bool | Pos of position list
type visited = (board * int) list

type t = {
  board : board;
  turn : color;
  wins : num_wins;
  moves : moves_list;
  visit : visited 
}

let board t =
  t.board

let turn t =
  t.turn

let wins t =
  t.wins

let moves t =
  t.moves

let red_wins t =
  match t.wins with
  | (r, _, _) -> r

let blue_wins t =
  match t.wins with
  | (_, b, _) -> b

let num_ties t = 
  match t.wins with
  | (_, _, ties) -> ties

let set_turn t clr =
  {board = t.board; turn = clr; wins = t.wins; moves = t.moves; visit = t.visit}

let empty = 
  []

let empty_board = 
  let rec empty_board_aux b r c = 
    if c > 7 && r < 6 then
      empty_board_aux b (r + 1) 1
    else if c < 8 && r < 7 then
      empty_board_aux (((c, r), None) :: b) r (c + 1)
    else 
      b
  in empty_board_aux empty 1 1

(** [half_board] creates a board that is half full and has no winners*)
let half_board = 
  let rec half_board_aux b r c =
    if c > 7 && r < 6 then
      half_board_aux b (r + 1) 1
    else if c < 8 && r < 7 then
      if c mod 2 = 0 && r < 3 then 
        half_board_aux (((c, r), Some Blue) :: b) r (c + 1)
      else if r < 3 then 
        half_board_aux (((c, r), Some Red) :: b) r (c + 1)
      else if r < 4 && c mod 2 = 0 then
        half_board_aux (((c, r), Some Red) :: b) r (c + 1)
      else if r < 4  then
        half_board_aux (((c, r), Some Blue) :: b) r (c + 1)
      else 
        half_board_aux (((c, r), None) :: b) r (c + 1)
    else 
      b
  in half_board_aux empty 1 1

(** [half_board_moves] is the list of moves for [half_board]. *)
let half_board_moves = 
  [1; 2; 3; 4; 5; 6; 7; 2; 3; 4; 5; 6; 7; 7; 1; 1; 2; 3; 4; 5; 6]

(** [testing] is true to start with a half board, false for an empty board *)
let testing = false

let init_state = 
  if testing then
    {board = half_board; turn = Blue; wins = (0, 0, 0); 
     moves = half_board_moves; visit = []}
  else 
    {board = empty_board; turn = Blue; wins = (0, 0, 0); moves = []; visit = []}

(** [bot] is the bottom row of a board *)
let bot = "1 | 2 | 3 | 4 | 5 | 6 | 7 | "

(** [line] is the line in between rows of a board*)
let line = 
  print_string "\n";
  "----------------------------- "

(** [get_team s] is the team or empty of [s] *)
let get_team s =
  match s with
  | Some Red -> ANSITerminal.(print_string [red] "O")
  | Some Blue -> ANSITerminal.(print_string [cyan] "O")
  | None -> print_string " "

(** [print_row b temp r c] is the unit that prints the piece at [r] and [c]. *)
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

(** [get_team_d s] is the team or empty of [s] in day mode*)
let get_team_d s =
  match s with
  | Some Red -> ANSITerminal.(print_string [red; Background White] "O")
  | Some Blue -> ANSITerminal.(print_string [cyan; Background White] "O")
  | None ->  ANSITerminal.(print_string [cyan; Background White] " ")

(** [print_row_d b temp r c] is the unit that prints the piece at [r] and [c] 
    in day mode*)
let rec print_row_d b temp r c =
  match temp with
  | [] -> 
    if c > 7 then begin 
      print_string "\n"; 
      ANSITerminal.(print_string [black; Background White] line); 
      print_string "\n"; 
      if r < 7 then ANSITerminal.(print_string [black; Background White] "| "); 
    end 
  | ((x,y), s) :: t -> 
    if x = c && y = r then begin
      (get_team_d s; 
       (ANSITerminal.(print_string [black; Background White] " | ")); 
       print_row_d b b r (c + 1);) 
    end
    else print_row_d b t r c

let rec display_d b r = 
  if r = 1 then begin 
    print_string "\n"; 
    ANSITerminal.(print_string [black; Background White] "| "); 
  end;
  if r < 7 then begin 
    display_d b (r + 1); 
    print_row_d b b r 1; 
  end;
  if r = 1 then 
    ANSITerminal.(print_string [black; Background White] bot)

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
  | (pos, Some c) :: t -> 
    if c = clr then pos :: pos_by_color t clr 
    else pos_by_color t clr
  | (pos, None) :: t -> pos_by_color t clr

(** [pos_status b (x,y)] is the [Some c] if c is the color of the piece on the 
    board [b] at position (x,y), and [None] if there is no piece at (x,y). *)
let rec pos_status b (x,y) =
  match b with
  | [] -> None
  | ((a, b), Some c) :: t -> 
    if a = x && b = y then Some c 
    else pos_status t (x,y)
  | ((a, b), None) :: t -> 
    if a = x && b = y then None 
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

(** [check_right_diag b clr lst op] checks if any right diagonal in board [b] 
    has four pieces of color [clr] in a row. *) 
let rec check_right_diag b clr lst op =
  match lst with
  | [] -> if op then Truth false else Pos []
  | (x, y) :: t -> 
    if op && four_in_a_row (right_diag (x, y)) b clr then Truth true else
    if four_in_a_row (right_diag (x, y)) b clr then Pos (right_diag (x, y))
    else check_right_diag b clr t op

(** [check_left_diag b clr lst op] checks if any left diagonal in board [b] has
    four pieces of color [clr] in a row. *)
let rec check_left_diag b clr lst op = 
  match lst with
  | [] -> if op then (Truth false) else Pos []
  | (x, y) :: t -> 
    if (op && four_in_a_row (left_diag (x, y)) b clr) then Truth true
    else if (four_in_a_row (left_diag (x, y)) b clr) then Pos (left_diag (x, y))
    else check_left_diag b clr t op

(** [check_horiz b clr lst op] checks if any horizontal sequence in board [b] 
    has four pieces of color [clr] in a row. *)
let rec check_horiz b clr lst op = 
  match lst with
  | [] -> if op then (Truth false) else Pos []
  | (x, y) :: t -> 
    if (op && four_in_a_row (horiz (x, y)) b clr) then Truth true
    else if (four_in_a_row (horiz (x, y)) b clr) then Pos (horiz (x, y))
    else check_horiz b clr t op

(** [check_vert b clr lst op] checks if any vertical sequence in board [b] has 
    four pieces of color [clr] in a row. *)
let rec check_vert b clr lst op = 
  match lst with
  | [] -> if op then (Truth false) else Pos []
  | (x, y) :: t -> 
    if (op && four_in_a_row (vert (x, y)) b clr) then Truth true
    else if (four_in_a_row (vert (x, y)) b clr) then Pos (vert (x, y))
    else check_vert b clr t op

let get_truth = function
  | Truth b -> b
  | Pos _ -> failwith "Not a truth value"

let get_pos = function
  | Pos lst -> lst
  | Truth _ -> failwith "Not a position list"

let check_win b clr =
  let positions = pos_by_color b clr in
  get_truth (check_right_diag b clr positions true) ||
  get_truth (check_left_diag b clr positions true) ||
  get_truth (check_horiz b clr positions true) ||
  get_truth (check_vert b clr positions true)

(** [get_win_pos b clr] is a list of the positions where [clr] can win in [b]*)
let get_win_pos b clr  =
  let positions = pos_by_color b clr in
  if get_truth (check_right_diag b clr positions true) then
    get_pos (check_right_diag b clr positions false) else
  if get_truth (check_left_diag b clr positions true) then
    get_pos (check_left_diag b clr positions false) else
  if get_truth (check_horiz b clr positions true) then
    get_pos (check_horiz b clr positions false) else
  if get_truth (check_vert b clr positions true) then
    get_pos (check_vert b clr positions false) else
    failwith "Not a winning board or color"

(** [get_team_win s win] is the representation of the position [s] on the 
    winning board. The piece is "X" if [s] is a winning piece, "O" if [s] is a 
    regular piece, and " " if [s] is an empty position. *)
let get_team_win s win =
  match s with
  | Some Red -> if win then ANSITerminal.(print_string [red; Blink] "X") else 
      ANSITerminal.(print_string [red] "O")
  | Some Blue -> if win then ANSITerminal.(print_string [cyan; Blink] "X") else 
      ANSITerminal.(print_string [cyan] "O")
  | None -> print_string " "

(** [print_row_win b temp r c clr] is the unit that prints the piece at [r] and 
    [c] in a winning board. *)
let rec print_row_win b temp r c clr =
  match temp with
  | [] -> 
    if c > 7 then begin print_string "\n"; print_string line; print_string "\n"; 
      if r < 7 then print_string "| "; end 
  | ((x,y), s) :: t -> 
    if x = c && y = r then
      (if (List.mem (x, y) (get_win_pos b clr)) then
         (get_team_win s true; (print_string " | "); 
          print_row_win b b r (c + 1) clr;)
       else 
         (get_team_win s false; (print_string " | "); 
          print_row_win b b r (c + 1) clr;) 
      )
    else print_row_win b t r c clr

let rec display_win clr b r = 
  if r = 1 then begin print_string "\n"; print_string "| "; end;
  if r < 7 then begin display_win clr b (r + 1); print_row_win b b r 1 clr; end;
  if r = 1 then print_string bot

(** [get_team_win_d s win] is [get_team_win] in day mode. *)
let get_team_win_d s win =
  match s with
  | Some Red -> 
    if win then ANSITerminal.(print_string [red; Blink; Background White] "X") 
    else ANSITerminal.(print_string [red; Background White] "O")
  | Some Blue -> 
    if win then ANSITerminal.(print_string [cyan; Blink; Background White] "X") 
    else ANSITerminal.(print_string [cyan; Background White] "O")
  | None -> ANSITerminal.(print_string [cyan ; Background White] " ")

(** [print_row_win_d b temp r c clr] is [print_row_win] in day mode. *)
let rec print_row_win_d b temp r c clr =
  match temp with
  | [] -> 
    if c > 7 then begin 
      print_string "\n"; 
      ANSITerminal.(print_string [black; Background White] line); 
      print_string "\n"; 
      if r < 7 then ANSITerminal.(print_string [black; Background White] "| "); 
    end 
  | ((x,y), s) :: t -> 
    if x = c && y = r then begin
      if (List.mem (x, y) (get_win_pos b clr)) then begin
        get_team_win_d s true; 
        (ANSITerminal.(print_string [black; Background White] " | ")); 
        print_row_win_d b b r (c + 1) clr;
      end
      else begin
        get_team_win_d s false; 
        (ANSITerminal.(print_string [black; Background White] " | ")); 
        print_row_win_d b b r (c + 1) clr;
      end
    end
    else print_row_win_d b t r c clr

let rec display_win_d clr b r = 
  if r = 1 then begin 
    print_string "\n"; 
    ANSITerminal.(print_string [black; Background White] "| "); 
  end;
  if r < 7 then begin 
    display_win_d clr b (r + 1); 
    print_row_win_d b b r 1 clr; 
  end;
  if r = 1 then ANSITerminal.(print_string [black; Background White] bot)

let winning_player t = 
  if check_win t.board Red then Some Red
  else if check_win t.board Blue then Some Blue
  else None 

let new_color wins = 
  match wins with
  | (r, b, t) -> if (r + b + t) mod 2 = 0 then Blue else Red 

let update_wins t =
  match winning_player t, t.wins with
  | Some Red, (red, blue, ties) -> 
    let u_wins = (red + 1, blue, ties) in
    {board = empty_board; turn = new_color u_wins; wins = u_wins; moves = []; 
     visit = t.visit} 
  | Some Blue, (red, blue, ties) -> 
    let u_wins = (red, blue + 1, ties) in
    {board = empty_board; turn = new_color u_wins; wins = u_wins; moves = []; 
     visit = t.visit} 
  | None, (red, blue, ties) -> 
    let u_wins = (red, blue, ties + 1) in
    {board = empty_board; turn = new_color u_wins; wins = u_wins; moves = []; 
     visit = t.visit} 

(** [update_wins_tuple t wins] is [t] with [wins] for the wins field*)
let update_wins_tuple t wins =
  match winning_player t, wins with
  | Some Red, (red, blue, ties) -> (red + 1, blue, ties)
  | Some Blue, (red, blue, ties) -> (red, blue + 1, ties)
  | None, (red, blue, ties) -> (red, blue, ties + 1)

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

let drop_height c b =
  let rows = pieces_in_col c b in
  match rows |> List.sort compare |> List.rev with
  | [] -> 1
  | h :: t -> h + 1

let rec update x y clr = function
  | [] -> []
  | ((x', y'), _) as pair :: t -> 
    if x' = x && y' = y then ((x, y), Some clr) :: t else
      pair :: update x y clr t

(** [anim t c low high] is an animation for a piece falling from [high] to [low]
    in column [c] at state [t] *)
let rec anim t c low high dis =
  if low <= high then 
    begin 
      dis (update c high t.turn t.board) 1;
      Unix.sleepf 0.5;
      print_newline ();
      anim t c low (high-1) dis;  
    end
  else ()

(** [update_moves_list lst c] is [lst] with [c] concatinated *)
let update_moves_list lst c = 
  List.rev (c :: (List.rev lst))

let move t c = 
  let height = drop_height c t.board in
  if height < 7 then
    {board = update c height t.turn t.board;
     turn = other_color t.turn;
     wins = t.wins;
     moves = update_moves_list t.moves c; 
     visit = t.visit}
  else t

let update_vis t vis =    
  {board = t.board; turn = t.turn; wins = t.wins; moves = t.moves; visit = vis}

let move_anim t c = 
  let height = drop_height c t.board in
  if height < 7 then
    begin 
      anim t c height 7 display;
      {board = update c height t.turn t.board;
       turn = other_color t.turn;
       wins = t.wins;
       moves = update_moves_list t.moves c;
       visit = t.visit} 
    end
  else t

let move_anim_d t c= 
  let height = drop_height c t.board in
  if height < 7 then
    begin 
      anim t c height 7 display_d;
      {board = update c height t.turn t.board;
       turn = other_color t.turn;
       wins = t.wins;
       moves = update_moves_list t.moves c; 
       visit = []} 
    end
  else t

let rec check_full b =
  match b with
  | [] -> true
  | (_, None) :: t -> false
  | _ :: t -> check_full t

(**[state_w_other_color t] is the state with the same board as [t.board] 
   but with the turn changed to the other color. *)
let state_w_other_color t = 
  {board = t.board; turn = other_color t.turn; wins = t.wins; moves = t.moves; 
   visit = t.visit}

(**[possible_moves_aux b c] is a list of the locations of the possible moves for
   the current turn of state [t]. *)
let possible_moves t =
  let rec possible_moves_aux b c = 
    if c <= 7 then
      if (drop_height c b ) < 7 then
        (c, drop_height c b) :: possible_moves_aux b (c + 1)
      else possible_moves_aux b (c + 1)
    else []
  in possible_moves_aux t.board 1

(**[block_four t c] is the boolean value of whether the current player of [t] 
   would block a potential 4 in a row of the opponent by putting a piece in 
   column [c]. *)
let block_four t c =
  let new_bd = (move t c).board in
  (not (check_win new_bd (other_color t.turn))) &&
  let state_if_other_went = move (state_w_other_color t) c in
  check_win state_if_other_went.board (other_color t.turn)

(**[moves_that_block t] is a list of the positions where [t.turn] can go to 
   block a potential 4 in a row of the opponent. *)
let moves_that_block t =
  let rec moves_that_block_aux t = function
    | [] -> []
    | (x, y) :: tl -> 
      if block_four t x then (x, y) :: moves_that_block_aux t tl
      else moves_that_block_aux t tl
  in moves_that_block_aux t (possible_moves t)

(**[will_win t c clr] is the boolean value of whether [clr] will win if the 
   current player of [t] places a piece in column [c]. *)
let will_win t c clr =
  let new_st = move t c in 
  check_win new_st.board clr

(**[moves_that_win t] is a list of positions where the current player of [t] 
   would win if they placed a piece there. *)
let moves_that_win t =
  let rec moves_that_win_aux t = function
    | [] -> []
    | (x, y) :: tl -> 
      if will_win t x t.turn then (x, y) :: moves_that_win_aux t tl
      else moves_that_win_aux t tl
  in moves_that_win_aux t (possible_moves t)

(**[will_cause_four t c] is the boolean value of whether the current player of 
   [t] placing a piece in column [c] would allow the opponent to get 4 in a row 
   by placing a piece on top of that one. *)
let will_cause_four t c =
  let new_st = move t c in
  will_win new_st c new_st.turn

(**[safe_moves t lst] is a list of positions from [lst] that wouldn't cause the 
   current player of [t] to allow the opponent to win next turn by placing a 
   piece on top of theirs. *)
let rec safe_moves t = function
  | [] -> []
  | (x, y) :: tl -> 
    if will_cause_four t x then safe_moves t tl 
    else (x, y) :: safe_moves t tl

(** [will_eliminate_potential t c] is true if a move to [c] in [t] blocks a 
    four in a row and is false otherwise*)
let will_eliminate_potential t c = 
  block_four (move t c) c

(** [safer_moves t lst] is the potential moves in [lst] that won't jeapardize a
    potential four in a row at [t] *)
let rec safer_moves t = function
  | [] -> []
  | (x, y) :: tl ->
    if will_eliminate_potential t x then safer_moves t tl
    else (x, y) :: safer_moves t tl

(** [cpu_choose_move t i lst] is the column the computer should play in, found
    by choosing the [ith] element of [lst], a list of possible positions.*)
let rec cpu_choose_move t i lst = 
  let rec cpu_choose_move_aux t' i' count' = function 
    | (x, y) :: tl -> 
      if count' = i' then begin 
        Unix.sleepf 1.0; 
        x 
      end 
      else cpu_choose_move_aux t' i' (count'+1) tl
    | [] -> failwith "No possible moves"
  in cpu_choose_move_aux t i 0 lst

(** [num_pieces_on_noard b] is the amount of pieces placed in [b] *)
let rec num_pieces_on_board = function
  | [] -> 0
  | ((x, y), Some clr) :: tl -> 1 + num_pieces_on_board tl
  | ((x, y), None) :: tl -> num_pieces_on_board tl

(** [piece_on_board b] is the first piece's column or -1 if there are no
    pieces *)
let rec piece_on_board = function
  | ((x, y), Some clr) :: tl -> x
  | ((x, y), None) :: tl -> piece_on_board tl
  | [] -> (-1)

(** [next_to loc] is a column adjacent to [loc] *)
let next_to loc =
  Unix.sleepf 1.0;
  if loc = (-1) then 4 else
  if loc < 4 then loc + 1 else
  if loc > 4 then loc - 1 else
    let rand = Random.int 2 in
    if rand = 0 then loc - 1 else loc + 1

let rec cpu_move_med t =
  if num_pieces_on_board t.board <= 1 then
    (next_to (piece_on_board t.board), t.visit) else
    match moves_that_win t with
    | (x, y) :: tl -> Unix.sleepf 1.0; (x, t.visit)
    | [] -> begin
        match moves_that_block t with 
        | (x, y) :: tl -> Unix.sleepf 1.0; (x, t.visit)
        | [] -> no_immediate t
      end

(** [no_immediate t] computes the computer's next move during medium mode 
    when there are no columns they could immediately win in or block in. *)
and no_immediate t = 
  let p_moves = possible_moves t in
  let s_moves = safe_moves t p_moves in 
  let safer = safer_moves t s_moves in
  match safer with
  | (x, y) :: tl -> 
    (cpu_choose_move t (Random.int (List.length safer)) safer, t.visit)
  | [] -> begin
      match s_moves with
      | (x, y) :: tl -> 
        (cpu_choose_move t (Random.int (List.length s_moves)) s_moves, t.visit)
      | [] -> 
        (cpu_choose_move t (Random.int (List.length p_moves)) p_moves, t.visit)
    end

let cpu_move_easy t =
  match moves_that_win t with
  | (x, y) :: tl -> Unix.sleepf 1.0; (x, t.visit)
  | [] -> begin
      match moves_that_block t with 
      | (x, y) :: tl -> Unix.sleepf 1.0; (x, t.visit)
      | [] -> 
        let p_moves = possible_moves t in
        (cpu_choose_move t (Random.int (List.length p_moves)) p_moves, t.visit)
    end

(** [count_moves b] is the amount of times a piece has been placed in [b] *)
let rec count_moves b =
  match b with 
  | [] -> 0
  | (_, Some c) :: t -> 1 + count_moves t
  | _ :: t -> count_moves t

(** [playable b c] is true if column [c] in [b] isn't full and false otherwise*)
let playable b c =
  drop_height c b != 7

(** [empty_spots b] is all the positions in [b] that are empty *)
let rec empty_spots = function
  | [] -> []
  | ((x, y), None) :: t -> (x, y) :: empty_spots t
  | _ :: t -> empty_spots t

(** [check_four (x, y) clr b] is if placing a piece at [(x, y)] will give [clr] 
    a win on [b]*)
let check_four (x, y) clr b =
  check_win (update x y clr b) clr

(** [threes clr b lst] is how many three in a rows [clr] has in [b] using
    [lst] of empty spots*)
let rec threes clr b = function
  | [] -> 0
  | (x, y) :: t -> if check_four (x, y) clr b then 1 + threes clr b t else
      threes clr b t 

(** [move_score st c] is the score for deternmining the order of moves for 
    [c] in [st]*)
let move_score st c = 
  threes (other_color st.turn) st.board (empty_spots st.board)

(** [col_sort (k1, k2) (k2, v2)] is < 0 if v1 < v2, > 0 if v1 > v2 and 0 
    otherwise *)
let col_sort (k1,v1) (k2, v2) = 
  v2 - v1

(** [next_col c] is the next column that should be checked after [c]. *)
let next_col = function
  | 4 -> 3
  | 3 -> 5
  | 5 -> 2
  | 2 -> 6
  | 6 -> 1
  | 1 -> 7
  | _ -> 8

(** [get_score_list st] is the list of columns in order of increasing 
    score for [st]*)
let get_score_list st = 
  let rec get_score st c = 
    if c > 7 then [] else
      (c, move_score st c) :: get_score st (next_col c)
  in List.stable_sort col_sort (get_score st 4)

(** [new_next_col c lst] is the column that comes after [c] in [lst]*)
let rec new_next_col c = function
  | (h, _) :: ((h', _) as s) :: tl -> if h = c then h' else
      new_next_col c (s :: tl)
  | _ -> 8

(** [put vis k v] is [vis] with ([k], [v]) injected into it *)
let put vis k v =
  if List.length vis > 10000 then
    match List.rev vis with
    | h :: tl -> (k, v) :: (List.rev tl)
    | [] -> (k, v) :: []
  else (k, v) :: vis

(** [check_safe st c] is true if a move in [c] won't lead to the opposing player 
    directly winning and false otherwise*)
let check_safe st c =
  let rec check_safe_aux c = function
    | [] -> false
    | (h, _) :: t -> h = c || check_safe_aux c t
  in check_safe_aux c (safe_moves st (possible_moves st)) 

(** [solve st] is the column, corresponding score, and boards visited
    respectively after choosing a move for [st]*)
let rec solve st =
  print_endline "Loading...";
  let i = if count_moves st.board < 26 then 
      (count_moves st.board)/6 + 2 else 9999 in
  let rec solve_aux min max c st = 
    if min >= max then (c, min, st.visit) else
      let med = min + (max - min)/2 in
      let med' = if med <= 0 && min/2 < med then min/2
        else if med >= 0 && max/2 > med then max/2 
        else med in
      let (col, r, v) = get_score st med' (med'+ 1) st.visit c i in
      if r <= med' then solve_aux min r col (update_vis st v) else
        solve_aux r max col (update_vis st v)
  in solve_aux (-1) 1 4 st

(** [get_score st alpha beta vis col i] is a potential column, corresponding 
    score, and boards visited at a depth of [i]*)
and get_score st alpha beta vis col i = 
  if check_full st.board then (1, 0, vis) else
    match moves_that_win st with
    | (x, y) :: tl -> (x, (43 - (count_moves st.board))/2, vis)
    | [] -> 
      let max = (41 - (count_moves st.board))/2 in
      let bm = if beta > max then max else beta in
      if alpha >= bm then (col, bm, vis) else
        calc_scores st alpha bm vis i

(** [calc_scores st a bm v i] is the best best column, score, and boards 
    visited respectfully with a depth of [i]*)
and calc_scores st a bm v i =
  let rec calc_scores_aux st c alpha beta vis col i = 
    if i < 1 then (1, 0, vis) else
    if c > 7 then (col, alpha, vis) else
    if playable st.board c && check_safe st c then
      let new_st = move st c in
      let score = if List.mem_assoc new_st.board vis then 
          List.assoc new_st.board vis else
          let neg = 
            match (get_score new_st (-beta) (-alpha) vis col (i-1)) with
            | (c', s', _) -> (c', -s')
          in match neg with
          | (_, ss) -> ss
      in
      if score >= beta then (c, score, vis) else
      if score > alpha then 
        let new_vis = put vis new_st.board score in
        let new_c = new_next_col c (get_score_list st) in
        calc_scores_aux st new_c score beta new_vis c i
      else 
        let new_vis' = put vis new_st.board score in
        let new_c' = new_next_col c (get_score_list st) in
        calc_scores_aux st new_c' alpha beta new_vis' col i
    else 
      let new_c'' = new_next_col c (get_score_list st) in
      calc_scores_aux st new_c'' alpha beta vis col i
  in calc_scores_aux st 4 a bm v 4 i

(** [pick_rand_from lst] is a random element of [lst]. *)
let pick_rand_from lst =
  let rand = Random.int (List.length lst) in
  List.nth lst rand

(** [one_played st] is the column where the cpu should play when 1 piece has
    been played so far. *)
let one_played st = 
  match st.moves with
  | [1] -> pick_rand_from [2; 4]
  | [2] -> 3
  | [3] -> pick_rand_from [3; 4; 5; 6]
  | [4] -> 4
  | [5] -> pick_rand_from [2; 3; 4; 5]
  | [6] -> 5
  | _ -> pick_rand_from [4; 6]

(** [two_played st] is the column where the cpu should play when 2 pieces have
    been played so far, assuming the cpu played perfectly on their first turn.*)
let two_played st =
  match st.moves with
  | [4; 1] -> 4
  | [4; 2] -> pick_rand_from [2; 6]
  | [4; 3] -> 6
  | [4; 4] -> 4
  | [4; 5] -> 2
  | [4; 6] -> pick_rand_from [2; 6]
  | _ -> 4

(** [three_played st] is the column where the cpu should play when 3 pieces have
    been played so far, assuming the cpu played perfectly on their first turn.*)
let three_played st = 
  match st.moves with
  | [1; 2; 1] -> pick_rand_from [1; 4]
  | [1; 2; 2] -> 2
  | [1; 2; 3] -> 3
  | [1; 2; 4] -> 4
  | [1; 2; 5] -> 6
  | [1; 2; 6] -> 5
  | [1; 2; 7] -> pick_rand_from [2; 5; 6]
  | [1; 4; 1] -> pick_rand_from [4; 5]
  | [1; 4; 2] -> 4
  | [1; 4; 3] -> 4
  | [1; 4; 4] -> pick_rand_from [4; 6]
  | [1; 4; 5] -> 4
  | [1; 4; 6] -> 4
  | [1; 4; 7] -> pick_rand_from [3; 5]
  | [2; 3; 1] -> 3
  | [2; 3; 2] -> 2
  | [2; 3; 3] -> 3
  | [2; 3; 4] -> 4
  | [2; 3; 5] -> 3
  | [2; 3; 6] -> pick_rand_from [3; 7]
  | [2; 3; 7] -> pick_rand_from [2; 3; 6]
  | [3; 3; 1] -> 4
  | [3; 3; 2] -> pick_rand_from [1; 4; 5]
  | [3; 3; 3] -> 4
  | [3; 3; 4] -> pick_rand_from [2; 5]
  | [3; 3; 5] -> 4
  | [3; 3; 6] -> 3
  | [3; 3; 7] -> 3
  | [3; 4; 1] -> 4
  | [3; 4; 2] -> 4
  | [3; 4; 3] -> 3
  | [3; 4; 4] -> 4
  | [3; 4; 5] -> pick_rand_from [3; 4; 5]
  | [3; 4; 6] -> 4
  | [3; 4; 7] -> 4
  | [3; 5; 1] -> 2
  | [3; 5; 2] -> 2
  | [3; 5; 3] -> 3
  | [3; 5; 4] -> 4
  | [3; 5; 5] -> 5
  | [3; 5; 6] -> 5
  | [3; 5; 7] -> 5
  | [3; 6; 1] -> pick_rand_from [2; 3]
  | [3; 6; 2] -> 4
  | [3; 6; 3] -> 3
  | [3; 6; 4] -> 2
  | [3; 6; 5] -> pick_rand_from [2; 3; 4; 6]
  | [3; 6; 6] -> 3
  | [3; 6; 7] -> 2
  | [4; 4; 1] -> pick_rand_from [3; 4]
  | [4; 4; 2] -> 3
  | [4; 4; 3] -> pick_rand_from [2; 5]
  | [4; 4; 4] -> 4
  | [4; 4; 5] -> pick_rand_from [3; 6]
  | [4; 4; 6] -> 5
  | [4; 4; 7] -> pick_rand_from [4; 5]
  | [5; 2; 1] -> 6
  | [5; 2; 2] -> 5
  | [5; 2; 3] -> pick_rand_from [2; 4; 5; 6]
  | [5; 2; 4] -> 6
  | [5; 2; 5] -> 5
  | [5; 2; 6] -> 4
  | [5; 2; 7] -> pick_rand_from [5; 6]
  | [5; 3; 1] -> 3
  | [5; 3; 2] -> 3
  | [5; 3; 3] -> 3
  | [5; 3; 4] -> 4
  | [5; 3; 5] -> 5
  | [5; 3; 6] -> 6
  | [5; 3; 7] -> 6
  | [5; 4; 1] -> 4
  | [5; 4; 2] -> 4
  | [5; 4; 3] -> pick_rand_from [3; 4; 5]
  | [5; 4; 4] -> 4
  | [5; 4; 5] -> 5
  | [5; 4; 6] -> 4
  | [5; 4; 7] -> 4
  | [5; 5; 1] -> 5
  | [5; 5; 2] -> 5
  | [5; 5; 3] -> 4
  | [5; 5; 4] -> pick_rand_from [3; 6]
  | [5; 5; 5] -> 4
  | [5; 5; 6] -> pick_rand_from [3; 4; 7]
  | [5; 5; 7] -> 4
  | [6; 5; 1] -> pick_rand_from [2; 5; 6]
  | [6; 5; 2] -> pick_rand_from [1; 5]
  | [6; 5; 3] -> 5
  | [6; 5; 4] -> 4
  | [6; 5; 5] -> 5
  | [6; 5; 6] -> 6
  | [6; 5; 7] -> 5
  | [7; 4; 1] -> pick_rand_from [3; 5]
  | [7; 4; 2] -> 4
  | [7; 4; 3] -> 4
  | [7; 4; 4] -> pick_rand_from [2; 4]
  | [7; 4; 5] -> 4
  | [7; 4; 6] -> 4
  | [7; 4; 7] -> pick_rand_from [3; 4]
  | [7; 6; 1] -> pick_rand_from [2; 3; 6]
  | [7; 6; 2] -> 3
  | [7; 6; 3] -> 2
  | [7; 6; 4] -> 4
  | [7; 6; 5] -> 5
  | [7; 6; 6] -> 6
  | [7; 6; 7] -> pick_rand_from [4; 7]
  | _ -> 4

(** [count_bot st i start] is the amoung of pieces on the bottom row in [st] 
    from columns [i] to [start]*)
let rec count_bot st i start =
  if i < 3 then 
    match pos_status st.board (i + start, 1) with
    | None | Some Red -> 0 + count_bot st (i + 1) start
    | Some Blue -> 1 + count_bot st (i + 1) start
  else 0

(** [five_played st] is the best move for when five pieces are placed in [st] *)
let five_played st =
  if count_bot st 0 1 > 0 && count_bot st 0 5 = 0 then 5
  else if count_bot st 0 1 = 0 && count_bot st 0 5 > 0 then 3
  else 4

(** [start_solve st] is the algorithm for the cpu's moves near the start of 
    the game.*)
let start_solve st =
  match num_pieces_on_board st.board with
  | 0 -> Unix.sleepf 1.0; (4, st.visit)
  | 1 -> Unix.sleepf 1.0; (one_played st, st.visit)
  | 2 -> Unix.sleepf 1.0; (two_played st, st.visit)
  | 3 -> Unix.sleepf 1.0; (three_played st, st.visit)
  | 5 -> Unix.sleepf 1.0; (five_played st, st.visit)
  | _ -> let (c, r, v) = solve st in 
    if check_safe st c then (c, v) else 
    if playable st.board c then (c, v) else
      let moves = safe_moves st (possible_moves st) in
      begin
        match moves with
        | [] -> let (c', _) = pick_rand_from (possible_moves st) in (c', v)
        | h::t -> let (c', _) = pick_rand_from moves in (c', v)
      end

let cpu_move_hard st =
  match moves_that_win st with  
  | (x, y) :: tl -> Unix.sleepf 1.0; (x, st.visit)
  | [] -> begin
      match moves_that_block st with 
      | (x, y) :: tl -> Unix.sleepf 1.0; (x, st.visit)
      | [] -> start_solve st
    end

let make_state b t w m v = 
  {board = b; turn = t; wins = w; moves = m; visit = v}