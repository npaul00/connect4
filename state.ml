type color = Red | Blue
type position = int * int
type status = color option
type board = (position * status) list
type num_wins = int * int * int

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

let set_turn t clr =
  {board = board t; turn = clr; wins = wins t}

let empty = 
  []

let red_wins t =
  match t.wins with
  | (r, _, _) -> r

let blue_wins t =
  match t.wins with
  | (_, b, _) -> b

let num_ties t = 
  match t.wins with
  | (_, _, ties) -> ties

let rec empty_board b r c = 
  if c > 7 && r < 6 then
    empty_board b (r+1) 1
  else if c < 8 && r < 7 then
    empty_board (((c,r), None) :: b) r (c+1)
  else 
    b

let init_state = 
  {board = (empty_board empty 1 1); turn = Blue; wins = (0, 0, 0)}

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

(** [check_win b clr] is true if [clr] is winning and false otherwise*)
let check_win b clr =
  let positions = pos_by_color b clr in
  check_right_diag b clr positions ||
  check_left_diag b clr positions ||
  check_horiz b clr positions ||
  check_vert b clr positions

(** [winning_player t] is the winner at state [t] *)
let winning_player t = 
  if (check_win t.board Red) then Some Red
  else if (check_win t.board Blue) then Some Blue
  else None 

let new_color wins = 
  match wins with
  | (a, b, t) -> if (a + b + t) mod 2 = 0 then Blue else Red 

(** [update_wins t] is the state made by resetting [t], with an empty board, the
    other player starting, and the win counts updated.*)
let update_wins t =
  match winning_player t, t.wins with
  | Some Red, (red, blue, ties) -> 
    let u_wins = (red + 1, blue, ties) in
    {board = (empty_board empty 1 1); turn = new_color u_wins; wins = u_wins} 
  | Some Blue, (red, blue, ties) -> 
    let u_wins = (red, blue + 1, ties) in
    {board = (empty_board empty 1 1); turn = new_color u_wins; wins = u_wins} 
  | None, (red, blue, ties) -> 
    let u_wins = (red, blue, ties + 1) in
    {board = (empty_board empty 1 1); turn = new_color u_wins; wins = u_wins} 

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

(** [anim t c low high] is an animation for a piece falling from [high] to [low]
    in column [c] at state [t] *)
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
  if height < 7 then
    {board = update c height t.turn t.board;
     turn = other_color t.turn;
     wins = t.wins}
  else t

let move_anim t c = 
  let height = drop_height c t.board in
  if height < 7 then
    begin 
      anim t c height 7;
      {board = update c height t.turn t.board;
       turn = other_color t.turn;
       wins = t.wins} end
  else t


(** returns true if the board is full*)
let rec check_full b =
  match b with
  | [] -> true
  | (_, None):: t -> false
  | _:: t -> check_full t

(**[state_w_other_color t] is the state with the same board as [t.board] 
   but with the turn changed to the other color. *)
let state_w_other_color t = 
  {board = t.board; turn = other_color t.turn; wins = t.wins}

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
  (not (check_win (move t c).board (other_color t.turn))) &&
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
  check_win (move t c).board clr

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
  will_win new_st c (new_st.turn)

(**[safe_moves t lst] is a list of positions from [lst] that wouldn't cause the 
   current player of [t] to allow the opponent to win next turn by placing a 
   piece on top of theirs. *)
let rec safe_moves t = function
  | [] -> []
  | (x, y) :: tl -> 
    if will_cause_four t x then safe_moves t tl 
    else (x, y) :: safe_moves t tl

let will_eliminate_potential t c = 
  block_four (move t c) c

let rec safer_moves t = function
  | [] -> []
  | (x, y) :: tl ->
    if will_eliminate_potential t x then safer_moves t tl
    else (x, y) :: safer_moves t tl

(**[cpu_choose_move t i count lst] is the column the computer should play in, 
   found by choosing the [ith] element of [lst], a list of possible positions.*)
let rec cpu_choose_move t i lst = 
  let rec cpu_choose_move_aux t' i' count' = function 
    | (x, y) :: tl -> 
      if count' = i' then x else cpu_choose_move_aux t' i' (count'+1) tl
    | [] -> failwith "No possible moves"
  in cpu_choose_move_aux t i 0 lst

let rec num_pieces_on_board = function
  | [] -> 0
  | ((x, y), Some clr) :: tl -> 1 + num_pieces_on_board tl
  | ((x, y), None) :: tl -> num_pieces_on_board tl

let rec piece_on_board = function
  | ((x, y), Some clr) :: tl -> x
  | ((x, y), None) :: tl -> piece_on_board tl
  | [] -> (-1)

let next_to loc =
  if loc = (-1) then 4 else
  if loc < 4 then loc + 1 else
  if loc > 4 then loc - 1 else
    let rand = Random.int 2 in
    if rand = 0 then loc - 1 else loc + 1

(**[cpu_move t] is the column that the computer should place a piece in. *)
let cpu_move t =
  if num_pieces_on_board t.board <= 1 then next_to (piece_on_board t.board) else
    match moves_that_win t with
    | (x, y) :: tl -> x
    | [] -> 
      match moves_that_block t with 
      | (x, y) :: tl -> x
      | [] -> 
        let p_moves = possible_moves t in
        let s_moves = safe_moves t p_moves in 
        let safer = safer_moves t s_moves in
        match safer with
        | (x, y) :: tl -> 
          cpu_choose_move t (Random.int (List.length safer)) safer
        | [] ->
          match s_moves with
          | (x, y) :: tl -> 
            cpu_choose_move t (Random.int (List.length s_moves)) s_moves
          | [] -> cpu_choose_move t (Random.int (List.length p_moves)) p_moves

let cpu_move_easy t =
  match moves_that_win t with
  | (x, y) :: tl -> x
  | [] -> 
    match moves_that_block t with 
    | (x, y) :: tl -> x
    | [] -> 
      let p_moves = possible_moves t in
      cpu_choose_move t (Random.int (List.length p_moves)) p_moves



(** [count_moves b] is the amount of times a piece has been placed in [b] *)
let rec count_moves b =
  match b with 
  | [] -> 0
  | (_, Some c)::t -> 1 + count_moves t
  | _::t -> count_moves t

(** [playable b c] is true if column [c] in [b] isn't full and false otherwise*)
let playable b c =
  drop_height c b != 7

let rec get_score st = 
  if check_full st.board then 0 else
    match moves_that_win st with
    | h :: tl -> (43 - (count_moves st.board))/2
    | [] -> 
      let best_score = -42 in
      calc_scores st best_score

and calc_scores st b_score =
  let rec calc_scores_aux st c score = 
    if c>7 then score else
    if playable st.board c then
      let new_st = move st c in
      let new_score = -get_score new_st in
      if new_score > score then calc_scores_aux st (c+1) new_score else
        calc_scores_aux st (c+1) score
    else calc_scores_aux st (c+1) score
  in calc_scores_aux st 1 b_score




(*
(** [search_win st c clr] is < 50 if there is a winning move*)
let rec search_win st c clr =
  if playable (board st) c && will_win st c then 22 - count_moves (board st) clr
  else if (c>7) then 50
  else search_win st (c+1) clr

(** [get_score min max st clr] is the best score among each move for [clr] in 
    [st]*)
let rec get_score0 min max st clr = 
  let board = board st in 
  if check_full board then 0
  else
  if search_win st 1 clr < 50 then search_win st 1 clr  
  else 
    next_move min max st clr 4 

(** [next_move min max st clr i] is the next move for [clr] in [st] 
    while searching with a depth of [i]*)
and next_move min max st clr i =
  let next_i = if i = 4 then 3
    else if i = 3 then 5
    else if i = 5 then 2
    else if i = 2 then 6
    else if i = 6 then 1 
    else if i = 1 then 7
    else 8 in
  if i < 8 then
    let color = if clr = Red then Blue else Red in
    if playable (board st) i &&  -1 * (get_score0 (-1 * min) (-1 * max) (move st i) color) >= max then 
      -1 * (get_score0 (-1 * min) (-1 * max) (move st i) color)
    else if playable (board st) i && -1 * (get_score0 (-1 * min) (-1 * max) (move st i) color) > min then
      next_move (-1 * (get_score0 (-1 * min) (-1 * max) (move st i) color)) max st clr next_i
    else next_move min max st clr next_i
  else
    min
*)


