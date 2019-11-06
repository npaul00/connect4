type color = Red | Black
type position = int * int
type status = color option
type board = (position * status) list

type t = {
  board : board;
  turn : color
}

let board t =
  t.board

let turn t =
  t.turn

let empty = 
  []

let rec empty_board b r c = 
  if c > 7 && r < 6 then
    empty_board b (r+1) 1
  else if c < 8 && r < 7 then
    empty_board (((c,r), None)::b) r (c+1)
  else 
    b

let init_state = {board = (empty_board empty 1 1); turn = Black}

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
  | Some Black -> ANSITerminal.(print_string [cyan] "O")
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

let horiz (x, y) = 
  [(x, y); (x+1, y); (x+2, y); (x+3, y)] 

let vert (x, y) = 
  [(x, y); (x, y+1); (x, y+2); (x, y+3)]

let right_diag (x, y) = 
  [(x, y); (x+1, y+1); (x+2, y+2); (x+3, y+3)]

let left_diag (x, y) = 
  [(x, y); (x-1, y+1); (x-2, y+2); (x-3, y+3)]

(** [get_pos b clr] is a list of the positions that have a piece with color 
    [clr] in board [b]. *)
let rec get_pos b clr = 
  match b with
  | [] -> []
  | (pos, Some c) :: t -> if c = clr then pos :: get_pos t clr 
    else get_pos t clr
  | (pos, None) :: t -> get_pos t clr

(** [pos_status b (x,y) is the color of the piece on the board at position 
    (x,y)] *)
let rec pos_status b (x,y) =
  match b with
  | [] -> None
  | ((a, b), Some c) :: t -> if a = x && b = y then Some c 
    else pos_status t (x,y)
  | ((a, b), None) :: t -> if a = x && b = y then None 
    else pos_status t (x, y)

(** [four_in_a_row lst b clr] is true if the four positions in [lst] have
    the same color [clr].  *)
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
  let positions = get_pos b clr in
  check_right_diag b clr positions ||
  check_left_diag b clr positions ||
  check_horiz b clr positions ||
  check_vert b clr positions

let other_color = function
  | Red -> Black
  | Black -> Red

let color_to_string = function
  | Red -> "Red"
  | Black -> "Black"

let rec pieces_in_col column board = 
  match board with
  | [] -> []
  | ((col, row), Some c) :: t -> 
    if col = column then row :: pieces_in_col column t
    else pieces_in_col column t
  | h :: t -> pieces_in_col column t

let drop_height column board =
  match List.rev(List.sort compare (pieces_in_col column board)) with
  | [] -> 1
  | h :: t -> h + 1

let rec update x y col = function
  | [] -> []
  | ((x', y'), _) as pair :: t -> 
    if x' = x && y' = y then ((x, y), Some col) :: t else
      pair :: update x y col t

let move t c = 
  let height = drop_height c t.board in
  if height < 7 then
    {board = update c (drop_height c t.board) t.turn t.board;
     turn = other_color t.turn}
  else t
