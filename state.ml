type color = Red | Blue
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

let init_state = {board = (empty_board empty 1 1); turn = Blue}

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

let move t c = 
  let height = drop_height c t.board in
  if height < 7 then
    {board = update c (drop_height c t.board) t.turn t.board;
     turn = other_color t.turn}
  else t
