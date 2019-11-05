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
  | Some Red -> "X"
  | Some Black -> "O"
  | None -> " "

(** [print_row b temp r c] is the unit that prints the piece at [r] and [c]*)
let rec print_row b temp r c =
  match temp with
  | [] -> 
    if c > 7 then begin print_string "\n"; print_string line; print_string "\n"; 
      if r < 7 then print_string "| "; end 
  | ((x,y), s) :: t -> 
    if x = c && y = r then 
      (print_string ((get_team s) ^ " | "); print_row b b r (c + 1);) 
    else print_row b t r c

let rec display b r = 
  if r = 1 then begin print_string "\n"; print_string "| "; end;
  if r < 7 then begin display b (r + 1); print_row b b r 1; end;
  if r = 1 then print_string bot

(** [get_pos b col] is a list of the positions that have a piece with color 
    [col] in board [b]. *)
let rec get_pos b col = 
  match b with
  | [] -> []
  | (pos, Some c) :: t -> if c = col then pos :: get_pos t col 
    else get_pos t col
  | (pos, None) :: t -> get_pos t col

(** [pos_status b (x,y) is the color of the piece on the board at position 
    (x,y)] *)
let rec pos_status b (x,y) =
  match b with
  | [] -> failwith "Not in board"
  | ((a, b), Some c) :: t -> if a = x && b = y then Some c 
    else pos_status t (x,y)
  | ((a, b), None) :: t -> if a = x && b = y then None 
    else pos_status t (x, y)

let horiz (x, y) = 
  [(x, y); (x+1, y); (x+2, y); (x+3, y)] 

let vert (x, y) = 
  [(x, y); (x, y+1); (x, y+2); (x, y+3)]

let diag (x, y) = 
  [(x, y); (x+1, y+1); (x+2, y+2); (x+3, y+3)]

let rec filter_positions lst =
  match lst with
  | [] -> []
  | (x, y) :: t -> if (x <= 4) && (y <= 3) then 
      (x, y) :: filter_positions t else
      filter_positions t

(** not finished *)
let four_diag b pos = 
  match (diag pos) with
  | [] -> false
  | pos :: t -> true

let check_win t col =
  failwith "Unimplemented"



let other_color = function
  |Red -> Black
  |Black -> Red

let color_to_string = function
  |Red -> "Red"
  |Black -> "Black"

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
