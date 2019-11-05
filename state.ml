type color = Red | Black
type position = int * int
type status = color option
type board = (position * status) list

let empty = 
  []

let rec empty_board b r c = 
  if c > 7 && r < 6 then
    empty_board b (r+1) 1
  else if c < 8 && r < 7 then
    empty_board (((c,r), None)::b) r (c+1)
  else 
    b

let bot = "1 | 2 | 3 | 4 | 5 | 6 | 7 |"

let line = 
  print_string "\n";
  "-----------------------------"

let get_team s =
  match s with
  | Some Red -> "X"
  | Some Black -> "O"
  | None -> " "

let rec print_row b temp r c =
  match temp with
  | [] -> 
    if (c > 7) then print_string "\n";
    if (c > 7) then print_string line;
    if (c > 7) then print_string "\n";
    if (r < 7) && c > 7 then print_string "| ";
  | ((x,y), s) :: t -> 
    (*print_string " x: ";print_int x;
      print_string " c: ";print_int c;
      print_string " y: ";print_int y;
      print_string " r: ";print_int r;
      print_string "\n ";*)
    if (x = c && y = r) then 
      print_string ((get_team s) ^ " | ");
    if (x = c && y = r) then print_row b b r (c + 1); 
    if not (x = c && y = r) then  print_row b t r c

let rec display b r = 
  if r = 1 then print_string "\n";
  if r = 1 then print_string "| ";
  if r < 7 then display b (r + 1);
  if r < 7 then print_row b b r 1;
  if r = 1 then print_string bot

let move b c col = 
  failwith "Unimplemented"

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

let check_win b col =
  failwith "Unimplemented"