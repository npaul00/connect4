(** The type for the color of a piece. *)
type color = Red | Blue

(** The type for the (x,y) coordinate position on the board. *)
type position

(** The type for the status of a position on the board. *)
type status

(** The type for the board. *)
type board

(** The abstract type of values representing the game state. *)
type t

(** The type for the number of wins for the red and blue players respectively. *)
type num_wins

(** The type for the list of moves. *)
type moves_list = int list

(** [turn t] is the color of whose turn it is to play in state [t]. *)
val turn : t -> color

(** [set_turn t clr] is the state [t] at color [clr] *)
val set_turn : t -> color -> t 

(** [board t] is the game board of state [t]. *)
val board : t -> board

(** [moves t] is the moves list of state [t]. *)
val moves : t -> moves_list

(** [wins t] is the number of wins for the red and blue players in [t]. *)
val wins : t -> num_wins

(** [red_wins t] is the number of wins for the red player in [t]. *)
val red_wins : t -> int

(** [blue_wins t] is the number of wins for the blue player in [t]. *)
val blue_wins : t -> int

(** [init_state] is the initial state of the game, with no pieces on the board, 
    and it being Blue's turn to play. *)
val init_state : t

(** [display b i] displays the board [b]. *)
val display : board -> int -> unit

(** [move t c] is the game state after the current player places a piece into
    column [c] in the current state [t]. *)
val move : t -> int -> t

(** [move t c] is the game state after the current player places a piece into
    column [c] in the current state [t] with a fall animation. *)
val move_anim : t -> int -> t

(** [check_win b clr] checks if there are 4 pieces with color [clr] in a row. *)
val check_win : board -> color -> bool

(** [winning_player t] is Some [clr] if that color won, or [None] if no one is 
    winning. *)
val winning_player : t -> color option

(** [update_wins t wins] is the game state after someone wins. *)
val update_wins : t -> t

(** [empty_board] is an empty board with 6 rows and 7 columns.*)
val empty_board : board

(** [empty] is an empty list *)
val empty : board

(** [color_to_string clr] is the string representation of color [clr]. *)
val color_to_string : color -> string

(** [other_color clr] is [Blue] if [clr] is [Red], and [Red] if [clr] is 
    [Blue]. *)
val other_color : color -> color

(** [check_full b] is if board [b] is full. *)
val check_full : board -> bool

(** [cpu_move t] is move chosen by the medium bot *)
val cpu_move : t -> int

(** [cpu_move_easy t] is move chosen by the easy bot *)
val cpu_move_easy : t -> int

(** [cpu_move_hard t] is move chosen by the hard bot *)
val cpu_move_hard : t -> int

(** [drop_height c b] is the highest open spot in column [c] on board [b] *)
val drop_height : int -> board -> int

(** [update x y clr] is the board but with a piece in [x], [y] with color 
    [clr] *)
val update : int -> int -> color -> board -> board

(** [new_color wins] is the new starting color of a new game*)
val new_color : num_wins -> color

val red_diag_win : board

val blue_horiz_win : board

val full_board_tie : board

val blue_horiz_win_updated : board

val empty_updated : board

val man_empty_board : board
