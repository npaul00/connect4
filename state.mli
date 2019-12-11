(** 
   Representation of dynamic game state.

   This module represents the state of a game as it is being played, including
   the current board, whose turn it is, the win count, the moves that have been 
   taken, and the boards that have been scored when calculating AI moves,
   and functions that cause the state to change.
*)

(** The abstract type of values representing the game state. *)
type t

(** The type for the color of a piece. *)
type color = Red | Blue

(** The type for the (x,y) coordinate position on the board. *)
type position = int * int

(** The type for the status of a position on the board. *)
type status = color option

(** The type for the board. *)
type board = (position * status) list

(** The type for the number of wins for the red and blue players respectively. *)
type num_wins = int * int * int

type visited = (board * int) list

(** The type for the list of moves. *)
type moves_list = int list

(** The type for a boolean or a position list. *)
type bool_or_pos = Truth of bool | Pos of position list

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

(** [num_ties t] is the number of ties in [t]. *)
val num_ties : t -> int

(** [init_state] is the initial state of the game, with no pieces on the board, 
    and it being Blue's turn to play. *)
val init_state : t

(** [move t c] is the game state after the current player places a piece into
    column [c] in the current state [t]. *)
val move : t -> int -> t

(** [move t c] is the game state after the current player places a piece into
    column [c] in the current state [t] with a fall animation. *)
val move_anim : t -> int -> t

(** [move t c] is the game state after the current player places a piece into
    column [c] in the current state [t] with a fall animation in night mode. *)
val move_anim_d : t -> int -> t

(** [get_truth b] turns [b] into a boolean.
    Requires:
    [b] is a Truth, not a Pos. *)
val get_truth : bool_or_pos -> bool

(** [get_pos b] turns [b] into a position list.
    Requires:
    [b] is a Pos, not a Truth. *)
val get_pos : bool_or_pos -> position list

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

(** [update_vis t vis] is [t] with [vis] added to visited *)
val update_vis : t -> visited -> t

(** [cpu_move_med t] is move chosen by the medium bot *)
val cpu_move_med : t -> int * visited

(** [cpu_move_easy t] is move chosen by the easy bot *)
val cpu_move_easy : t -> int * visited

(** [cpu_move_hard t] is move chosen by the hard bot *)
val cpu_move_hard : t -> int * visited

(** [drop_height c b] is the highest open spot in column [c] on board [b] *)
val drop_height : int -> board -> int

(** [update x y clr] is the board but with a piece in [x], [y] with color 
    [clr] *)
val update : int -> int -> color -> board -> board

(** [new_color wins] is the new starting color of a new game*)
val new_color : num_wins -> color

(** [display b i] displays the board [b]. *)
val display : board -> int -> unit

(** [display b i] displays the board [b] in night mode. *)
val display_d : board -> int -> unit

(** ADD DOCS*)
val display_win : color -> board -> int -> unit

(** ADD DOCS*)
val display_win_d : color -> board -> int -> unit

(** [make_state b t w m v] is a state with [b] as a board, [t] as a turn, [w] 
    as an amount of wins, [m] as a list of moves, and [v] as a list of boards 
    seen*)
val make_state : board -> color -> num_wins -> moves_list -> visited -> t