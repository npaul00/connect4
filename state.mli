(** The type for the color of a piece. *)
type color

(** The type for the (x,y) coordinate position on the board. *)
type position

(** The type for the status of a position on the board. *)
type status

(** The type for the board. *)
type board

(** The abstract type of values representing the game state. *)
type t

val turn : t -> color

val board : t -> board

val init_state : t

(** [display b] displays the board [b]. *)
val display : board -> int -> unit

(** [move t c] is the game state after the current player places a piece into
    column [c] in the current state [t]. *)
val move : t -> int -> t

(** [check_win b clr] checks if there are four pieces with color [clr] in a row.
*)
val check_win : board -> color -> bool

(** [empty_board b r c] is an empty board with [r] rows and [c] columns*)
val empty_board : board -> int -> int -> board

(** [empty] is an empty list of boards *)
val empty : board

(** [drop_height col b] is the height of the piece being dropped in column [col]
    in board [b]. *)
val drop_height : int -> board -> int

(** [color_to_string clr] is the String representation of the color type. *)
val color_to_string : color -> string

(** [other_color clr] is the color other than [clr]. *)
val other_color : color -> color