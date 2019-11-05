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

(** [check_win b col] checks if there are four pieces with color [col] in a row.
*)
val check_win : board -> color -> bool

(** [empty_board b r c] is an empty board with [r] rows and [c] columns*)
val empty_board : board -> int -> int -> board

(** [empty] is an empty list of boards *)
val empty : board

val drop_height : int -> board -> int

