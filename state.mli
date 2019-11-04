(** The type for the color of a piece. *)
type color

(** The type for the (x,y) coordinate position on the board. *)
type position

(** The type for the status of a position on the board. *)
type status

(** The type for a board. *)
type board

(** [display b] displays the board [b]. *)
val display : board -> unit

(** [move b c col] is the state of board [b] after a player with color [col] 
    puts a piece into column [c]. *)
val move : board -> int -> color -> board

(** [check_win b col] checks if there are four pieces with color [col] in a row.
*)
val check_win : board -> color -> bool