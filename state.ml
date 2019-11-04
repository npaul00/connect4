type color = Red | Black
type position = int * int
type status = color option
type board = (position * status) list

let display b =
  failwith "Unimplemented"

let move b c col = 
  failwith "Unimplemented"

let check_win b col =
  failwith "Unimplemented"