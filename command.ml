
type command = 
  | Go of int
  | Quit

exception Invalid

(**[words lst] is a list of the words (i.e. consecutive sequences of non-space
   characters) in [lst]. *)
let rec words = function
  | [] -> []
  | "" :: t -> words t 
  | h :: t -> h :: words t

let parse str = 
  match words (String.split_on_char ' ' (String.lowercase_ascii str)) with
  | "quit" :: [] -> Quit
  | "go" :: i :: [] -> begin
      try 
        let num = int_of_string i in
        if (num > 0 && num < 8) then Go num else raise Invalid
      with exn -> raise Invalid
    end   
  | _ -> raise Invalid

let rec execute_command st d () = 
  let turn = State.turn st in
  let board = State.board st in
  let last_clr = State.other_color turn in
  if d then State.display board 1;
  if State.check_win board last_clr then 
    print_endline ("\n" ^ State.color_to_string last_clr ^ " wins!")
  else begin
    if d then print_endline ("\n" ^ State.color_to_string turn ^ "'s turn");
    print_string "> ";
    try match parse (read_line()) with
      | Quit -> exit 0
      | Go i -> 
        let new_state = State.move st i in
        if new_state = st then begin
          print_endline "That column is full, try again!";
          execute_command st false ()
        end
        else execute_command new_state true ()
    with 
    | Invalid -> 
      print_endline "Invalid move! Hint: type 'go' and a column number";
      execute_command st false ()
  end