(** TEST PLAN
    We tested most parts of our system using OUnit to make sure all of our 
    functions work properly. We manually created boards and tested to make sure 
    the A.I. made the appropriate move. We did a lot of glass-box testing to 
    make sure the individual parts of our system, i.e. functions that update the 
    state, determine winning players, boards, scores, etc., work properly.  We 
    left off testing helper functions, such as get_truth and get_pos in state.ml,
    as the functions we test use the helper functions and validate their 
    functionality. We also left off functions from the files command.ml, main.ml, 
    and the functions display, display_d, display_win, and display_win_d from 
    state.ml, because those are display functions which we manually tested by 
    running 'make play' in our terminal. We left off move, move_anim, and 
    move_anim_d from state.ml because it updates the "visited" property of the 
    state which is algorithmically complex and inconvenient to test in OUnit; 
    instead we tested them by playing the game for ourselves. We didn't make 
    OUnit tests for situations that induce errors (such as trying to place a 
    piece in a full column) because we tested them in our terminal. Our testing 
    approach demonstrates the correctness of our system because we conducted 
    glass-box testing by testing individual functions, as well as black-box 
    testing by playing the game for ourselves several times and making sure the 
    A.I. makes the correct move in different situations, all the main menu and 
    settings features work, and the U.I. functions properly. *)

open OUnit2
open State
open Command

module type TestCases = sig
  (** [red_diag_win] is a board with a red diagonal win. *)
  val red_diag_win : board

  (** [blue_horiz_win] is a board with a blue horizontal win. *)
  val blue_horiz_win : board

  (** [full_board_tie] is a full board where the players tied. *)
  val full_board_tie : board

  (** [blue_horiz_win_updated] is a board with one piece inserted into 
      [blue_horiz_win]. *)
  val blue_horiz_win_updated : board

  (** [empty_updated] is a board with one piece. *)
  val empty_updated : board

  (** [man_empty_board] is an empty board. *)
  val man_empty_board : board

  (** [blue_3] is a board with 3 blue pieces in a row. *)
  val blue_3 : board

  (** [state_blue_3] is the state with board [blue_3]. *)
  val state_blue_3 : t

  (** [blue_diag_pot] is a board with three blues in a row diagonally. *)
  val blue_diag_pot : board

  (** [state_blue_pot] is the state with the board [blue_diag_pot]. *)
  val state_blue_pot : t

  (** [red_3] is a board with three red pieces in a row. *)
  val red_3 : board

  (** [blue_diag_pot_2] is a board with three blues in a row diagonally. *)
  val blue_diag_pot_2 : board

  (** [state_red_3] is the state with board [red_3]. *)
  val state_red_3 : t

  (** [state_red_3_blue_turn] is [state_red_3] but switched to blue's turn. *)
  val state_red_3_blue_turn : t

  (** [state_blue_3_blue_turn] is [state_blue_3] but switched to red's turn.  *)
  val state_blue_3_blue_turn : t

  (** [state_blue_pot_2] is the state with the board [blue_diag_pot_2]. *)
  val state_blue_pot_2 : t

  (** [state_red_diag_win] is the state with the board [red_diag_win]. *)
  val state_red_diag_win : t

  (** [state_blue_horiz_win] is the state with the board [blue_horiz_win]. *)
  val state_blue_horiz_win : t

  (** [state_tie_board] is the state with the board [full_board_tie]. *)
  val state_tie_board : t

  (** [expected_init_state] is the expected initial state for Connect Four. *)
  val expected_init_state : t

  (** [init_state_updated] is the state for board [empty_updated]. *)
  val init_state_updated : t

  (** [state_one_piece] is a state with a board with 1 piece placed in col 6. *)
  val state_one_piece : t

  (**[state_two_pieces] is a state with a board with 2 pieces placed. *)
  val state_two_pieces : t 

  (**[state_three_pieces] is a state with a board with 3 pieces placed. *)
  val state_three_pieces : t
end

(** MANUALLY-TYPED BOARDS AND STATES FOR TESTING *)
module TstCases : TestCases = struct
  let red_diag_win = [((1,6), None);      ((2,6), None);      ((3,6), None);      ((4,6), None);      ((5,6), None);     ((6,6), None); ((7,6), Some Red);
                      ((1,5), None);      ((2,5), None);      ((3,5), None);      ((4,5), Some Blue); ((5,5), None);     ((6,5), None);  ((7,5), Some Red);
                      ((1,4), None);      ((2,4), None);      ((3,4), Some Red);  ((4,4), Some Red);  ((5,4), None);     ((6,4), None);  ((7,4), Some Red);
                      ((1,3), None);      ((2,3), None);      ((3,3), Some Red);  ((4,3), Some Red);  ((5,3), None);     ((6,3), None);  ((7,3), Some Red);
                      ((1,2), Some Blue); ((2,2), Some Blue); ((3,2), Some Blue); ((4,2), Some Red);  ((5,2), Some Red); ((6,2), Some Blue);  ((7,2), Some Red);
                      ((1,1), Some Red);  ((2,1), Some Red);  ((3,1), Some Blue); ((4,1), Some Blue); ((5,1), Some Blue); ((6,1), Some Red);  ((7,1), Some Red)]
  let blue_horiz_win = [((1,6), None);      ((2,6), None);      ((3,6), None);      ((4,6), None);      ((5,6), None);          ((6,6), None);      ((7,6), None);
                        ((1,5), None);      ((2,5), None);      ((3,5), None);      ((4,5), None);      ((5,5), None);          ((6,5), None);      ((7,5), None);
                        ((1,4), None);      ((2,4), None);      ((3,4), Some Blue);  ((4,4), Some Blue);  ((5,4), Some Blue);   ((6,4), Some Blue); ((7,4), None);
                        ((1,3), None);      ((2,3), None);      ((3,3), Some Red);  ((4,3), Some Blue);  ((5,3), Some Red);     ((6,3), Some Red);  ((7,3), None);
                        ((1,2), Some Red); ((2,2), Some Blue); ((3,2), Some Red); ((4,2), Some Red);    ((5,2), Some Red);      ((6,2), Some Blue); ((7,2), Some Red);
                        ((1,1), Some Red);  ((2,1), Some Red);  ((3,1), Some Blue); ((4,1), Some Red); ((5,1), Some Blue);      ((6,1), Some Blue); ((7,1), Some Blue)]
  let full_board_tie = [((1,6), Some Red);      ((2,6), Some Red);      ((3,6), Some Blue);  ((4,6), Some Red);      ((5,6), Some Blue);   ((6,6), Some Blue); ((7,6), Some Red);
                        ((1,5), Some Blue);      ((2,5), Some Blue);    ((3,5), Some Red);   ((4,5), Some Blue);      ((5,5), Some Red);   ((6,5), Some Red);  ((7,5), Some Blue);
                        ((1,4), Some Red);      ((2,4), Some Red);      ((3,4), Some Red);  ((4,4), Some Blue);    ((5,4), Some Blue);     ((6,4), Some Blue); ((7,4), Some Red);
                        ((1,3), Some Blue);      ((2,3), Some Blue);    ((3,3), Some Red);  ((4,3), Some Blue);     ((5,3), Some Blue);    ((6,3), Some Red);  ((7,3), Some Blue);
                        ((1,2), Some Red);      ((2,2), Some Red);      ((3,2), Some Blue); ((4,2), Some Red);      ((5,2), Some Red);     ((6,2), Some Red); ((7,2), Some Blue);
                        ((1,1), Some Blue);     ((2,1), Some Red);      ((3,1), Some Red); ((4,1), Some Blue);     ((5,1), Some Blue);     ((6,1), Some Red); ((7,1), Some Blue)]
  let blue_horiz_win_updated = [((1,6), None);      ((2,6), None);      ((3,6), None);      ((4,6), None);      ((5,6), None);          ((6,6), None);      ((7,6), None);
                                ((1,5), None);      ((2,5), None);      ((3,5), None);      ((4,5), None);      ((5,5), None);          ((6,5), None);      ((7,5), None);
                                ((1,4), None);      ((2,4), None);      ((3,4), Some Blue);  ((4,4), Some Blue);  ((5,4), Some Blue);   ((6,4), Some Blue); ((7,4), None);
                                ((1,3), Some Blue);      ((2,3), None);      ((3,3), Some Red);  ((4,3), Some Blue);  ((5,3), Some Red);     ((6,3), Some Red);  ((7,3), None);
                                ((1,2), Some Red); ((2,2), Some Blue); ((3,2), Some Red); ((4,2), Some Red);    ((5,2), Some Red);      ((6,2), Some Blue); ((7,2), Some Red);
                                ((1,1), Some Red);  ((2,1), Some Red);  ((3,1), Some Blue); ((4,1), Some Red); ((5,1), Some Blue);      ((6,1), Some Blue); ((7,1), Some Blue)]
  let man_empty_board = [((1,6), None); ((2,6), None); ((3,6), None); ((4,6), None); ((5,6), None); ((6,6), None); ((7,6), None);
                         ((1,5), None); ((2,5), None); ((3,5), None); ((4,5), None); ((5,5), None); ((6,5), None); ((7,5), None);
                         ((1,4), None); ((2,4), None); ((3,4), None); ((4,4), None); ((5,4), None); ((6,4), None); ((7,4), None);
                         ((1,3), None); ((2,3), None); ((3,3), None); ((4,3), None); ((5,3), None); ((6,3), None); ((7,3), None);
                         ((1,2), None); ((2,2), None); ((3,2), None); ((4,2), None); ((5,2), None); ((6,2), None); ((7,2), None);
                         ((1,1), None); ((2,1), None); ((3,1), None); ((4,1), None); ((5,1), None); ((6,1), None); ((7,1), None)]
  let empty_updated = [((1,6), None); ((2,6), None); ((3,6), None);      ((4,6), None); ((5,6), None); ((6,6), None); ((7,6), None);
                       ((1,5), None); ((2,5), None); ((3,5), None);      ((4,5), None); ((5,5), None); ((6,5), None); ((7,5), None);
                       ((1,4), None); ((2,4), None); ((3,4), None);      ((4,4), None); ((5,4), None); ((6,4), None); ((7,4), None);
                       ((1,3), None); ((2,3), None); ((3,3), None);      ((4,3), None); ((5,3), None); ((6,3), None); ((7,3), None);
                       ((1,2), None); ((2,2), None); ((3,2), None);      ((4,2), None); ((5,2), None); ((6,2), None); ((7,2), None);
                       ((1,1), None); ((2,1), None); ((3,1), Some Blue); ((4,1), None); ((5,1), None); ((6,1), None); ((7,1), None)]
  let blue_3 = [((1,6), None);   ((2,6), None);      ((3,6), None);      ((4,6), None); ((5,6), None); ((6,6), None); ((7,6), None);
                ((1,5), None);     ((2,5), None);      ((3,5), None);      ((4,5), None); ((5,5), None); ((6,5), None); ((7,5), None);
                ((1,4), None);     ((2,4), None);      ((3,4), None);      ((4,4), None); ((5,4), None); ((6,4), None); ((7,4), None);
                ((1,3), Some Blue); ((2,3), None);      ((3,3), None);      ((4,3), None); ((5,3), None); ((6,3), None); ((7,3), None);
                ((1,2), Some Blue); ((2,2), Some Red); ((3,2), None);      ((4,2), None); ((5,2), None); ((6,2), None); ((7,2), None);
                ((1,1), Some Blue); ((2,1), Some Red); ((3,1), Some Red); ((4,1), None); ((5,1), None); ((6,1), None); ((7,1), None)]
  let blue_diag_pot = [((1,6), None); ((2,6), None);      ((3,6), None);     ((4,6), None);      ((5,6), None);      ((6,6), None);      ((7,6), None);
                       ((1,5), None); ((2,5), None);      ((3,5), None);     ((4,5), None);      ((5,5), None);      ((6,5), None);      ((7,5), None);
                       ((1,4), None); ((2,4), None);      ((3,4), None);     ((4,4), None);      ((5,4), None);      ((6,4), None);      ((7,4), None);
                       ((1,3), None); ((2,3), None);      ((3,3), None);     ((4,3), None);      ((5,3), None);      ((6,3), Some Blue); ((7,3), None);
                       ((1,2), None); ((2,2), None);      ((3,2), None);     ((4,2), Some Blue); ((5,2), Some Blue); ((6,2), Some Red);  ((7,2), Some Red);
                       ((1,1), None); ((2,1), Some Blue); ((3,1), Some Red);  ((4,1), Some Blue); ((5,1), Some Red);   ((6,1), Some Red);  ((7,1), Some Blue)]
  let red_3 = [((1,6), None);     ((2,6), None);      ((3,6), None);      ((4,6), None);      ((5,6), None);       ((6,6), None);       ((7,6), None);
               ((1,5), None);     ((2,5), None);      ((3,5), None);      ((4,5), None);      ((5,5), None);       ((6,5), None);       ((7,5), None);
               ((1,4), None);     ((2,4), None);      ((3,4), None);      ((4,4), None);      ((5,4), None);       ((6,4), None);       ((7,4), None);
               ((1,3), None);     ((2,3), None);      ((3,3), None);      ((4,3), None);      ((5,3), None);       ((6,3), Some Red);   ((7,3), Some Blue);
               ((1,2), None);     ((2,2), None);      ((3,2), None);      ((4,2), Some Red);  ((5,2), Some Red);   ((6,2), Some Blue);  ((7,2), Some Blue);
               ((1,1), Some Red); ((2,1), Some Red);  ((3,1), Some Blue); ((4,1), Some Red);  ((5,1), Some Blue);  ((6,1), Some Blue);  ((7,1), Some Red)]

  let blue_diag_pot_2 = [((1,6), None);      ((2,6), None); ((3,6), None);      ((4,6), Some Blue); ((5,6), None);     ((6,6), None); ((7,6), None);
                         ((1,5), None);      ((2,5), None); ((3,5), None);      ((4,5), Some Red);  ((5,5), None);     ((6,5), None); ((7,5), None);
                         ((1,4), None);      ((2,4), None); ((3,4), None);      ((4,4), Some Blue); ((5,4), None);     ((6,4), None); ((7,4), None);
                         ((1,3), None);      ((2,3), None); ((3,3), Some Blue); ((4,3), Some Red);  ((5,3), None);     ((6,3), None); ((7,3), None);
                         ((1,2), None);      ((2,2), None); ((3,2), Some Red);  ((4,2), Some Blue); ((5,2), Some Red); ((6,2), None); ((7,2), None);
                         ((1,1), Some Blue); ((2,1), None); ((3,1), Some Blue); ((4,1), Some Red);  ((5,1), Some Red); ((6,1), None); ((7,1), None)]

  let test_safe = [((1,6), None);     ((2,6), Some Blue);      ((3,6), Some Blue);      ((4,6), Some Red);      ((5,6), None);       ((6,6), Some Red);       ((7,6), None);
                   ((1,5), Some Blue);     ((2,5), Some Red);      ((3,5), Some Red);      ((4,5), Some Blue);      ((5,5), None);       ((6,5), Some Blue);       ((7,5), None);
                   ((1,4), Some Red);     ((2,4), Some Blue);      ((3,4), Some Red);      ((4,4), Some Red);      ((5,4), None);       ((6,4), Some Red);       ((7,4), None);
                   ((1,3), Some Blue);     ((2,3), Some Red);      ((3,3), Some Blue);      ((4,3), Some Blue);      ((5,3), None);       ((6,3), Some Blue);   ((7,3), None);
                   ((1,2), Some Red);     ((2,2), Some Blue);      ((3,2), Some Red);      ((4,2), Some Red);  ((5,2), None);   ((6,2), Some Blue);  ((7,2), None);
                   ((1,1), Some Blue); ((2,1), Some Red);  ((3,1), Some Red); ((4,1), Some Blue);  ((5,1), Some Blue);  ((6,1), Some Blue);  ((7,1), Some Red)]

  let one_piece = [((1,6), None); ((2,6), None); ((3,6), None); ((4,6), None); ((5,6), None); ((6,6), None);      ((7,6), None);
                   ((1,5), None); ((2,5), None); ((3,5), None); ((4,5), None); ((5,5), None); ((6,5), None);      ((7,5), None);
                   ((1,4), None); ((2,4), None); ((3,4), None); ((4,4), None); ((5,4), None); ((6,4), None);      ((7,4), None);
                   ((1,3), None); ((2,3), None); ((3,3), None); ((4,3), None); ((5,3), None); ((6,3), None);      ((7,3), None);
                   ((1,2), None); ((2,2), None); ((3,2), None); ((4,2), None); ((5,2), None); ((6,2), None);      ((7,2), None);
                   ((1,1), None); ((2,1), None); ((3,1), None); ((4,1), None); ((5,1), None); ((6,1), Some Blue); ((7,1), None)]

  let two_pieces = [((1,6), None); ((2,6), None); ((3,6), None);      ((4,6), None);     ((5,6), None); ((6,6), None); ((7,6), None);
                    ((1,5), None); ((2,5), None); ((3,5), None);      ((4,5), None);     ((5,5), None); ((6,5), None); ((7,5), None);
                    ((1,4), None); ((2,4), None); ((3,4), None);      ((4,4), None);     ((5,4), None); ((6,4), None); ((7,4), None);
                    ((1,3), None); ((2,3), None); ((3,3), None);      ((4,3), None);     ((5,3), None); ((6,3), None); ((7,3), None);
                    ((1,2), None); ((2,2), None); ((3,2), None);      ((4,2), None);     ((5,2), None); ((6,2), None); ((7,2), None);
                    ((1,1), None); ((2,1), None); ((3,1), Some Blue); ((4,1), Some Red); ((5,1), None); ((6,1), None); ((7,1), None)]

  let three_pieces = [((1,6), None);      ((2,6), None);     ((3,6), None);      ((4,6), None); ((5,6), None); ((6,6), None); ((7,6), None);
                      ((1,5), None);      ((2,5), None);     ((3,5), None);      ((4,5), None); ((5,5), None); ((6,5), None); ((7,5), None);
                      ((1,4), None);      ((2,4), None);     ((3,4), None);      ((4,4), None); ((5,4), None); ((6,4), None); ((7,4), None);
                      ((1,3), None);      ((2,3), None);     ((3,3), None);      ((4,3), None); ((5,3), None); ((6,3), None); ((7,3), None);
                      ((1,2), None);      ((2,2), None);     ((3,2), None);      ((4,2), None); ((5,2), None); ((6,2), None); ((7,2), None);
                      ((1,1), Some Blue); ((2,1), Some Red); ((3,1), Some Blue); ((4,1), None); ((5,1), None); ((6,1), None); ((7,1), None)]

  let state_blue_3 = 
    State.make_state blue_3 Red (0, 0, 0) [2; 1; 2; 1; 3; 1; 7] []

  let state_blue_3_blue_turn = 
    State.make_state blue_3 Blue (0, 0, 0) [2; 1; 2; 1; 3; 1; 7] []

  let state_blue_pot = 
    State.make_state blue_diag_pot Red (0, 0, 0) 
      [2; 3; 4; 5; 4; 6; 5; 6; 7; 7; 6] []

  let state_blue_pot_2 = 
    State.make_state blue_diag_pot_2 Red (0, 0, 0) 
      [4; 4; 4; 4; 4; 4; 5; 3; 3; 3; 5; 1] []

  let state_red_3 = 
    State.make_state red_3 Red (3, 2, 1) 
      [2; 3; 4; 5; 4; 6; 5; 6; 7; 7; 6; 1; 7] []

  let state_red_3_blue_turn = 
    State.make_state red_3 Blue (3, 2, 1) 
      [2; 3; 4; 5; 4; 6; 5; 6; 7; 7; 6; 1; 7] []

  let state_red_diag_win = 
    State.make_state red_diag_win Blue (0, 0, 0) 
      [] []

  let state_blue_horiz_win = 
    State.make_state blue_horiz_win Red (0, 0, 0) 
      [] []

  let state_tie_board = 
    State.make_state full_board_tie Red (0, 0, 0) 
      [] []

  let expected_init_state =
    State.make_state empty_board Blue (0,0,0) 
      [] []

  let init_state_updated =
    State.make_state empty_updated Red (0,0,0)
      [3] []

  let state_one_piece = 
    State.make_state one_piece Red (0, 0, 0) [6] []

  let state_two_pieces = 
    State.make_state two_pieces Red (0, 0, 0) [4; 3] []

  let state_three_pieces = 
    State.make_state three_pieces Red (0, 0, 0) [1; 2; 3] []
end

open TstCases

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether 
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. (Taken from a2) *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [empty_board_test name expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with [empty_board]. *)
let empty_board_test
    (name: string)
    (expected_output: State.board) : test = 
  name >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists expected_output empty_board)

(** [empty_test name expected_output] constructs an OUnit test named [name] that 
    asserts the quality of [expected_output] with [empty]. *)
let empty_test
    (name: string)
    (expected_output: State.board) : test = 
  name >:: (fun _ ->
      assert_equal expected_output empty)

(** [check_full_test name input1 expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [check_full input1]. *)
let check_full_test
    (name: string)
    (input1: State.board)
    (expected_output: bool) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (check_full input1))

(** [check_win_test name input1 input2 expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [check_win input1 input2]. *)
let check_win_test
    (name: string)
    (input1: State.board)
    (input2: State.color)
    (expected_output: bool) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (check_win input1 input2))

(** [winning_player_test name input1 expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [winning_player input1]. *)
let winning_player_test
    (name: string)
    (input1: State.t)
    (expected_output: State.color option) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (winning_player input1))

(** [color_to_string_test name input1 expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [color_to_string input1]. *)
let color_to_string_test
    (name: string)
    (input1: State.color)
    (expected_output: string) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (color_to_string input1))

(** [other_color_test name input1 expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [other_color input1]. *)
let other_color_test
    (name: string)
    (input1: State.color)
    (expected_output: State.color) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (other_color input1))

(** [drop_height_test name input1 input2 expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [drop_height input1 input2]. *)
let drop_height_test
    (name: string)
    (input1: int)
    (input2: State.board)
    (expected_output: int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (drop_height input1 input2))

(** [update_test name input1 input2 input3 input4 expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [update input1 input2 input3 input4]. *)
let update_test
    (name: string)
    (input1: int)
    (input2: int)
    (input3: State.color)
    (input4: State.board)
    (expected_output: State.board) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (update input1 input2 input3 input4))

(** [cpu_move_med_test name input1 expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [cpu_move_med input1]. *)
let cpu_move_med_test
    (name: string)
    (input1: State.t)
    (expected_output: int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (let (c, _) = cpu_move_med input1 in c))

(** [cpu_move_med_test_notequals name input1 wrong_output] constructs an OUnit 
    test named [name] that asserts that [wrong_output] does not equal 
    [cpu_move_med input1]. *)
let cpu_move_med_test_notequals
    (name: string)
    (input1: State.t)
    (wrong_output: int) : test = 
  name >:: (fun _ -> 
      assert_equal false (let (c, _) = cpu_move_med input1 in c = wrong_output))

(** [cpu_move_easy_test name input1 expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [cpu_move_easy input1]. *)
let cpu_move_easy_test
    (name: string)
    (input1: State.t)
    (expected_output: int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (let (c, _) = cpu_move_easy input1 in c))

(** [cpu_move_hard_test name input1 expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [cpu_move_hard input1]. *)
let cpu_move_hard_test
    (name: string)
    (input1: State.t)
    (expected_output: int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (let (c, _) = cpu_move_hard input1 in c))

(** [cpu_move_hard_test_notequals name input1 wrong_output] constructs an OUnit 
    test named [name] that asserts that [wrong_output] does not equal 
    [cpu_move_hard input1]. *)
let cpu_move_hard_test_notequals
    (name: string)
    (input1: State.t)
    (wrong_output: int) : test = 
  name >:: (fun _ -> 
      assert_equal false (let (c, _) = cpu_move_hard input1 in c = wrong_output))

(** [set_turn_test name input1 input2 expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [set_turn input1 input2]. *)
let set_turn_test
    (name: string)
    (input1: State.t)
    (input2: State.color)
    (expected_output: State.t) : test =
  name >:: (fun _ -> assert_equal expected_output (set_turn input1 input2))

(** [init_state_test name expected_output] constructs an OUnit test named [name] 
    that asserts the quality of [expected_output] with [init_state]. *)
let init_state_test
    (name: string)
    (expected_output: State.t) : test =
  name >:: (fun _ -> assert_equal expected_output init_state)

(** [board_test name input1 expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with [board input1]. *)
let board_test
    (name: string)
    (input1: State.t)
    (expected_output: State.board) : test =
  name >:: (fun _ -> assert_equal expected_output (board input1))

(** [turn_test name input1 expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with [turn input1]. *)
let turn_test
    (name: string)
    (input1: State.t)
    (expected_output: State.color) : test =
  name >:: (fun _ -> assert_equal expected_output (turn input1))

(** [moves_test name input1 expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with [moves input1]. *)
let moves_test
    (name: string)
    (input1: State.t)
    (expected_output: State.moves_list) : test =
  name >:: (fun _ -> assert_equal expected_output (moves input1))

(** [wins_test name input1 expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with [wins input1]. *)
let wins_test
    (name: string)
    (input1: State.t)
    (expected_output: State.num_wins) : test =
  name >:: (fun _ -> assert_equal expected_output (wins input1))

(** [red_wins_test name input1 expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [red_wins input1]. *)
let red_wins_test
    (name: string)
    (input1: State.t)
    (expected_output: int) : test =
  name >:: (fun _ -> assert_equal expected_output (red_wins input1))

(** [blue_wins_test name input1 expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [blue_wins input1]. *)
let blue_wins_test
    (name: string)
    (input1: State.t)
    (expected_output: int) : test =
  name >:: (fun _ -> assert_equal expected_output (blue_wins input1))

(** [bum_ties_test name input1 expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [num_ties input1]. *)
let num_ties_test
    (name: string)
    (input1: State.t)
    (expected_output: int) : test =
  name >:: (fun _ -> assert_equal expected_output (num_ties input1))

(* Test Suites *)

let empty_tests = 
  [
    empty_test "empty should return an empty list" [];
    empty_board_test "empty_board should return an appropriate empty board."
      man_empty_board;
  ]

let win_tests =
  [
    check_win_test 
      "Red piece won in the board with the red diagonal win, red_diag_win." 
      red_diag_win Red true;
    check_win_test 
      "Blue did not win in the board with the red diagonal win, red_diag_win." 
      red_diag_win Blue false;
    check_win_test 
      "Blue won in the board with the blue horizontal win, blue_horiz_win." 
      blue_horiz_win Blue true;
    check_win_test 
      "Red didn't win in the board with the blue horizontal win, blue_horiz_win" 
      blue_horiz_win Red false;
    check_win_test "Red did not win in the tie board." 
      full_board_tie Red false;
    check_win_test "Blue did not win in the tie board." 
      full_board_tie Blue false;
    winning_player_test 
      "Winning player is Some Red for the state with board red_diag_win."
      state_red_diag_win (Some Red);
    winning_player_test 
      "Winning player is Some Blue for the state with board blue_horiz_win."
      state_blue_horiz_win (Some Blue);
    winning_player_test 
      "Winning player is None for the state with board red_3." state_red_3 None;
    winning_player_test 
      "Winning player is None for the state with the tie board." 
      state_tie_board None;
  ]

let check_full_tests =
  [
    check_full_test "Red diagonal win board should return false." 
      red_diag_win false;
    check_full_test "Blue horizontal win board should return false." 
      blue_horiz_win false;
    check_full_test "Tie game full board should return true." 
      full_board_tie true;
  ]

let color_tests =
  [
    color_to_string_test "Blue should return 'Blue'." Blue "Blue";
    color_to_string_test "Red should return 'Red'." Red "Red";
    other_color_test "The opposite of 'Red' is 'Blue'." Red Blue;
    other_color_test "The opposite of 'Blue' is 'Red'." Blue Red;
  ]

let drop_height_tests =
  [
    drop_height_test "Empty board has a drop height of 1 in column 1." 
      1 State.empty 1;
    drop_height_test "Empty board has a drop height of 1 in column 7." 
      7 State.empty 1;
    drop_height_test "Red diagonal win board's drop height at column 1 is 3." 
      1 red_diag_win 3;
    drop_height_test "Red diagonal win board's drop height at column 4 is 6." 
      4 red_diag_win 6;
    drop_height_test "Blue horizontal win board's drop height at column 3 is 5." 
      3 blue_horiz_win 5;
    drop_height_test "Full board's drop height should be 7 at any column." 
      4 full_board_tie 7;
  ]

let board_update_tests = 
  [
    update_test 
      "Blue horizontal win board with a blue piece inserted into (1,3) 
    should return appropriate board." 
      1 3 Blue blue_horiz_win blue_horiz_win_updated;
    update_test 
      "Empty board with a blue piece inserted into (3,1) should return
       appropriate board." 
      3 1 Blue man_empty_board empty_updated;
  ]

let move_tests = 
  [
    cpu_move_med_test 
      "Medium AI should play column 1 to block blue win in board blue_3" 
      state_blue_3 1;
    cpu_move_med_test 
      "Medium AI should play column 7 to win diagonally in board red_3" 
      state_red_3 7;
    cpu_move_med_test 
      "Medium AI should play column 1 to win vertically in board blue_3" 
      state_blue_3 1;
    cpu_move_med_test_notequals 
      "Medium AI should not play column 7 as it would trigger blue win" 
      state_blue_pot 7;
    cpu_move_med_test_notequals 
      "Medium AI should not play column 2 as it would trigger blue win" 
      state_blue_pot_2 2;
    cpu_move_easy_test 
      "Easy AI should play column 1 to block blue win in board blue_3" 
      state_blue_3 1;
    cpu_move_easy_test 
      "Easy AI should play column 7 to win diagonally in board red_3" 
      state_red_3 7;
    cpu_move_easy_test 
      "Easy AI should play column 1 to win vertically in board blue_3" 
      state_blue_3 1;
    cpu_move_hard_test 
      "Hard AI should play column 1 to block blue win in board blue_3" 
      state_blue_3 1;
    cpu_move_hard_test 
      "Hard AI should play column 7 to win diagonally in board red_3" 
      state_red_3 7;
    cpu_move_hard_test 
      "Hard AI should play column 1 to win vertically in board blue_3" 
      state_blue_3 1;
    cpu_move_hard_test_notequals 
      "Hard AI should not play column 7 as it would trigger blue win" 
      state_blue_pot 7;
    cpu_move_hard_test_notequals 
      "Hard AI should not play column 2 as it would trigger blue win" 
      state_blue_pot_2 2;
    cpu_move_hard_test 
      "Hard AI should play 6 when the moves 4 and 3 have been taken"
      state_two_pieces 6;
    cpu_move_hard_test
      "Hard AI should play 5 when there is only a piece in column 6"
      state_one_piece 5; 
    cpu_move_hard_test
      "Hard AI should play 3 when the moves 1, 2, 3 have been taken"
      state_three_pieces 3; 
  ]

let state_tests = 
  [
    init_state_test "init_state should match appropriate state." 
      expected_init_state;
    turn_test "In state_red_3, it is Red's turn." state_red_3 Red;
    board_test "state_red_3 has board red_3" state_red_3 red_3;
    moves_test "state_red_3 has the appropriate moves list" state_red_3
      [2; 3; 4; 5; 4; 6; 5; 6; 7; 7; 6; 1; 7];
    wins_test "state_red_3 has the 3 red wins, 2 blue wins, 1 tie." state_red_3
      (3, 2, 1);
    red_wins_test "state_red_3 has the 3 red wins." state_red_3 3;
    blue_wins_test "state_red_3 has the 2 red wins." state_red_3 2;
    num_ties_test "state_red_3 has the 1 tie." state_red_3 1;
    set_turn_test "Set state_blue_3 to color blue" 
      state_blue_3 Blue state_blue_3_blue_turn;
    set_turn_test "Set state_red_3 to color blue" 
      state_red_3 Blue state_red_3_blue_turn;
  ]

let suite =
  "test suite for connect four"  >::: List.flatten [
    empty_tests;
    win_tests;
    check_full_tests;
    color_tests;
    drop_height_tests;
    board_update_tests;
    move_tests;
    state_tests
  ]

let _ = run_test_tt_main suite
