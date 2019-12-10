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
    State.make_state red_3 Red (0, 0, 0) 
      [2; 3; 4; 5; 4; 6; 5; 6; 7; 7; 6; 1; 7] []

  let state_red_3_blue_turn = 
    State.make_state red_3 Blue (0, 0, 0) 
      [2; 3; 4; 5; 4; 6; 5; 6; 7; 7; 6; 1; 7] []

end

open TstCases

let check_full_test
    (name: string)
    (input1: State.board)
    (expected_output: bool) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (check_full input1))

let check_win_test
    (name: string)
    (input1: State.board)
    (input2: State.color)
    (expected_output: bool) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (check_win input1 input2))

let color_to_string_test
    (name: string)
    (input1: State.color)
    (expected_output: string) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (color_to_string input1))

let other_color_test
    (name: string)
    (input1: State.color)
    (expected_output: State.color) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (other_color input1))

let drop_height_test
    (name: string)
    (input1: int)
    (input2: State.board)
    (expected_output: int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (drop_height input1 input2))

let update_test
    (name: string)
    (input1: int)
    (input2: int)
    (input3: State.color)
    (input4: State.board)
    (expected_output: State.board) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (update input1 input2 input3 input4))

let cpu_move_test
    (name: string)
    (input1: State.t)
    (expected_output: int) : test = 
  name >:: (fun _ -> assert_equal expected_output (let (c, _) = cpu_move_med input1 in c))

let cpu_move_test_notequals
    (name: string)
    (input1: State.t)
    (wrong_output: int) : test = 
  name >:: (fun _ -> assert_equal false (let (c, _) = cpu_move_med input1 in c = wrong_output))

let cpu_move_hard_test_notequals
    (name: string)
    (input1: State.t)
    (wrong_output: int) : test = 
  name >:: (fun _ -> assert_equal false (let (c, _) = cpu_move_hard input1 in c = wrong_output))

let cpu_move_easy_test
    (name: string)
    (input1: State.t)
    (expected_output: int) : test = 
  name >:: (fun _ -> assert_equal expected_output (let (c, _) = cpu_move_easy input1 in c))

let cpu_move_hard_test
    (name: string)
    (input1: State.t)
    (expected_output: int) : test = 
  name >:: (fun _ -> assert_equal expected_output (let (c, _) = cpu_move_hard input1 in c))

let set_turn_test
    (name: string)
    (input1: State.t)
    (input2: State.color)
    (expected_output: State.t) : test =
  name >:: (fun _ -> assert_equal expected_output (set_turn input1 input2))

let check_win_tests =
  [
    check_win_test "Red diagonal win should return true for appropriate board and color Red." 
      red_diag_win Red true;
    check_win_test "Red diagonal win should return false for appropriate board and color Blue." 
      red_diag_win Blue false;
    check_win_test "Blue horizontal win should return true for appropriate board and color Blue." 
      blue_horiz_win Blue true;
    check_win_test "Blue horizontal win should return false for appropriate board and color Red." 
      blue_horiz_win Red false;
    check_win_test "Tie board should return false for color Red." 
      full_board_tie Red false;
    check_win_test "Tie board should return false for color Blue." 
      full_board_tie Blue false;
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

let update_tests = 
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
    cpu_move_test 
      "Medium AI should play column 1 to block blue win in board blue_3" 
      state_blue_3 1;
    cpu_move_test 
      "Medium AI should play column 7 to win diagonally in board red_3" 
      state_red_3 7;
    cpu_move_test 
      "Medium AI should play column 1 to win vertically in board blue_3" 
      state_blue_3 1;
    cpu_move_test_notequals 
      "Medium AI should not play column 7 as it would trigger blue win" 
      state_blue_pot 7;
    cpu_move_test_notequals 
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
  ]

let set_turn_tests = 
  [
    set_turn_test "Set state_blue_3 to color blue" 
      state_blue_3 Blue state_blue_3_blue_turn;
    set_turn_test "Set state_red_3 to color blue" 
      state_red_3 Blue state_red_3_blue_turn;
  ]

let suite =
  "test suite for connect four"  >::: List.flatten [
    check_win_tests;
    check_full_tests;
    color_tests;
    drop_height_tests;
    update_tests;
    move_tests;
    set_turn_tests
  ]

let _ = run_test_tt_main suite
