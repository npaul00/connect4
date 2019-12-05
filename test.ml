open OUnit2
open State
open Command

let empty_board_test
    (name: string)
    (expected_output: State.board) : test =
  name >:: (fun _ -> assert_equal expected_output (empty_board))

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

let check_win_tests =
  [
    check_win_test "Red diagonal win should return true for appropriate board and color Red." 
      State.red_diag_win Red true;
    check_win_test "Red diagonal win should return false for appropriate board and color Blue." 
      State.red_diag_win Blue false;
    check_win_test "Blue horizontal win should return true for appropriate board and color Blue." 
      State.blue_horiz_win Blue true;
    check_win_test "Blue horizontal win should return false for appropriate board and color Red." 
      State.blue_horiz_win Red false;
    check_win_test "Tie board should return false for color Red." 
      State.full_board_tie Red false;
    check_win_test "Tie board should return false for color Blue." 
      State.full_board_tie Blue false;
  ]

let board_tests = 
  [
    empty_board_test "Empty board test" State.man_empty_board;
  ]

let check_full_tests =
  [
    check_full_test "Red diagonal win board should return false." 
      State.red_diag_win false;
    check_full_test "Blue horizontal win board should return false." 
      State.blue_horiz_win false;
    check_full_test "Tie game full board should return true." 
      State.full_board_tie true;
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
      1 State.red_diag_win 3;
    drop_height_test "Red diagonal win board's drop height at column 4 is 6." 
      4 State.red_diag_win 6;
    drop_height_test "Blue horizontal win board's drop height at column 3 is 5." 
      3 State.blue_horiz_win 5;
    drop_height_test "Full board's drop height should be 7 at any column." 
      4 State.full_board_tie 7;
  ]

let update_tests = 
  [
    update_test 
      "Blue horizontal win board with a blue piece inserted into (1,3) 
    should return appropriate board." 
      1 3 Blue State.blue_horiz_win State.blue_horiz_win_updated;
    (* update_test 
       "Empty board with a blue piece inserted into (3,1) should return
       appropriate board." 
       3 1 Blue State.empty_board State.empty_updated;   *)
  ]

let suite =
  "test suite for connect four"  >::: List.flatten [
    check_win_tests;
    check_full_tests;
    color_tests;
    drop_height_tests;
    update_tests;
    (* board_tests  *)
  ]

let _ = run_test_tt_main suite
