open OUnit2
open State
open Player
open Game
open Input

(*
 * STATE
 *)

(** [p1] is a sample initial player. *)
let p1 = Player.init_player "Whitney"

(** [st_init] is a sample initial state with one player. *)
let st_init = 
  State.init_state ["Whitney"]

(** [(st_job), (p1_job)] is a sample state and player
    after a salary has been assigned. *)
let (st_job, p1_job) =
  let (x,y,_,_) = (State.select_career_salary 
                     st_init p1 true)
  in x,y

(** [p1_bal5000] is a sample player with 5000 added to balance. *)
let p1_bal5000 = 
  Player.update_balance p1 5000 true

(** [p1_bal10000] is
    [p1_bal5000] with 5000 added to balance. *)
let p1_bal10000 = 
  Player.update_balance p1_bal5000 5000 true  

(** [(st_move1, p1_move1] is the state and player
    after moving 1 space on the board. *)
let (st_move1, p1_move1) = 
  let (x,y,_) = (State.move_player st_init p1 1) in x, y

(** [st_move2] is the state
    after moving 2 spaces on the board. *)
let st_move2 = 
  let (x,_,_) = (State.move_player st_move1 p1 2) in x

(** [st_move2] is the state
    after moving 2 spaces on the board. *)
let st_move2b = 
  let (x,_,_) = (State.move_player st_move1 p1 2) in x

(** [st_move3] is the state
    after moving 3 spaces on the board. *)
let st_move3 =
  let (x,_,_) = (State.move_player st_init p1 3) in x

(** [st_move3] is the player
    after moving 3 spaces on the board. *)
let p1_move3 =   
  let (_,x,_) = (State.move_player st_init p1 3) in x

(** [player2] is a sample initial player. *)
let p2 = Player.init_player "Marcus"

(** [player2] is a sample initial state with two players. *)
let st_init_2 = 
  State.init_state ["Whitney"; "Marcus"]

(** [st_p1_job] is [st_init_2] after assigning salary
    to the first player. *)
let st_p1_job =
  let (x,_,_,_) = (State.select_career_salary st_init_2 p1 true)
  in x

(** [st_p2_job] is [st_init_2] after assigning salary
    to both players. *)
let st_p2_job =
  let (x,_,_,_) = (State.select_career_salary st_p1_job p2 true)
  in x

(** [(p1_c1, p1_c2)] are the
    careers that [p1]
    can choose from in [st_p1_job]. *)
let (p1_c1, p1_c2) =
  let (_,_,x,y) = (State.select_career_salary st_p1_job p1 
                     true)
  in x, y

(** [(p2_c1, p2_c2)] are the
    careers that the [p2]
    can choose from in [st_p2_job]. *)
let (p2_c1, p2_c2) =
  let (_,_,x,y) = (State.select_career_salary st_p2_job p2
                     true)
  in x, y

(** [p1_job_set] is [p1] after a career has been assigned. *)
let p1_job_set =
  snd (State.set_career st_p1_job p1 p1_c1)

(** [player_twoafterjob2] is [p2] after a career has been assigned. *)
let p2_job_set =
  snd (State.set_career st_p2_job p2 p2_c1)

(** [(st_house, p1_with_house)] is
    [st_init, p1] after a house has been assigned. *)
let (st_house, p1_with_house) = 
  (State.set_house st_init p1)

(** [p1_house] is [Some] house of [p1], or [None]. *)
let p1_house =
  Player.get_house p1_with_house

(** [make_change_state_test name state player expected_state] constructs an 
    OUnit test named [name] that asserts the quality of [expected_state]
    with [State.set_career_salary state player]. *)
let make_set_career_salary_state_test 
    (name : string)
    (state : State.state)
    (player : Player.player)
    (expected_state : State.state) : test = 
  name >:: (fun _ -> (assert_equal (expected_state)
                        (let (x,_,_,_)= (State.select_career_salary state 
                                           player true) in x)))

(** [make_move_player_test name state player num expected_state] constructs an 
    OUnit test named [name] that asserts the quality of [expected_state]
    with [State.move_player state player num]. *)
let make_move_player_test
    (name : string)
    (state : State.state)
    (player : Player.player)
    (num : int)
    (expected_state : State.state) : test = 
  name >:: (fun _ -> (assert_equal (expected_state) 
                        (let (x,_,_)= (State.move_player 
                                         st_init
                                         p1 num) in x)))

(** [make_get_nonret_player_list_test name state expected_state] constructs an 
    OUnit test named [name] that asserts the quality of [expected_state]
    with [State.get_nonret_player_list state]. *)
let make_get_nonret_player_list_test
    (name : string)
    (state : State.state)
    (expected_list : Player.player list) : test = 
  name >:: (fun _ -> (assert_equal expected_list 
                        (State.get_nonret_player_list state)))     

(** [make_get_player_list_test name state pname expected_state] constructs an 
    OUnit test named [name] that asserts the quality of [expected_player]
    with [State.get_player state pname]. *)
let make_get_player_test
    (name : string)
    (state : State.state)
    (pname : string)
    (expected_player : Player.player) : test = 
  name >:: (fun _ -> (assert_equal expected_player 
                        (State.get_player state pname)))  

(** [make_set_career_test name state p c_accept c_reject expected_state]
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_player] with [State.set_career state p c_accept c_reject]. *)
let make_set_career_test
    (name : string)
    (state : State.state)
    (p : Player.player)
    (c_accept : Game.career)
    (c_reject : Game.career)
    (expected_player : Player.player) : test = 
  name >:: (fun _ -> (assert_equal expected_player 
                        (snd (State.set_career state p c_accept))))

(** [make_update_balance_test name state src tgt_string amt add expected_player]
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_player] with [State.update_balance state src tgt_string amt
    add]. *)
let make_update_balance_test
    (name : string)
    (state : State.state)
    (src : Player.player)
    (tgt_string : string)
    (amt : int)
    (add: bool)
    (expected_player : Player.player) : test = 
  name >:: (fun _ -> (assert_equal expected_player 
                        (snd (State.update_balance state 
                                src tgt_string amt add))))

(** [make_set_house_test name state player expected_state]
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_player] with [State.set_house state player]. *)
let make_set_house_test
    (name : string)
    (state : State.state)
    (player : Player.player)
    (expected_player : Player.player) : test = 
  name >:: (fun _ -> (assert_equal expected_player 
                        (snd (State.set_house state player))))





let state_tests : (OUnit2.test list) =
  [ 
    make_set_career_salary_state_test "give job to init_state player"
      st_init p1 st_job;
    make_set_career_salary_state_test "give job to two init_state players 1"
      st_init_2 p1 st_p1_job;
    make_set_career_salary_state_test "give job to two init_state players 2"
      st_p1_job p2 st_p2_job;    

    make_move_player_test "move one space" st_init p1 
      1 st_move1;  
    make_move_player_test "move two spaces after moving one" 
      st_move1 p1 2 st_move2;  
    make_move_player_test "move two spaces after moving one again"
      st_move1 p1 2 st_move2b;
    make_move_player_test "moved three spaces from beginning again"
      st_init p1 3 st_move3;  

    make_get_nonret_player_list_test "get nonretired player when there is one 
    nonretired player" st_job [p1_job];


    make_get_player_test "get one player when there is one player"
      st_job "Whitney" p1_job;
    make_get_player_test "get player2 when there are two players"
      st_init_2 "Marcus" p2;  
    make_get_player_test "get player1 when there are two players"
      st_init_2 "Whitney" p1;  

    make_set_career_test "set a career for one player when there
    are two players" st_p1_job p1 p1_c1 
      p1_c2 p1_job_set;  
    make_set_career_test "set a career for player 2 when there are two 
    players" st_p2_job p2 p2_c1 
      p2_c2 p2_job_set;  

    make_update_balance_test "update balance if one player gives money to the
    bank" st_job p1_bal10000 "bank" 5000 false 
      p1_bal5000;

    make_set_house_test "set house for one player" st_init
      p1 p1_with_house;


  ]

(* 
 * PLAYER
 *)  

(** [make_move_player_test name player num expected_player] constructs an 
    OUnit test named [name] that asserts the quality of [expected_player]
    with [Player.move player num]. *)
let make_move_test
    (name : string)
    (player : Player.player)
    (num : int)
    (expected_player : Player.player) : test = 
  name >:: (fun _ -> (assert_equal (expected_player) 
                        (Player.move p1 num)))

(** [make_update_balance_test name player amt add expected_player] constructs an 
    OUnit test named [name] that asserts the quality of [expected_player]
    with [Player.update_balance player amt add]. *)
let make_update_balance_test
    (name : string)
    (player : Player.player)
    (amt : int)
    (add: bool)
    (expected_player : Player.player) : test = 
  name >:: (fun _ -> (assert_equal (expected_player) 
                        (Player.update_balance player amt add)))     

let make_compare_fields_test
    (name : string)
    (player : Player.player)
    (player2 : Player.player)
    (func : 'a -> 'b) : test = 
  name >:: (fun _ -> (assert_equal (func player) (func player2)))



let player_test : (OUnit2.test list) = 
  [
    make_move_test "move one player one space" p1 1 
      p1_move1;
    make_move_test "move one player 3 spaces" p1 3
      p1_move3; 

    make_update_balance_test "update balance from 5,000 to 10,000" 
      p1_bal5000 5000 true p1_bal10000;
    make_update_balance_test "update 10,000 to 5,000"
      p1_bal10000 5000 false p1_bal5000;  

    make_compare_fields_test "compare balance of initial player" p1
      p2 Player.get_balance;
    make_compare_fields_test "compare balance of same player" 
      p1_bal10000 p1_bal10000 Player.get_balance; 
    make_compare_fields_test "compare name of the same player"
      p1 p1 Player.get_name;   
    make_compare_fields_test "compare car insurance" p1 p2 
      Player.get_car_insure_bool;  
    make_compare_fields_test "compare college" p1 p2 
      Player.get_college;
    make_compare_fields_test "compare home insurance" p1 
      p2 Player.get_home_insure_bool;  
    make_compare_fields_test "compare career of initial players" p1
      p2 Player.get_career;  
    make_compare_fields_test "careers of different players are not equal"
      p1_job_set p2_job_set Player.get_career;  
    make_compare_fields_test "compare house" p1 p2
      Player.get_house;
    make_compare_fields_test "compare insurance" p1 p2 
      Player.get_insurance; 
    make_compare_fields_test "compare loans" p1 p2
      Player.get_loans;    
    make_compare_fields_test "compare player position" p1_move1  
      p1_move1 Player.get_position;
    make_compare_fields_test "compare retired" p1_job_set 
      p2_job_set Player.get_retired;  
    make_compare_fields_test "compare salary of two players after job" 
      p1_job_set p2_job_set Player.get_salary;  
    make_compare_fields_test "compare salary amount" p1_job_set 
      p1_job_set Player.get_salary_amt;  
    make_compare_fields_test "compare square" p1_move3
      p1_move3 Player.get_square;  
    make_compare_fields_test "compare stocks" p1 p2
      Player.get_stocks;  
    make_compare_fields_test "compare tiles" p1 p2
      Player.get_tiles;  

  ]

(*
 * INPUT
 *)


(** [make_parse_clean_test name strls expected_strls] constructs an 
    OUnit test named [name] that asserts the quality of [expected_strls]
    with [Input.clean strls]. *)
let make_parse_clean_test
    (name : string)
    (strls : string list)
    (expected_strls : string list) : test =
  name >:: (fun _ -> (assert_equal (expected_strls) 
                        (Input.clean strls)))

let make_parse_input_test
    (name : string)
    (cmd : string)
    (expected_parsed_input : Input.input) : test =
  name >:: (fun _ -> (assert_equal (expected_parsed_input) 
                        (Input.parse cmd)))

(** [parse_raise_exn cmd] is a function that applies [parse] to [cmd]. *)
let parse_raise_exn (cmd : string) = fun () -> Input.parse cmd

let make_parse_input_exn_test
    (name : string)
    (cmd : string)
    (expected_exn : exn) : test =
  name >:: (fun _ -> (assert_raises (expected_exn) 
                        (parse_raise_exn cmd)))


let input_tests : (OUnit2.test list) = [
  make_parse_clean_test "Input.clean, empty string list" [""] [];
  make_parse_clean_test "Input.clean, simple dirty string" [" a "] ["a"];
  make_parse_clean_test "Input.clean, multiple dirty strings"
    [" b"; "c "; "clean"] ["b"; "c"; "clean"];
  make_parse_clean_test "Input.clean, multiple dirtier strings"
    [" d    "; ""; "   e "; "give"; ""] ["d"; "e"; "give"];
  make_parse_clean_test "Input.clean, sample game input"
    [""; "give"; ""; "Whitney"; "5000 "] ["give"; "Whitney"; "5000"];

  make_parse_input_test "Input.parse, view global"
    "view global" (View("global"));
  make_parse_input_test "Input.parse, view local"
    "view next" (View("next"));
  make_parse_input_test "Input.parse, view Whitney"
    "view Whitney" (View("Whitney"));
  make_parse_input_test "Input.parse, view g123"
    "view g123     " (View("g123"));
  make_parse_input_test "Input.parse, spin" "spin" (Spin);
  make_parse_input_test "Input.parse, pay whitney 5000"
    "pay whitney 5000" (Pay("whitney", 5000));
  make_parse_input_test "Input.parse, pay qt314 10000000"
    "pay qt314 10000000" (Pay("qt314", 10000000));
  make_parse_input_test "Input.parse, pay sm1 0"
    "pay sm1 0" (Pay("sm1", 0));  
  make_parse_input_test "Input.parse, get whitney 5000"
    "take whitney 5000" (Take("whitney", 5000));
  make_parse_input_test "Input.parse, get qt314 10000000"
    "take qt314 10000000" (Take("qt314", 10000000));
  make_parse_input_test "Input.parse, get sm1 0"
    "take sm1 0" (Take("sm1", 0));
  make_parse_input_test "Input.parse, help" "help" Help;
  make_parse_input_test "Input.parse, done" "done" Done;
  make_parse_input_test "Input.parse, exit" "exit" Exit;

  make_parse_input_exn_test "Input.parse, exn 1" "blah" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 2" "take give sm1 0" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 3, view" "view" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 4, spin" "spin wheel" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 5, pay" "pay" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 6, pay" "pay whitney" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 7, pay" "pay 500" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 8, get" "take" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 9, get" "take breaks" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 10, get" "take 10" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 11, help" "help me" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 12, done" "done for" Bad_input;
  make_parse_input_exn_test "Input.parse, exn 13, exit" "exit suffering"
    Bad_input;

  make_parse_input_test "Input.parse, insure home" "insure home" 
    (Insure ("home"));

  make_parse_input_test "Input.parse insure car" "insure car"
    (Insure("car"));
  make_parse_input_exn_test "Input.parse, insure exn1" "insure" Bad_input;  
  make_parse_input_exn_test "Input.parse, insure lots exn2" 
    "insure ca ha what what" Bad_input;
  make_parse_input_test "Input.parse, invest" "invest 1000" (Invest(1000));
  make_parse_input_test "Input.parse, invest" "invest 4000000" 
    (Invest(4000000));
  make_parse_input_exn_test "Input.parse, invest word exn1" "invest word" 
    Bad_input;
  make_parse_input_exn_test "Input.parse, invest exn2" "invest" Bad_input;
  make_parse_input_exn_test "Input.parse, invest lots exn3" "invest 1 2 3"
    Bad_input;

]

let suite = "test suite for A6" >::: List.flatten [
    state_tests;
    input_tests;
    player_test;
  ]

let _ = run_test_tt_main suite