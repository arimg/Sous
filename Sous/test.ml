open OUnit2
open Read
open Command
open State
open Pantry
open Cookbook

(* The following helper function is from the test file provided to us in A2 *)
(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(********************************************************************
   End helper functions.
 ********************************************************************)

(** Read Tests*)

let make_get_step_test
    (name : string)
    (step_num : int)
    (rd : Read.t)
    (expected_output : Read.instruction) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_step step_num rd))

let make_get_step_ingredients_test
    (name : string)
    (step_num : int)
    (rd : Read.t)
    (expected_output : Read.ingredient list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_step_ingredients step_num rd))

let make_get_name_test
    (name : string)
    (assoc_lst : Read.t)
    (expected_output : Read.name) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_name assoc_lst))

let make_increment_test
    (name : string)
    (step_num : Read.step_num)
    (expected_output : Read.step_num) : test =
  name >:: (fun _ ->
      assert_equal expected_output (increment step_num))

let make_decrement_test
    (name : string)
    (step_num : Read.step_num)
    (expected_output : Read.step_num) : test =
  name >:: (fun _ ->
      assert_equal expected_output (decrement step_num))

let make_get_cuisine_test
    (name : string)
    (assoc_lst : Read.t)
    (expected_output : Read.cuisine) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_cuisine assoc_lst))

let make_get_course_test
    (name : string)
    (assoc_lst : Read.t)
    (expected_output : Read.course) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_course assoc_lst))

let make_get_servings_test
    (name : string)
    (assoc_lst : Read.t)
    (expected_output : Read.servings) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_servings assoc_lst))

let make_check_test
    (name : string)
    (rd : Read.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Read.check rd))

(* From our test file in A2 *)
let make_cmp_set_like_lists_test
    (name : string)
    (lst1 : 'a list)
    (lst2 : 'a list)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (cmp_set_like_lists lst1 lst2))

let make_check_test_error
    (name : string)
    (rd : Read.t)
    (expected_output : exn) : test =
  name >:: (fun _ ->
      assert_raises expected_output (fun () -> (Read.check rd) ))

let read_tests =
  (*Values to be used in Read tests*)
  let chocchip_file = Yojson.Basic.from_file "chocolate_chip_cookies.json" in
  let chocchip = Read.from_json chocchip_file in
  let empty_name = "empty_name.json" |> Yojson.Basic.from_file |> Read.from_json in
  let no_name = "no_name.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_name_type = "invalid_name_type.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_servings = "invalid_servings.json" |> Yojson.Basic.from_file |> Read.from_json in
  let no_servings = "no_servings.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_serving_type = "invalid_serving_type.json" |> Yojson.Basic.from_file |> Read.from_json in
  let empty_cuisine = "empty_cuisine.json" |> Yojson.Basic.from_file |> Read.from_json in
  let no_cuisine = "no_cuisine.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_cuisine_type = "invalid_cuisine_type.json" |> Yojson.Basic.from_file |> Read.from_json in
  let empty_course = "empty_course.json" |> Yojson.Basic.from_file |> Read.from_json in
  let no_course = "no_course.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_course_type = "invalid_course_type.json" |> Yojson.Basic.from_file |> Read.from_json in
  let no_step_num = "no_step_num.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_step_num_type = "invalid_step_num_type.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_step_nums = "invalid_step_nums.json" |> Yojson.Basic.from_file |> Read.from_json in
  let empty_step_instr = "empty_step_instr.json" |> Yojson.Basic.from_file |> Read.from_json in

  let no_step_instr = "no_step_instr.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_step_instr_type = "invalid_step_instr_type.json" |> Yojson.Basic.from_file |> Read.from_json in
  let no_step_ing = "no_step_ing.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_step_ing_type = "invalid_step_ing_type.json" |> Yojson.Basic.from_file |> Read.from_json in
  let empty_steps = "empty_steps.json" |> Yojson.Basic.from_file |> Read.from_json in
  let no_steps = "no_steps.json" |> Yojson.Basic.from_file |> Read.from_json in
  let invalid_steps_type = "invalid_step_type.json" |> Yojson.Basic.from_file |> Read.from_json in

  [ 
    (* [get_step step_num rd] tests *)
    make_get_step_test "get_step choc" 1 chocchip 
      "Place ½ cup melted butter in the bowl of a stand mixer fitted with the paddle attachment (or a large bowl if using a hand mixer).";

    (* [get_step_ingredients step_num rd] tests *)
    make_get_step_ingredients_test "get_step_ingredients choc" 1 chocchip 
      ["butter"];

    (* [get_key_words assoc_lst] tests *)
    make_cmp_set_like_lists_test "get_key_words choc" (get_key_words chocchip) 
      ["american"; "dessert"; "butter"; "granulated sugar"; "brown sugar";
       "egg"; "vanilla"; "baking soda"; "salt"; "flour"; "chocolate chips"]
      true;

    (** [get_name assoc_lst] tests *)
    make_get_name_test "get_name choc" chocchip "chocolate chip cookies";

    (* [increment step_num] tests *)
    make_increment_test "increment choc" 1 2;

    (* [decrement step_num] tests *)
    make_decrement_test "decrement choc" 2 1;

    (* [get_ingredients assoc_lst step acc] tests *)
    make_cmp_set_like_lists_test "get_ingredients choc" 
      (get_ingredients chocchip 1 []) 
      ["butter"; "granulated sugar"; "brown sugar"; "egg"; 
       "vanilla"; "baking soda"; "salt"; "flour";"chocolate chips"] true;

    (* [get_cuisine assoc_lst] tests *)
    make_get_cuisine_test "get_cuisine choc" chocchip "american";

    (* [get_course assoc_lst] tests *)
    make_get_course_test "get_course choc" chocchip "dessert";

    (* [get_servings assoc_lst] tests *)
    make_get_servings_test "get_servings choc" chocchip 24;

    (* [get_check rd] tests *)
    make_check_test "check choc" chocchip true;

    make_check_test_error "empty name" empty_name Read.EmptyName;
    make_check_test_error "no name" no_name Read.NoName;
    make_check_test_error "invalid name type" invalid_name_type Read.InvalidNameType;
    make_check_test_error "invalid servings" invalid_servings Read.InvalidServings;
    make_check_test_error "no servings" no_servings Read.NoServings;
    make_check_test_error "invalid serving type" invalid_serving_type Read.InvalidServingType;
    make_check_test_error "empty cuisine" empty_cuisine Read.EmptyCuisine;
    make_check_test_error "no cuisine" no_cuisine Read.NoCuisine;
    make_check_test_error "invalid cuisine type" invalid_cuisine_type Read.InvalidCuisineType;
    make_check_test_error "empty course" empty_course Read.EmptyCourse;
    make_check_test_error "no course" no_course Read.NoCourse;
    make_check_test_error "invalid course type" invalid_course_type Read.InvalidCourseType;
    make_check_test_error "no course" no_course Read.NoCourse;
    make_check_test_error "invalid step_num type" invalid_step_num_type Read.InvalidStepNumType;
    make_check_test_error "invalid step_nums" invalid_step_nums Read.InvalidStepNums;
    make_check_test_error "empty step_instr" empty_step_instr Read.EmptyStepInstr;

    make_check_test_error "no step_instr" no_step_instr Read.NoStepInstr;
    make_check_test_error "invalid step_instr type" invalid_step_instr_type Read.InvalidStepInstrType;
    make_check_test_error "no step_ing" no_step_ing Read.NoStepIng;
    make_check_test_error "invalid step_ing type" invalid_step_ing_type Read.InvalidStepIngsType;
    make_check_test_error "empty steps" empty_steps Read.EmptySteps;
    make_check_test_error "no steps" no_steps Read.NoSteps;
    make_check_test_error "invalid steps type" invalid_steps_type Read.InvalidStepsType;
  ]

(** Command Tests*)

let make_parse_test_valid
    (name : string)
    (status : State.status)
    (str : string)
    (expected_output : command) : test =
  name >:: (fun _ ->
      assert_equal expected_output (parse status str))

let make_parse_test_error
    (name : string)
    (status : State.status)
    (str : string)
    (expected_output : exn) : test =
  name >:: (fun _ ->
      assert_raises expected_output (fun () -> (parse status str) ))

let command_tests =
  [
    make_parse_test_valid "Cook valid" Browsing "cook chocolate chip cookies"
      (Cook["chocolate_chip_cookies.json"]);
    make_parse_test_valid "Next" (Cooking 1) "next" Next; 
    make_parse_test_valid "Previous" (Cooking 2) "previous" Previous;
    make_parse_test_valid "Update" Browsing "update flour, high"
      (Update["flour"; "high"]);
    make_parse_test_valid "Search" Browsing "search eggs" (Search["eggs"]);
    make_parse_test_valid "Add" Browsing "add chocolate chip cookies"
      (Add["chocolate_chip_cookies.json"]); 
    make_parse_test_valid "Check" Browsing "check egg" (Check["egg"]);
    make_parse_test_valid "Unrecommend" Browsing "unrecommend egg"
      (Unrecommend["egg"]);
    make_parse_test_valid "Rerecommend" Browsing "rerecommend egg"
      (Rerecommend["egg"]);
    make_parse_test_valid "Recommend" Browsing "recommend" Recommend;
    make_parse_test_valid "Remove1" Browsing "remove chocolate chip cookies"
      (Remove["chocolate_chip_cookies.json"]);
    make_parse_test_valid "Pantry" Browsing "pantry" Pantry;
    make_parse_test_valid "Recipes" Browsing "recipes" Recipes;
    make_parse_test_valid "Cancel" (Cooking 1) "cancel" Cancel;
    make_parse_test_valid "Exit" Browsing "exit" Exit;

    make_parse_test_error "Cook" (Cooking 1) "cook chocolate chip cookies"
      BrowsingInput;
    make_parse_test_error "Next" Browsing "next" CookingInput; 
    make_parse_test_error "Previous" Browsing "previous" CookingInput;
    make_parse_test_error "Update" (Cooking 1) "update flour, high"
      BrowsingInput;
    make_parse_test_error "Search" (Cooking 1) "search eggs" BrowsingInput;
    make_parse_test_error "Add" (Cooking 1) "add chocolate chip cookies"
      BrowsingInput; 
    make_parse_test_error "Check" (Cooking 1) "check egg" BrowsingInput;
    make_parse_test_error "Unrecommend" (Cooking 1) "unrecommend egg"
      BrowsingInput;
    make_parse_test_error "Rerecommend" (Cooking 1) "rerecommend egg"
      BrowsingInput;
    make_parse_test_error "Recommend" (Cooking 1) "recommend" BrowsingInput;
    make_parse_test_error "Remove2" (Cooking 1) "remove chocolate chip cookies"
      BrowsingInput;
    make_parse_test_error "Pantry" (Cooking 1) "pantry" BrowsingInput;
    make_parse_test_error "Recipes" (Cooking 1)"recipes" BrowsingInput;
    make_parse_test_error "Cancel" Browsing "cancel" CookingInput;

    make_parse_test_error "Empty" Browsing "" Empty;
    make_parse_test_error "Malformed" Browsing "tyfjytfjyf" Command.Malformed;
    make_parse_test_error "TooFew" Browsing "update high" 
      (TooFewInputs("update"));
    make_parse_test_error "TooMany" Browsing "cook apple pie, pizza" 
      (Command.TooManyInputs("cook"));
    make_parse_test_error "NoInputIng" Browsing "update" (NoInputIng("update"));
    make_parse_test_error "NoInputRecp" Browsing "cook" (NoInputRecp("cook"));
    make_parse_test_error "HasInput" Browsing "recommend panda"
      (HasInput("recommend"));
    make_parse_test_error "ExitWhileCooking" (Cooking 1) "exit"
      ExitWhileCooking;

  ]

(** State Tests*)

let make_search_dir_test
    (name : string)
    (name_var : string)
    (d : string)
    (expected_output : existing) : test =
  name >:: (fun _ ->
      assert_equal expected_output (search_dir name_var d))

let make_status_test
    (name : string)
    (st : State.t)
    (expected_output : status) : test =
  name >:: (fun _ ->
      assert_equal expected_output (status st))

let make_step_instr_test
    (name : string)
    (st : State.t)
    (rd : Read.t)
    (expected_output : string) : test =
  name >:: (fun _ ->
      assert_equal expected_output (step_instr st rd))

let make_add_test_cookbook
    (name : string)
    (st : State.t)
    (json_name : string)
    (expected_output : bool ) : test =
  name >:: (fun _ ->
      assert_equal expected_output (recipe_mem (cookbook (add st json_name))
                                      json_name))

let make_update_test
    (name : string)
    (st : State.t)
    (item : Pantry.key)
    (expected_output : Pantry.value) : test =
  name >:: (fun _ ->
      assert_equal expected_output (State.update st item "high"; 
                                    (Pantry.check (pantry st) item)))


let state_tests = 

  (*Values to be used in State tests*)
  let chocchip_file = Yojson.Basic.from_file "chocolate_chip_cookies.json" in
  let chocchip = Read.from_json chocchip_file in
  let new_st = init_state (New("testml")) in
  let cook_st = new_st |> cook in 
  let next_st = cook_st |> next in
  let choc_st = (add new_st "chocolate_chip_cookies.json") in 
  (* let old_st = init_state (Old("sweater")) in  *)

  [
    (* these tests pass, but once they occur, then their accuracy of old/new
       is no longer applicable, so commenting out so we can see the rest of 
       our tests*)
    (* make_search_dir_test "new" "thing" "." (New("thing"));
       make_search_dir_test "old" "sweater" "." (Old("sweater")); *)

    make_status_test "browsing" new_st Browsing;
    make_status_test "cook" (cook new_st) (Cooking 1);
    make_status_test "next" (next next_st) (Cooking 3);
    make_status_test "previous" (previous next_st) (Cooking 1);
    make_step_instr_test ("step instr") (next_st) (chocchip) 
      ("Add ⅓ cup granulated and ½ cup light brown sugars and mix on low speed until the mixture is smooth.");
    make_status_test "cancel" (cancel cook_st) Browsing;
    make_add_test_cookbook "add choc cookbook" new_st 
      "chocolate_chip_cookies.json" true;
    make_cmp_set_like_lists_test "add choc pantry" 
      (all_ings (pantry (add new_st "chocolate_chip_cookies.json")))
      ["butter"; "granulated sugar"; "brown sugar"; "egg"; "vanilla"; 
       "baking soda"; "salt"; "flour"; "chocolate chips"] true; 
    make_update_test "update" choc_st "egg" High; 

  ]

(** Cookbook Tests*)

let rec recipes_to_list lst acc=
  match lst with
  |[] -> acc
  |h::[] -> (h^acc)
  |h::t -> recipes_to_list t (h^acc^", ")

let cookbook_to_string cookbook =
  let string_cookbook = Hashtbl.fold 
      (fun k v acc -> acc^" - "^k^": "^(recipes_to_list v "")^"\n") 
      cookbook "" in
  if string_cookbook = "" then raise EmptyCookbook else string_cookbook


let make_search_test
    (name : string)
    (book : Cookbook.t)
    (key_list : Cookbook.key list)
    (acc : Cookbook.value)
    (expected_output : Cookbook.value) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (search book key_list acc))

let make_search_test_exn
    (name : string)
    (book : Cookbook.t)
    (key_list : Cookbook.key list)
    (acc : Cookbook.value)
    (expected_output : exn) : test = 
  name >:: (fun _ ->
      assert_raises expected_output (fun () -> (search book key_list acc)))

let make_all_recipes_test
    (name : string)
    (book : Cookbook.t)
    (expected_output : string list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (all_recipes book))

let make_recipe_mem_test
    (name : string)
    (book : Cookbook.t)
    (recip: string)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (recipe_mem book recip))

let make_add_to_recipes_test
    (name : string)
    (book : Cookbook.t)
    (recip: string)
    (expected_output : string list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (all_recipes(add_to_recipes book recip)))

let make_remove_valid_test
    (name : string)
    (book : Cookbook.t)
    (recip: string)
    (expected_output : string list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (all_recipes(remove_book book recip)))


(* remove*)
let cookbook_tests =
  let book1 = start_cook_book "123" in
  let bookadded = add_book book1 ["american"; "dessert"; "butter"; "flour"] 
      "chocolate_chip_cookies.json"; 
    add_to_recipes book1 "chocolate_chip_cookies.json" in
  let book_empty = start_cook_book "em" in
  [

    make_search_test "search 1" bookadded ["flour"] [] 
      ["chocolate_chip_cookies.json"];
    make_search_test "search 2" bookadded ["american"; "dessert"] [] 
      ["chocolate_chip_cookies.json"];
    make_search_test_exn "empty" book_empty ["flour"] [] 
      (UnknownIngredient("flour"));
    make_all_recipes_test "all recipes" bookadded 
      ["chocolate_chip_cookies.json"];
    make_all_recipes_test "empty recipes" book_empty 
      [];
    make_recipe_mem_test "recipe_mem test" bookadded
      "chocolate_chip_cookies.json" true;
    make_recipe_mem_test "recipe_mem empty test" book_empty
      "chocolate_chip_cookies.json" false;
    make_add_to_recipes_test "add_to_recipe chocolate_chip_cookies.json" 
      book_empty "chocolate_chip_cookies.json" 
      (["chocolate_chip_cookies.json"]);
    make_add_to_recipes_test "add_to_recipe empty" 
      book_empty "" ([""]);
    make_remove_valid_test "remove chocolate_chip_cookies.json" bookadded 
      "chocolate_chip_cookies.json" [];
  ]


let make_check_test
    (name : string)
    (pant : Pantry.t)
    (ing: string)
    (expected_output : Pantry.quantity) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Pantry.check pant ing))

let make_grocery_list_test
    (name : string)
    (pant : Pantry.t)
    (expected_output : (Pantry.key*string) list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Pantry.grocery_list pant))

let make_pant_to_string_test
    (name : string)
    (pant : Pantry.t)
    (expected_output : string) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Pantry.pantry_to_string pant))

let make_add_list_to_blacklist_test
    (name : string)
    (pant : Pantry.t)
    (ings: string list)
    (expected_output : Pantry.key list) : test =
  name >:: (fun _ ->
      assert_equal expected_output 
        (Pantry.get_blacklist(Pantry.add_list_to_blacklist pant ings)))

let make_add_to_blacklist_test
    (name : string)
    (pant : Pantry.t)
    (ing: string)
    (expected_output : Pantry.key list) : test =
  name >:: (fun _ ->
      assert_equal expected_output 
        (Pantry.get_blacklist(Pantry.add_to_blacklist pant ing)))

let make_rem_from_blacklist_test
    (name : string)
    (pant : Pantry.t)
    (ing: string)
    (expected_output : Pantry.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Pantry.rem_from_blacklist pant ing))

let make_get_blacklist_test
    (name : string)
    (pant : Pantry.t)
    (expected_output : string list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Pantry.get_blacklist pant))


let make_add_pantry_test
    (name : string)
    (pant : Pantry.t)
    (ings: string list)
    (expected_output : Pantry.t) : test =
  name >:: (fun _ ->
      Pantry.add_pantry pant ings;
      assert_equal expected_output (pant))



let pantry_tests = 
  let basic_pant = Pantry.start_pantry "" in
  Pantry.update basic_pant ("flour") Pantry.High;
  Pantry.update basic_pant ("egg") Pantry.Low;
  Pantry.update basic_pant ("milk") Pantry.None;


  let none_pant = Pantry.start_pantry "" in
  Pantry.update none_pant ("lettuce") Pantry.None;
  Pantry.update none_pant ("tomato") Pantry.None;
  Pantry.update none_pant ("onion") Pantry.None;

  let basic_pant_blacklist = Pantry.add_to_blacklist basic_pant "egg" in 

  let basic_blacklist_list_pant =  ["flour";"milk"] in 

  let basic_blacklist_ing_pant = ["egg"] in 
  [
    make_check_test "basic_pant check flour test" basic_pant "flour" 
      Pantry.High;
    make_check_test "basic_pant check flour test" basic_pant "egg" Pantry.Low;
    make_check_test "basic_pant check flour test" basic_pant "milk" Pantry.None;

    make_grocery_list_test "basic_pant grocery_list test" basic_pant
      [("egg","low");("milk","none")];

    make_pant_to_string_test "basic_pant to_string test" basic_pant
      " - egg: low\n - milk: none\n - flour: high\n";

    make_add_list_to_blacklist_test "basic_pant add_list_to_blacklist flour,
                                                 milk test" basic_pant 
      ["flour";"milk"] basic_blacklist_list_pant;
    make_add_to_blacklist_test "basic_pant add_ing_to_blacklist egg test"
      basic_pant "egg" basic_blacklist_ing_pant;

    make_rem_from_blacklist_test "basic_pant rem_from_blacklist egg test"
      basic_pant_blacklist "egg" basic_pant;

    make_get_blacklist_test "basic_pant_blacklist get_blacklist test" 
      basic_pant_blacklist ["egg"];

    make_add_pantry_test "add_pantry empty_pantry [onion;tomato;lettuce]"
      (Pantry.start_pantry "") ["onion";"tomato";"lettuce"] none_pant;
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    read_tests;
    command_tests;
    state_tests;
    cookbook_tests;
    pantry_tests;
  ]

let _ = run_test_tt_main suite
