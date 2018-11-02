open Print
open Read
open Command
open Cookbook
open Pantry
open State

(**[extract option] is the value inside of [option].
   Examples:
   - [extract [Some 0]] is 0
   - [extract [Some "eat"] is "eat" 
   Raises: [Not_found] if [option] is None. *)
let extract (option : Read.t option) =
  match option with
  |None -> raise (Not_found)
  |Some o -> o

(**[head lst] is the first element of [lst].
   Examples:
   - [head [0; 3; 7]] is 0
   - [head ["eat"; "apple"; "pie"]] is "eat" 
   Raises: [Malformed] if [lst] is empty. *)
let head lst =
  match lst with
  |[] -> raise (Not_found)
  |h::t -> h

(*[last lst] returns the last element of [lst].
  Examples:
    - [last ["high"] is "high"
    - [last ["chocolate"; "chips"; "low"]] is "low" 
    Raises: [Not_found] if [lst] is an empty list. *)
let rec last lst =
  match lst with
  |[] -> raise (Not_found)
  |h::[] -> h
  |h::t -> last t

(*[ingredient lst acc] returns the elements of [lst] in the same order
  but as a single string separated by spaces, excluding the last element.
  Examples:
    - [ingredient ["apples"; "high"]] is " apples"
    - [ingredient ["chocolate"; "chips"; "high"]] is " chocolate chips" 
    Raises: [Not_found] if [lst] is an empty list. *)
let rec ingredient lst acc =
  match lst with
  |[] -> raise (Not_found)
  |h::[] -> acc
  |h::t -> ingredient t  (acc^" "^h)

(** [print_list] prints items in a list starting with "- " and
    seperated by "\n"
    Examples:
    - [print_list ["apples"]] prints "- apples"
    - [print_list ["apples"; "flour"]] 
            prints "- apples
                    - flour "*)
let rec print_list = function
  | [] -> ()
  | h::t -> print_string ("- "^h^"\n"); print_list t

(** [print_search_list lst] prints [lst] in the terminal with special
     formatting that includes the recipe name bolded followed by a ':' and
     the cuisine, course, and servings.
    Examples:
    - [print_search_list ["apple_pie.json"]] 
        prints "apple pie: american, dessert, serves 12"
    - [print_search_list ["apple_pie.json"; "creme_brulee.json"]] 
        prints "apple pie: american, dessert, serves 12
                creme brulee: french, dessert, serves 6 " *)
let rec print_search_list lst = begin
  match lst with
  |[] -> ()
  |h::t -> let read = h |> Yojson.Basic.from_file |> from_json in 
    let name = String.sub h 0 (String.length h - 5) |> 
               String.map (fun c -> if c = '_' then ' ' else c) in
    let cuisine = get_cuisine read in 
    let course = get_course read in 
    let servings = get_servings read in
    ANSITerminal.print_string [Bold] (name);
    print_string (": "^cuisine^", "^course^", serves ");
    print_int servings;
    print_string "\n"; 
    print_search_list t end

(** [groc_list_to_string lst acc] returns [lst] as a string to be printed
    in the terminal with accumulator [acc]
    Examples:
    - [groc_list_to_string [("apples", "low")] ""] prints "apples: low"
    - [groc_list_to_string [("apples", "low"); ("flour", "none")] ""] 
            prints "apples: low
                    flour : none" *)
let rec groc_list_to_string lst acc =
  match lst with
  |[] -> acc
  |(k,v)::t -> groc_list_to_string t ("-"^k^": "^v^" \n"^acc)

(** [add_ings recipe num step acc] is the list of ingredients used in the steps
    before and inclusing [step]. [num] is the accumulator for the step numbers
     and [acc] accumulates the final list. 
     Examples:
    - [add_ings creme_brulee 1 2 []] is ["heavy cream"; "vanilla bean"]
    - [add_ings mac_and_cheese 1 1 []] is ["milk"; "butter"] *)
let rec add_ings recipe num step acc =
  if num = step then ( get_step_ingredients num recipe)@acc
  else add_ings recipe (num+1) step (( get_step_ingredients num recipe)@acc)

(** [print_main_menu color1 color2] prints the main menu with Cookbook commands
    colored in [color1] and Pantry commands colored in [color2]. *)
let print_main_menu color1 color2 = begin
  ANSITerminal.(print_string [color1; on_white] "Cookbook Commands:     Add");
  ANSITerminal.(print_string [color1; on_white] "     Search");
  ANSITerminal.(print_string [color1; on_white] "     Cook");
  ANSITerminal.(print_string [color1; on_white] "     Remove");
  ANSITerminal.(print_string [color1; on_white] "     Recipes           \n");
  ANSITerminal.(print_string [color2; on_white] "Pantry Commands:  Update");
  ANSITerminal.(print_string [color2; on_white] "   Recommend");
  ANSITerminal.(print_string [color2; on_white] "   Check");
  ANSITerminal.(print_string [color2; on_white] "   Unrecommend");
  ANSITerminal.(print_string [color2; on_white] "   Rerecommend");
  ANSITerminal.(print_string [color2; on_white] "  Pantry\n");
  ANSITerminal.(print_string [blue; on_white] "     \n Exit");
  print_string("\n>"); end

(** [cook_help pantry book st] is a helper function that repeats until the game
    is quit. It prints the prompt when the game starts, reads the player's 
    input, and parses it. It calls the appropriate function if the input is
     valid or prints an error message if it is invalid.
     [name] is the username entered by the user.
     [recipe] is [Some recipe] when cooking and [None] otherwise.
     [pantry] is the user's current Pantry.
     [book] is the user's current Book.
     [st] is the current kitchen State.
*)
let rec cook_help name recipe pantry book st =
  let str = Pervasives.read_line () in
  try
    match  parse ( status st) str with
    | Cook(t) -> begin 
        let file_name = head t in try
          let read = file_name |> Yojson.Basic.from_file |> from_json in
          print_string ("Ingredients Needed: \n");
          print_list ( get_ingredients read 1 []);
          print_endline ("");
          let new_st =  cook st in
          print_string (( step_instr new_st read)^" \n> ");
          cook_help name (Some read) pantry book new_st
        with |Sys_error e -> raise ( InvalidFile file_name) end
    | Next -> let new_st =  next st in
      print_string (( step_instr new_st (extract recipe))^" \n> ");
      cook_help name recipe pantry book new_st
    | Previous -> let new_st =  previous st in
      print_string (( step_instr new_st (extract recipe))^" \n> ");
      cook_help name recipe pantry book new_st
    | Update(t) -> let item = ingredient t "" in 
      let quantity = (t |> last) in 
      update st item quantity;
      print_string (item^" updated! \n> ");
      cook_help name recipe pantry book st
    | Search(t)-> print_search_list ( search book t []);
      print_string "\n> ";
      cook_help name recipe pantry book st
    | Add(t) -> let recipe_name = t |> head in
      ignore (Read.check (recipe_name |> Yojson.Basic.from_file |> from_json));
      let state =  add st recipe_name in
      print_string (recipe_name^" is now in your cookbook. \n> ");
      cook_help name recipe ( State.pantry state) ( cookbook state) state
    | Recommend ->
      let x = groc_list_to_string ( grocery_list pantry) "" in 
      print_endline(String.sub x 0 ((String.length x)-2));
      print_string "> ";
      cook_help name recipe pantry book st
    | Remove (t) -> let x =  remove_book book (head t) in
      print_endline ((head t)^" has been removed from your cookbook."); 
      print_string "> ";
      cook_help name recipe pantry x st
    | Recipes -> if (( all_recipes book) = []) then 
        (print_string ("You do not have any recipes in your cookbook right now.
    Type 'add [recipe]' to add a recipe to your cookbook!");) else
        print_list ( all_recipes book);
      print_string "\n> ";
      cook_help name recipe pantry book st
    | Exit -> print_string ("See you soon, "^name^"! \n");
      quit_help book pantry (String.lowercase_ascii name); exit 0
    | Check(t) -> let quantity = match  check pantry (head t) with 
        | High -> "high"
        | Low -> "low"
        | None -> "none" in
      print_endline (quantity); 
      print_string "> ";
      cook_help name recipe pantry book st
    | Cancel -> print_string ("Please remember to update the quantity of 
      any ingredients used while cooking today.\n>" );
      let step = match  status st with
        |Browsing -> 1
        |Cooking t -> t in
      let used_ings = add_ings (extract recipe) 1 step [] in
      add_pantry pantry [(extract recipe |> get_name)];
      post_cook_update pantry 
        ((extract recipe |> get_name)::used_ings);
      print_main_menu ANSITerminal.red ANSITerminal.cyan;
      cook_help name None pantry book ( cancel st)
    | Pantry -> let pantry_str =  pantry_to_string pantry in
      print_string (pantry_str^"> ");
      cook_help name recipe pantry book st
    | Rerecommend t -> 

      let p =  rem_from_blacklist pantry (head t) in
      print_string((head t)^" has been rerecommended \n>");
      cook_help name recipe p book st
    | Unrecommend t -> let p =  add_to_blacklist pantry (head t) in 
      print_string((head t)^" has been unrecommended \n>");
      cook_help name recipe p book st
  with
  | Empty -> 
    print_string Print.c_empty;
    cook_help name recipe pantry book st
  |Command.Malformed ->
    print_string Print.c_malformed;
    cook_help name recipe pantry book st
  | NoInputIng s ->
    print_string (Print.c_no_input_ing s);
    cook_help name recipe pantry book st
  | NoInputRecp s ->
    print_string (Print.c_no_input_recp s);
    cook_help name recipe pantry book st
  | HasInput s ->
    print_string (Print.c_has_input s);
    cook_help name recipe pantry book st
  |Command.TooManyInputs s ->
    print_string (Print.c_too_many_inputs s);
    cook_help name recipe pantry book st
  | TooFewInputs s ->
    print_string (Print.c_too_few_inputs s);
    cook_help name recipe pantry book st
  | UnknownIngredient s -> 
    print_string (Print.cb_unknown_search s);
    cook_help name recipe pantry book st
  | UnknownRecipe s -> 
    print_string (Print.cb_unknown_recipe s);
    cook_help name recipe pantry book st
  | ItemNotFound s ->
    print_string (Print.p_item_not_found s);
    cook_help name recipe pantry book st 
  | ItemNotFoundB s ->
    print_string (Print.p_item_not_found s);
    cook_help name recipe pantry book st
  | EmptyPantry -> print_string (Print.p_empty_pantry);
    cook_help name recipe pantry book st 
  | FullPantry -> print_string (Print.p_full_pantry);
    cook_help name recipe pantry book st 
  | AllBlacklistOverlap -> 
    print_string (Print.p_black_overlap);
    cook_help name recipe pantry book st 
  | BlacklistItemNotInPantry s -> 
    print_string (Print.p_item_not_in_pantry s);
    cook_help name recipe pantry book st 
  | InvalidQuantity s -> print_string (Print.p_invalid_quantity s);
    cook_help name recipe pantry book st
  | Malformed ->
    print_string (Print.p_malformed);
    cook_help name recipe pantry book st
  | AlreadyCooking -> print_string (Print.s_already_cooking);
    cook_help name recipe pantry book st 
  | NotCooking -> print_string (Print.s_not_cooking);
    cook_help name recipe pantry book st 
  | AddQuantity -> print_string (Print.s_add_quantity);
    cook_help name recipe pantry book st 
  | NoNextStep -> print_string (Print.s_end_of_recipe);
    let x = begin match status st with
      |Browsing -> 1
      |Cooking t -> t end in
    let used_ings = add_ings (extract recipe) 1 x [] in
    add_pantry pantry [(extract recipe |> get_name)];
    post_cook_update pantry 
      ((extract recipe |> get_name)::used_ings);
    print_main_menu ANSITerminal.red ANSITerminal.cyan;
    cook_help name None pantry book ( cancel st)
  | InvalidFile f -> print_string (Print.s_invalid_file f);
    cook_help name recipe pantry book st
  | ExitWhileCooking -> print_string (Print.c_exit_while_cooking);
    cook_help name recipe pantry book st
  | CookingInput -> print_string (Print.c_cooking_input);
    cook_help name recipe pantry book st
  | BrowsingInput -> print_string (Print.c_browsing_input);
    cook_help name recipe pantry book st
  (*| InvalidQuantity s -> print_string (Print.s_invalid_quantity s);
    cook_help name recipe pantry book st *)
  | OnFirstStep -> print_string (Print.s_first_step);
    cook_help name recipe pantry book st
  |Yojson.Json_error s ->
    let s = String.lowercase_ascii s in 
    print_string (Print.json_error s); 
    cook_help name recipe pantry book st
  |Yojson.Basic.Util.Type_error (s, j) -> 
    let s = String.lowercase_ascii s in 
    print_string (Print.json_type_error s); 
    cook_help name recipe pantry book st
  | InvalidJSON -> print_string (Print.r_invalid_json); 
    cook_help name recipe pantry book st
  | AlreadyAdded s -> print_string (Print.s_already_added s);
    cook_help name recipe pantry book st
  | EmptyCookbook ->  print_string (Print.cb_empty_book);
    cook_help name recipe pantry book st
  | EmptyName -> print_string (Print.r_empty_name); 
    cook_help name recipe pantry book st
  | NoName -> print_string (Print.r_no_name); 
    cook_help name recipe pantry book st
  | InvalidNameType -> print_string (Print.r_invalid_name_type); 
    cook_help name recipe pantry book st
  | InvalidServings -> print_string (Print.r_invalid_servings); 
    cook_help name recipe pantry book st
  | NoServings -> print_string (Print.r_no_servings); 
    cook_help name recipe pantry book st
  | InvalidServingType -> print_string (Print.r_invalid_serving_type); 
    cook_help name recipe pantry book st
  | EmptyCuisine -> print_string (Print.r_empty_cuisine); 
    cook_help name recipe pantry book st
  | NoCuisine -> print_string (Print.r_no_cuisine); 
    cook_help name recipe pantry book st
  | InvalidCuisineType -> print_string (Print.r_invalid_cuisine_type); 
    cook_help name recipe pantry book st
  | EmptyCourse -> print_string (Print.r_empty_course); 
    cook_help name recipe pantry book st
  | NoCourse -> print_string (Print.r_no_course); 
    cook_help name recipe pantry book st
  | InvalidCourseType -> print_string (Print.r_invalid_course_type); 
    cook_help name recipe pantry book st
  | NoStepNum -> print_string (Print.r_no_step_num); 
    cook_help name recipe pantry book st
  | InvalidStepNumType -> print_string (Print.r_invalid_step_num_type); 
    cook_help name recipe pantry book st
  | InvalidStepNums -> print_string (Print.r_invalid_step_nums); 
    cook_help name recipe pantry book st
  | EmptyStepInstr -> print_string (Print.r_empty_step_instr); 
    cook_help name recipe pantry book st
  | NoStepInstr -> print_string (Print.r_no_step_instr); 
    cook_help name recipe pantry book st
  | InvalidStepInstrType -> print_string (Print.r_invalid_step_instr_type); 
    cook_help name recipe pantry book st
  | NoStepIng -> print_string (Print.r_no_step_ing); 
    cook_help name recipe pantry book st
  | InvalidStepIngsType -> print_string (Print.r_invalid_step_ing_type); 
    cook_help name recipe pantry book st
  | EmptySteps -> print_string (Print.r_empty_steps); 
    cook_help name recipe pantry book st
  | NoSteps -> print_string (Print.r_no_steps); 
    cook_help name recipe pantry book st
  | InvalidStepsType -> print_string (Print.r_invalid_steps_type); 
    cook_help name recipe pantry book st

(** [tutorial_help messages] begins the tutorial using the strings in 
    [messages].
    Example:
    - [tutorial_help [message1; message2]] prints [message1] followed by
    [message2]
*)
let rec tutorial_help messages =
  match messages with 
  |[] -> ()
  |(Sous s)::t -> ANSITerminal.(print_string [Bold; magenta] s); tutorial_help t
  |(Part s)::t -> ANSITerminal.(print_string [Bold; blue] s); tutorial_help t
  |(Name s)::t -> ANSITerminal.(print_string [Bold; green] s); tutorial_help t
  |(Words s)::t -> ANSITerminal.(print_string [Bold; blue] s); tutorial_help t
  |(Enter s)::t -> begin ANSITerminal.(print_string [Bold; magenta; Blink] s);
      let input = read_line() in
      match input with
      | exception End_of_file -> tutorial_help t
      | "" ->tutorial_help t
      |_-> print_endline ("Please press enter to continue.");
        tutorial_help t end
  |(Italics s)::t ->
    ANSITerminal.(print_string [yellow; Bold] s); tutorial_help t
  |(Commands s)::t -> 
    ANSITerminal.(print_string [cyan; Bold] s); tutorial_help t
  |(Description s)::t ->
    ANSITerminal.(print_string [blue; Bold] s); tutorial_help t

(** [tutorial_check dir] continues the program if [dir] is [Old n] and starts
    the tutorial if [dir] is [New n] 
    Example:
    - [tutorial_check [Old "Me"] is ()
    - [tutorial_check [New "Me"] is the tutorial
*)
let tutorial_check dir =
  match dir with
  | Old x -> ()
  | New x -> 
    tutorial_help tutorial;
    ANSITerminal.(print_string [green; Bold]
                    "Enjoy cooking and thank you for using Sous!\n\n")

(** [start_kitchen name] starts the kitchen/application using username [name]. 
    Example:
    - [start_kitchen "Me"] starts the program with username being "Me".
*)
let start_kitchen name =
  let lowercase_name = String.lowercase_ascii name in
  let dir = search_dir lowercase_name "." in 
  let state =  init_state (dir) in
  tutorial_check (dir);
  print_endline ("Hi "^name^"! How may I help you today?");
  print_main_menu ANSITerminal.red ANSITerminal.cyan;
  cook_help name None (State.pantry state) (State.cookbook state) state

(**[read_line_check input] calls [start_kitchen input] if [input] is a valid
   username, and prompts for a new username otherwise. 
   Example:
   - [read_line_check "Me"] is [start_kitchen "Me"]
   - [read_line_check "Me/"] recurses *)
let rec read_line_check input = 
  match input with
  | exception End_of_file -> ()
  | name -> begin if String.contains name '/' 
      then begin print_string "Sorry, your username can't contain '/'.
   Please enter a new name.\n>"; read_line_check (read_line ()) end
      else start_kitchen name end

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to your kitchen!\n");
  print_endline "What is your name?";
  print_string "> ";
  read_line_check (read_line ())

(* Execute the game engine. *)
let () = main ()
