(* Errors from Command.ml *)
let c_empty = "Please enter a command. \n> "
let c_malformed = "Sorry, I can't understand that command. Please try again. \n> "
let c_no_input_ing s = ("Sorry, what would you like me to '"^s^"'? 
    P.S: '"^s^"' requires an argument. Try adding an ingredient. \n> ")
let c_no_input_recp s = ("Sorry, what would you like me to '"^s^"'? 
    P.S: '"^s^"' requires an argument. Try adding a recipe. \n> ")
let c_has_input s = ("Sorry, can you repeat that?
     If you'd like to '"^s^"', please enter only '"^s^"'. \n> ")
let c_too_many_inputs s = "Sorry, I didn't get that.
     I can only '"^s^"' one thing at a time. \n> "
let c_too_few_inputs s = "Sorry, I didn't get that.
     I need more information about what you want to "^s^".
     Did you forget a comma between the ingredient and its new quantity?\n> "
let c_exit_while_cooking =
  "Please enter 'cancel' to end your cooking session. \n> "
let c_cooking_input = "Sorry, I can only do this while you're cooking.
   Please try a new command or enter 'cook [json file]' to begin cooking! \n> "
let c_browsing_input = 
  "Sorry, I can only do this when you're not cooking.
 Please try a new command or enter 'cancel' to end your cooking session. \n> "

(*Errors from Cookbook.ml *)
let cb_unknown_search s = 
  "Sorry, I can't find a recipe that fits the search term "^s^".
     Please try a new search. \n> "
let cb_empty_book = "Sorry, you have no recipes in your cookbook. \n> "
let cb_unknown_recipe s = 
  "Sorry, I can't find a recipe name "^s^" in your pantry.
     Please try again. \n> "

(*Errors from Pantry.ml *)
let p_item_not_found s = "Sorry, I couldn't find '"^s^"' in your pantry. \n> "
let p_item_not_found_b s = 
  "Sorry, it seems like you never unrecommended '"^s^"'. \n> "
let p_no_input = "That command was applied to no arguments,
   it requires two. Please try a new input. \n> "
let p_one_input = "That command was only applied to one
   argument, it requires two. Please try a new input. \n> "
let p_too_many_inputs = "That command was applied to too many
   arguments. Please try a new input. \n> "
let p_empty_pantry = "Sorry, your pantry is empty.
 You can add items by calling 'update [item], [quantity]'
  or by adding a recipe. \n> "
let p_full_pantry = "Sorry, your pantry is full. \n> "
let p_black_overlap = "It seems like all your items that are Low or Empty
 have been unrecommended. \n> "
let p_item_not_in_pantry s = "Sorry, "^s^" is not in your pantry.
  Please make sure it is in your pantry before unrecommending "^s^".
   \n> "
let p_invalid_quantity s = "Sorry, "^s^" isn't a valid quantity.
  Please try again with 'high', 'low', or 'none'. \n> "
let p_malformed =
  "Sorry, I can't understand that command. Please try again. \n>"


(* Erros from state.ml *)
let s_already_cooking = "You are already cooking.
   Please try a new input.\n> "
let s_not_cooking = "You are not cooking.
   Please try a new input.\n> "
let s_add_quantity = "You did not type the quantity of your
   ingredient. Please try a new input.\n> "
let s_end_of_recipe = "Your dish is finished! Congrats!\n> "^
                      "Please remember to update the quantity of 
      any ingredients used while cooking today.\n>"
let s_invalid_file f = "Sorry, I can't find "^f^".
   Please enter a different file name. \n> "

let s_invalid_quantity s = "Sorry, "^s^" isn't a valid quantity.
  Please try again with 'high', 'low', or 'none'. \n> "
let s_first_step = "Sorry, You are on the first step.
   There is no previous step. Please try a new command or enter 'cancel'
   to end your cooking session. \n> "
let s_already_added s = s^" is already in your cookbook. \n>"

(* Errors from read.ml *)
let r_invalid_json = "Sorry, it seems like that recipe is not in
the correct JSON format. Please edit the file or try a different one. \n>"
let r_empty_name =
  "Sorry, it seems like the 'name' field in this JSON file is empty.
  Please edit the file or try a different one.\n>"
let r_no_name = 
  "Sorry, it seems like this recipe has no 'name' field.
 Please edit the file or try a different one.\n> "
let r_invalid_name_type = 
  "Sorry, it seems like this recipe doesn't have a name of type string.
 Please edit the file or try a different one.\n>"
let r_invalid_servings =
  "Please make sure this recipe has servings greater than 0. 
 Please edit the file or try a different one.\n"
let r_no_servings = 
  "Sorry, it seems like this recipe has no 'servings' field.
 Please edit the file or try a different one.\n> "
let r_invalid_serving_type = 
  "Sorry, it seems like this recipe doesn't have 'servings' of type int.
 Please edit the file or try a different one.\n>"
let r_empty_cuisine =
  "Sorry, it seems like the 'cuisine' field in this JSON file is empty.
  Please edit the file or try a different one.\n>"
let r_no_cuisine = 
  "Sorry, it seems like this recipe has no 'cuisine' field.
 Please edit the file or try a different one.\n> "
let r_invalid_cuisine_type = 
  "Sorry, it seems like this recipe doesn't have a 'cuisine' of type string.
 Please edit the file or try a different one.\n>"
let r_empty_course =
  "Sorry, it seems like the 'course' field in this JSON file is empty.
  Please edit the file or try a different one.\n>"
let r_no_course = 
  "Sorry, it seems like this recipe has no 'course' field.
 Please edit the file or try a different one.\n> "
let r_invalid_course_type = 
  "Sorry, it seems like this recipe doesn't have a 'course' of type string.
 Please edit the file or try a different one.\n>"
let r_no_step_num = 
  "Sorry, it seems like this recipe is missing a 'step_num' field.
 Please edit the file or try a different one.\n> "
let r_invalid_step_num_type = 
  "Sorry, it seems like this recipe doesn't have 'step_num' of type int.
 Please edit the file or try a different one.\n>"
let r_invalid_step_nums = 
  "Please make sure the recipe's step numbers start with 1 and increment by 1 for
each step."
let r_empty_step_instr = 
  "Sorry, it seems like this recipe has an empty 'instructions' field.
 Please edit the file or try a different one.\n> "
let r_no_step_instr = 
  "Sorry, it seems like this recipe is missing an 'instructions' field.
 Please edit the file or try a different one.\n> "
let r_invalid_step_instr_type = 
  "Sorry, it seems like this recipe doesn't have 'instructions' of type string.
 Please edit the file or try a different one.\n>"
let r_no_step_ing = 
  "Sorry, it seems like this recipe is missing an 'ingredients' field.
 Please edit the file or try a different one.\n> "
let r_invalid_step_ing_type = 
  "Sorry, it seems like this recipe doesn't have 'ingredients' of type string
   list. Please edit the file or try a different one.\n>"
let r_empty_steps =
  "Sorry, it seems like the 'steps' field in this JSON file is empty.
  Please edit the file or try a different one.\n>"
let r_no_steps = 
  "Sorry, it seems like this recipe is missing a 'steps' field.
 Please edit the file or try a different one.\n> "
let r_invalid_steps_type = 
  "Sorry, it seems like this recipe doesn't have 'steps' of type list.
 Please edit the file or try a different one.\n>"


(*Other errors *)
let json_error s = "Sorry, it seems like that JSON is invalid. It has '"^s^"'.\n>"
let json_type_error s =
  "Sorry, it seems like that JSON is invalid. It has '"^s^"'.\n>"

type tutorial =
  | Part of string
  | Name of string
  | Words of string
  | Enter of string 
  | Italics of string
  | Commands of string
  | Description of string
  | Sous of string

(* Tutorial *)
let part1 = Part "
Hello there!\n
My name is "
let sous = Sous "Sous"
let words = Words ". I'm here to help you around the kitchen!\n
I can keep track of your "
let name1 = Name " cookbook " 
let words1 = Words "and your" 
let name2 = Name " pantry "
let words2 = Words ".\n Your"
(* print name1*) 
let words3 = Words "stores your recipes and \n your "
(*print name2*)
let words4 = Words "stores the names of foods you own or have owned before.\n"
let enter = Enter "[Press enter to continue]"

let part2 = Part "There are 5"
(* print name 1*)
let words5 = Words "commands: "
let italics1 = Italics "add, search, remove, recipes"
(*print words75*)
let words55 = Italics "cook.\n"
let command1 = Commands "\"add [name]\""
let description1 = Description " adds the recipe titled [name] to your cookbook.\n"
let command2 = Commands "\"search [keyword1, keyword2,… ]\""
let description2 = Description 
    " searches your cookbook for
              recipes that use at least one of [keyword1, keyword2,…].\n"
let italics2 = Description 
    "              (Keywords can be ingredients, cuisines, or courses!)\n"
let command3 = Commands "\"remove [name]\"" 
let description3 = Description " removes the recipe titled [name] from your cookbook.\n"
let command4 = Commands "\"recipes\""
let description4 = Description " displays a list of all the recipes currently in your cookbook.\n"
let command5 = Commands "\"cook [name]\""
let description5 = Description " guides you through step-by-step instructions 
              for the recipe titled [name].\n"
(*print enter*)

let part3 =  Part "While cooking, you can call: \n"
let command6 = Commands "\"next\""
let description6 =  Description " takes you to the next step.\n"
let command7 = Commands "\"previous\""
let description7 = Description " takes you to the previous step.\n"
let command8 = Commands "\"cancel\""
let desctription8 =
  Description " exits the recipe and takes you back to the main menu.\n\n"
let words6 = Words 
    "Once you cancel or complete the recipe, you will be prompted to update the
quantites of every ingredient you used while cooking.\n
You can press enter to keep the old quantity or update the ingredient by
entering the new quantity (none, low, or high).\n"
(*print enter*)

let part4 =  Part "There are 6"
(*print name 2*)
let words7 = Words "commands: \n"
let italics3 = Italics "update, check, recommend, unrecommend, rerecommend"
let words75 = Words " and "
let italics35 = Italics "pantry.\n\n"
(*print name 2*)
let words8 = Words 
    "keeps track of the quantity of each ingredient. 
 The value of an ingredient can be "
let italics4 = Italics "High, Low, "
let words85 = Words "or "
let italics5 = Italics "None.\n\n"
let command9 = Commands "\"update [name, quantity]\""
let description9 = Description " updates the ingredient titled [name] to have
              amount [quantity].\n"
let italics6 = Description
    "              If [name] is a new ingredient, it will be added with its
              initial value set to [quantity].\n"
let command10 = Commands "\"check [name]\""
let description10 = Description " returns the quantity of [name] if stored in your pantry.\n"
let command11 = Commands "\"recommend\""
let description11 = Description " prints a list of every ingredient in your pantry with quantities
              Low or None.\n"
let command12 = Commands "\"unrecommend [name]\""
let description12 = Description " removes [name] from your recommended grocery list, 
              even if it has quantity Low or None.\n"
let command13 = Commands "\"rerecommend [name]\""
let description13 = Description " allows [name] to be included in your recommended grocery
              list once again when it has quantity Low or None\n"
let command14 = Commands "\"pantry\""
let description14 = Description " displays all the ingredients in your pantry along with their quantites\n"
(*print enter*)

let part5 = Part "Finally, \n"
let command15 = Commands "\"exit\""
let description15 = Description " quits the program and saves your"
(*print name 1*)
let description16 = Description "and your"
(*print name 2*)
let words9 = Words ".\n\nOne thing to note is that you cannot "
(*print command15*)
let words10 =
  Words " if you are in the middle of cooking a recipe. You first must call "
(*print command8*)
let words11 = Words " to stop cooking and then call "
(*print command15*)
let words12 = Words " \nto quit the program. \n"
(*print enter*)

let tutorial = 
  [part1; sous; words; name1; words1; name2; words2; name1; words3; name2; 
   words4; enter; part2; name1; words5; italics1; words75; words55; command1;
   description1;
   command2; description2; italics2; command3; description3; command4;
   description4; command5; description5; enter; part3; command6; description6;
   command7; description7; command8; desctription8; words6; enter; part4;
   name2; words7; italics3; words75; italics35; name2; words8; italics4; 
   words85; italics5; command9; description9; italics6;
   command10; description10; command11; description11; command12;
   description12; command13; description13; command14; description14;
   enter; part5; command15; description15; name1; description16; name2; words9;
   command15; words10; command8; words11; command15; words12; enter
  ]