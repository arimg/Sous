
type existing = Old of string | New of string
type status = Cooking of int | Browsing

type t = {cookbook : Cookbook.t; status : status; pantry: Pantry.t}

exception AlreadyCooking
exception NotCooking
exception AddQuantity
exception NoNextStep
exception InvalidFile of string
exception InvalidQuantity of string
exception OnFirstStep
exception AlreadyAdded of string

(** [find_book_key lst word] is the cookbook key found in [lst] with accumulator
    [word]. 

    Examples: 
    [find_book_key ["eggs"; "chocolate_chip_cookies.json"] ""] 
    would return "eggs"

    [find_book_key ["chocolate"; "chips"; "chocolate_chip_cookies.json"] ""] 
    would return "chocolate chips" *)
let rec find_book_key lst word =
  match lst with
  |[] -> raise (Not_found)
  |h::t -> if (String.length h > 4) && ((Str.last_chars h 5) = ".json") 
    then String.sub word 0 ((String.length word) -1)
    else find_book_key t (word^h^" ")

(** [json_tail lst] is list of words ending in .json in [lst]. 

    Examples: 
    [json_tail ["chocolate"; "chips"; "chocolate_chip_cookies.json"]]
    would return ["chocolate_chip_cookies.json"] 

    [json_tail ["eggs"; "apple_pie.json"; "chocolate_chip_cookies.json"]]
    would return ["apple_pie.json"; "chocolate_chip_cookies.json"] *)
let rec json_tail lst =
  match lst with
  |[] -> []
  |h::t -> try if (Str.last_chars h 5) = ".json"
      then h::(json_tail t)
      else json_tail t
    with
    |Invalid_argument e -> json_tail t

(** [first_elts lst] is [lst] without the last element as a string. 

    Example: 
    [first_elts ["panda"; "cat"; "giraffe"]] would return
    "panda cat" *)
let rec first_elts lst =
  match lst with
  |[] -> raise AddQuantity
  |h::[] -> raise AddQuantity
  |h::i::[] -> h
  |h::t -> h^" "^(first_elts t)

(** [last lst] is the last element of [lst]. 

    Example: 
    [first_elts ["panda"; "cat"; "giraffe"]] would return
    "giraffe" *)
let rec last lst =
  match lst with
  |[] -> raise AddQuantity
  |h::[] -> h
  |h::t -> last t

(** [file_parse_book file_name file book] is the [book] with inputs found from
    [file] with name [file_name]. 

    Example: 
    [file_parse_book "panda_book.txt" panda p_book] returns the cookbook
    p_book with the inputs from "panda_book.txt" *)
let rec file_parse_book_help file_name file book =
  try
    let line = input_line file in
    let words = String.split_on_char ' ' line in
    Cookbook.rewrite_book book (find_book_key words "" 
                                |> String.trim) (json_tail words);
    file_parse_book_help file_name file book
  with
  |End_of_file -> close_in file; Sys.remove file_name; book

(** [file_parse_book file_name file book] is the [book] with inputs found
    from [file] with name [file_name]. It processes the first line of [file] as
    a list of all of the recipes in [book] and then calls [file_parse_book_help] 
    to process the rest of the file.

    Example: 
    [file_parse_pantry "panda_pantry.txt" panda p_pantry] returns the pantry
    p_pantry with the inputs from "panda_pantry.txt" *)
let file_parse_book file_name file book =
  try
    let line = input_line file in
    let words = 
      if line = "" then [] 
      else String.split_on_char ' ' line in
    let cookbook = List.fold_left Cookbook.add_to_recipes book words in
    file_parse_book_help file_name file cookbook
  with
  |End_of_file -> close_in file; Sys.remove file_name; book


(** [init_cookbook exists] is the initial cookbook based on [exists]. 

    Example: 
    [init_cookbook Old("panda")] would return a cookbook based on the
    file "panda_book.txt"

    [init_cookbook New("panda")] would return a new cookbook with name 
    "panda" *)
let init_cookbook exists = 
  match exists with
  | New s -> Cookbook.start_cook_book s
  | Old s -> let new_book = Cookbook.start_cook_book s in
    let file_name = s^"_book.txt" in 
    let file = open_in file_name in

    file_parse_book file_name file new_book

(** [file_parse_pantry_help file_name file pantry] is the [pantry] with inputs found
    from [file] with name [file_name]. 

    Example: 
    [file_parse_pantry_help "panda_pantry.txt" panda p_pantry] returns the pantry
    p_pantry with the inputs from "panda_pantry.txt" *) 
let rec file_parse_pantry_help file_name file pantry =
  try
    let line = input_line file in
    let words = String.split_on_char ' ' line in
    (Pantry.rewrite_pantry pantry (first_elts words) (last words));
    file_parse_pantry_help file_name file pantry
  with
  |End_of_file -> close_in file; Sys.remove file_name; pantry

(** [file_parse_pantry file_name file pantry] is the [pantry] with inputs found
    from [file] with name [file_name]. It processes the first line of [file] as
    the blacklist of [pantry] and then calls [file_parse_pantry_help] to process
    the rest of the file.

    Example: 
    [file_parse_pantry "panda_pantry.txt" panda p_pantry] returns the pantry
    p_pantry with the inputs from "panda_pantry.txt" *)
let file_parse_pantry file_name file pantry =
  try
    let line = input_line file in
    let words = String.split_on_char ' ' line in
    let p = Pantry.add_list_to_blacklist pantry words in
    file_parse_pantry_help file_name file p
  with
  |End_of_file -> close_in file; Sys.remove file_name; pantry

(** [init_pantry exists] is the initial pantry based on [exists]. 

    Examples: 
    [init_pantry Old("panda")] would return a pantry based on the
    file "panda_pantry.txt" 

    [init_pantry New("panda")] would return a new pantry with name "panda" *)
let init_pantry exists = 
  match exists with
  | New s -> Pantry.start_pantry s
  | Old s -> let new_pantry = Pantry.start_pantry s in
    let file_name = s^"_pantry.txt" in 
    let file = open_in file_name in
    file_parse_pantry file_name file new_pantry

let init_state existing =
  {cookbook = init_cookbook existing; status = Browsing;
   pantry = init_pantry existing}

let cookbook st = 
  st.cookbook

let status st = 
  st.status

let pantry st =
  st.pantry

(** [files_in_dir name open_dir] is [true] if a file starting with [name]
    exists in the open directory [open_dir] and [false] otherwise. 

    Example: 
    [files_in_dir "panda" p] would return true if "panda" is
    a file in the open directory p *)
let rec files_in_dir name open_dir  =
  try 
    let file_name = Unix.readdir open_dir in 
    let regexp = Str.regexp_string_case_fold (name^"_book.txt") in
    (Str.string_match regexp file_name 0 || files_in_dir name open_dir)

  with 
  |End_of_file -> Unix.closedir open_dir ; false

let search_dir name d = 
  let dir = Unix.opendir d in
  if files_in_dir name dir then Old name else New name

let quit_help book pantry username = begin
  Cookbook.save_book book username;
  Pantry.save_pantry pantry username; end

let cook st =
  match st.status with
  |Browsing -> {cookbook = st.cookbook; status = Cooking 1; pantry = st.pantry}
  |Cooking n -> raise (AlreadyCooking)

let next st =
  match st.status with
  |Browsing -> raise (NotCooking)
  |Cooking n -> {cookbook = st.cookbook; status = Cooking (Read.increment n);
                 pantry = st.pantry}

let step_instr st rd =
  try 
    match st.status with
    |Browsing -> raise (NotCooking)
    |Cooking n -> Read.get_step n rd
  with
  |Read.UnknownField -> raise NoNextStep

let previous st =
  match st.status with
  |Browsing -> raise (NotCooking)
  |Cooking 1 -> raise (OnFirstStep)
  |Cooking n -> {cookbook = st.cookbook; status = Cooking (Read.decrement n);
                 pantry = st.pantry}

let cancel st = 
  match st.status with
  |Cooking n -> let x = {st with status = Browsing} in x
  |Browsing -> raise NotCooking

let add st json_name = 
  if not (Cookbook.recipe_mem st.cookbook json_name) then
    try
      let json = Yojson.Basic.from_file json_name in
      let recipe_json = Read.from_json json in
      let keys = Read.get_key_words recipe_json in
      let ingredients = Read.get_ingredients recipe_json 1 [] in
      Cookbook.add_book st.cookbook keys json_name;
      let cbook = Cookbook.add_to_recipes st.cookbook json_name in
      Pantry.add_pantry st.pantry ingredients;
      {cookbook = cbook; status = Browsing; pantry = st.pantry}

    with
    |Sys_error e -> raise (InvalidFile json_name)
  else 
    raise (AlreadyAdded json_name)

let update st item amount = 
  let quantity = begin
    match amount with
    |"none" -> Pantry.None
    |"low" -> Pantry.Low
    |"high" -> Pantry.High
    |_ -> raise (InvalidQuantity amount) end in
  Pantry.update st.pantry item quantity

let set_book st book =
  {st with cookbook = book}