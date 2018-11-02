type t = (string * Yojson.Basic.json) list
type instruction = string
type ingredient = string
type key = string
type step_num = int
type name = string
type cuisine = string
type course = string
type servings = int

exception UnknownField
exception InvalidJSON
exception EmptyName
exception NoName
exception InvalidNameType
exception InvalidServings
exception NoServings
exception InvalidServingType
exception EmptyCuisine
exception NoCuisine
exception InvalidCuisineType
exception EmptyCourse
exception NoCourse
exception InvalidCourseType
exception NoStepNum
exception InvalidStepNumType
exception InvalidStepNums
exception EmptyStepInstr
exception NoStepInstr
exception InvalidStepInstrType
exception NoStepIng
exception InvalidStepIngsType
exception EmptySteps
exception NoSteps
exception InvalidStepsType

let from_json json = 
  Yojson.Basic.Util.to_assoc json

(** [find_step property expected assoc_lst] finds the step of step number equal 
    to [expected] in the recipe [assoc_lst] and returns the [property] of that 
    step

    Examples: 
    [find_step "instructions" 1 creme] returns 
    "Preheat the oven to 325 degrees F." (the instruction associated with 
    step 1) if [creme] is a read type of "creme_brulee.json"

    [find_step "ingredients" 2 creme] returns 
    ["heavy cream"; "vanilla bean"] (the ingredients associated with 
    step 2) if [creme] is a read type of "creme_brulee.json" *)
let rec find_step property expected assoc_lst =
  try
    match assoc_lst with
    |[] -> raise (UnknownField)
    |h::t -> let step = Yojson.Basic.Util.to_assoc h in 
      if (List.assoc "step_num" step |> Yojson.Basic.Util.to_int) = expected 
      then List.assoc property step
      else find_step property expected t
  with 
  |Not_found -> raise InvalidJSON

(** [json_to_string_lst acc jlist] transforms [jlst] from type json list to 
    type string list 

    Example: 
    [json_to_string_lst [] json["panda"]] will return ["panda"], where
    [json["panda"]] is type json list *)
let rec json_to_string_lst acc jlist = 
  match jlist with
  |[] -> acc
  |h::t -> json_to_string_lst ((Yojson.Basic.Util.to_string h)::acc) t

let get_step step_num rd =
  try
    rd |> List.assoc "steps" |> Yojson.Basic.Util.to_list 
    |> find_step "instructions" step_num |> Yojson.Basic.Util.to_string
  with 
  |Not_found -> raise InvalidJSON

let get_step_ingredients step_num rd =
  try
    rd |> List.assoc "steps" |> Yojson.Basic.Util.to_list
    |> find_step "ingredients" step_num |> Yojson.Basic.Util.to_list
    |> json_to_string_lst []
  with 
  |Not_found -> raise InvalidJSON

let rec get_ingredients assoc_lst step acc = 
  try
    let new_step = step + 1 in
    let step_ings =  get_step_ingredients step assoc_lst in
    get_ingredients assoc_lst new_step (step_ings::acc)
  with
  |UnknownField -> acc |> List.flatten |> List.sort_uniq String.compare

let get_key_words assoc_lst = 
  try
    let cuisine = assoc_lst |> List.assoc "cuisine" 
                  |> Yojson.Basic.Util.to_string in 
    let course = assoc_lst |> List.assoc "course" 
                 |> Yojson.Basic.Util.to_string in 
    let ingredients = get_ingredients assoc_lst 1 [] in 
    cuisine::course::ingredients
  with 
  |Not_found -> raise InvalidJSON

let get_cuisine assoc_lst = 
  try
    assoc_lst |> List.assoc "cuisine" |> Yojson.Basic.Util.to_string
  with 
  |Not_found -> raise InvalidJSON

let get_servings assoc_lst = 
  try
    assoc_lst |> List.assoc "servings" |> Yojson.Basic.Util.to_int
  with 
  |Not_found -> raise InvalidJSON

let get_course assoc_lst = 
  try
    assoc_lst |> List.assoc "course" |> Yojson.Basic.Util.to_string
  with 
  |Not_found -> raise InvalidJSON

let get_name assoc_lst =
  try
    assoc_lst |> List.assoc "name" |> Yojson.Basic.Util.to_string
  with 
  |Not_found -> raise InvalidJSON

let increment step_num = step_num + 1

let decrement step_num = step_num - 1

(** [ings_to_string lst acc] returns a string of the ingredients in [lst]

    Example:
    [ings_to_string ["eggs"; "apples"] ""] returns "eggs, apples" *)
let rec ings_to_string (lst: ingredient list) acc=
  match lst with
  |[] -> acc
  |h::t -> ings_to_string t (h^", "^acc)

(** [check_name rd] ensures that there is a valid "name" field in [rd]

    Examples: 
    [check_name creme] returns true since there is a valid "name" field if
    [creme] is a read type of "creme_brulee.json"

    Raises: 
    [EmptyName] if the "name" field is an empty string; 
    [NoName] if the "name" field is missing; 
    [InvalidNameType] if the "name" field is an incorrect type for 
    "name" *)
let check_name rd = 
  try
    let name = rd |> List.assoc "name" |> Yojson.Basic.Util.to_string in 
    if name = "" then raise EmptyName
    else true
  with
  |Not_found -> raise NoName
  |Yojson.Basic.Util.Type_error (s, j)-> raise InvalidNameType

(** [check_servings rd] ensures that there is a valid "servings" field in [rd]

    Examples: 
    [check_servings creme] returns true since there is a valid "servings" field 
    if [creme] is a read type of "creme_brulee.json"

    Raises: 
    [InvalidServings] if the "servings" field is less than or equal to zero; 
    [NoServings] if the "servings" field is missing; 
    [InvalidServingType] if the "servings" field is an incorrect type for 
    "servings" *)
let check_servings rd = 
  try
    let servings = rd |> List.assoc "servings" |> Yojson.Basic.Util.to_int in
    if servings <= 0 then raise InvalidServings
    else true
  with
  |Not_found -> raise NoServings
  |Yojson.Basic.Util.Type_error (s, j)-> raise InvalidServingType

(** [cuisine_check rd] ensures that there is a valid "cuisine" field in [rd]

    Example: 
    [cuisine_check creme] returns true since there is a valid "cuisine" field if
    [creme] is a read type of "creme_brulee.json"

    Raises: 
    [EmptyCuisine] if the "cuisine" field is an empty string; 
    [NoCuisine] if the "cuisine" field is missing; 
    [InvalidCuisineType] if the "cuisine" field is an incorrect type for 
    "cuisine" *)
let cuisine_check rd =
  try 
    let name = rd |> List.assoc "cuisine" |> Yojson.Basic.Util.to_string in 
    if name = "" then raise EmptyCuisine
    else true
  with
  |Not_found -> raise NoCuisine
  |Yojson.Basic.Util.Type_error (s, j)-> raise InvalidCuisineType

(** [course_check rd] ensures that there is a valid "course" field in [rd]

    Example: 
    [course_check creme] returns true since there is a valid "course" field if
    [creme] is a read type of "creme_brulee.json"

    Raises: 
    [EmptyCourse] if the "course" field is an empty string; 
    [NoCourse] if the "course" field is missing; 
    [InvalidCourseType] if the "course" field is an incorrect type for 
    "course" *)
let course_check rd = 
  try
    let name = rd |> List.assoc "course" |> Yojson.Basic.Util.to_string in 
    if name = "" then raise EmptyCourse
    else true
  with
  |Not_found -> raise NoCourse
  |Yojson.Basic.Util.Type_error (s, j)-> raise InvalidCourseType

(** [step_nums step_lst] retrieves the step numbers from each step in 
    the recipe and returns them as an int list

    Example: 
    [step_nums creme] returns [1; 2; 3; 4] if it is based on the recipe 
    "creme_brulee.json" 

    Raises: 
    [NoStepNum] if the "step_num" field is missing; 
    [InvalidStepNumType] if the "step_num" field is an incorrect type for 
    "step_num" *)
let rec step_nums step_lst = 
  try
    match step_lst with
    |[] -> []
    |h::t -> let num = h |> Yojson.Basic.Util.to_assoc |> List.assoc "step_num" 
                       |> Yojson.Basic.Util.to_int in 
      num::(step_nums t)
  with
  |Not_found -> raise NoStepNum
  |Yojson.Basic.Util.Type_error (s, j)-> raise InvalidStepNumType

(** [check_nums prev lst] ensures that the steps are numbered properly, starting
    with step 1 and proceeding in numerical order

    Example: 
    [check_nums 0 [1; 2; 3]] returns true since the integers in the list start
    at one and proceed in numerical order

    Raises: 
    [InvalidStepNums] if the integers from [lst] are not in numerical order *)
let rec check_nums prev lst = 
  match lst with
  |[] -> true
  |h::t -> if h = prev + 1 then check_nums h t
    else raise InvalidStepNums

(** [step_instrs steps] ensures that there is a valid "instructions" field in 
    [steps]

    Example: 
    [step_instrs creme_steps] returns true since there is a valid "instructions" 
    field if [creme_steps] is a JSON list of the steps from "creme_brulee.json"

    Raises: 
    [EmptyStepInstr] if the "instructions" field is an empty string in a step; 
    [NoStepInstr] if the "instructions" field is missing from a step; 
    [InvalidStepInstrType] if the "instructions" field is an incorrect type for 
    "instructions" *)
let rec step_instrs steps = 
  try
    match steps with
    |[] -> true
    |h::t -> let instr = h |> Yojson.Basic.Util.to_assoc |> List.assoc "instructions" 
                         |> Yojson.Basic.Util.to_string |> String.trim in 
      if instr = "" then raise EmptyStepInstr
      else step_instrs t
  with
  |Not_found -> raise NoStepInstr
  |Yojson.Basic.Util.Type_error (s, j)-> raise InvalidStepInstrType

(** [check_step_ings ings] returns true if there is no error converting each 
    element in [ings] to a string type

    Raises: 
    Yojson.Basic.Util.Type_error (s, j) if there is an error converting an element 
    in [ings] to a string *)
let rec check_step_ings ings =
  match ings with
  |[] -> true
  |h::t -> ignore (h |> Yojson.Basic.Util.to_string);
    check_step_ings t

(** [step_ings steps] ensures that there is a valid "ingredients" field in 
    [steps]

    Example: 
    [step_ings creme_steps] returns true since there is a valid "ingredients" 
    field if [creme_steps] is a JSON list of the steps from "creme_brulee.json"

    Raises: 
    [NoStepIng] if the "ingredients" field is missing from a step; 
    [InvalidStepIngsType] if the "ingredients" field is an incorrect type for 
    "ingredients"  *)
let rec step_ings steps = 
  try
    match steps with
    |[] -> true
    |h::t -> ignore (h |> Yojson.Basic.Util.to_assoc |> List.assoc "ingredients" 
                     |> Yojson.Basic.Util.to_list |> check_step_ings);
      step_instrs t
  with
  |Not_found -> raise NoStepIng
  |Yojson.Basic.Util.Type_error (s, j)-> raise InvalidStepIngsType

(** [check_steps rd] ensures that there is a valid "steps" field in [rd]

    Example: 
    [check_steps creme] returns true since there is a valid "steps" field if
    [creme] is a read type of "creme_brulee.json"

    Raises: 
    [EmptySteps] if the "steps" field is an empty list; 
    [NoSteps] if the "steps" field is missing; 
    [InvalidStepsType] if the "steps" field is an incorrect type for "steps" *)
let check_steps rd = 
  try
    let steps = rd |> List.assoc "steps" |> Yojson.Basic.Util.to_list in
    if steps = [] then raise EmptySteps else 
      let num = steps |> step_nums |> check_nums 0 in 
      let instrs = steps |> step_instrs in
      let ings = steps |> step_ings in
      num && instrs && ings
  with
  |Not_found -> raise NoSteps
  |Yojson.Basic.Util.Type_error (s, j)-> raise InvalidStepsType

let check rd = 
  check_steps rd && course_check rd && cuisine_check rd && check_servings rd &&
  check_name rd