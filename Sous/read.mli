(** The abstract type of values representing read *)
type t

(** [instruction] is the type of the instruction from the read file *)
type instruction = string

(** [ingredient] is the type of the ingredient from the read file *)
type ingredient = string

(** [key] is the type of the key from the read file*)
type key = string

(** [step_num] is the type of the step number from the read file*)
type step_num = int

(** [name] is the type of the name from the read file *)
type name = string

(** [cuisine] is the type of the cuisine from the read file *)
type cuisine = string

(** [course] is the type of the course from the read file *)
type course = string

(** [servings] is the type of the servings from the read file *)
type servings = int

(** Raised when an unknown field is encountered *)
exception UnknownField

(** Raised when an invalid JSON file is encountered *)
exception InvalidJSON

(** Raised when the JSON has an empty string as the recipe name *)
exception EmptyName

(** Raised when the JSON has no name field *)
exception NoName

(** Raised when the JSON does not have a name of type string *)
exception InvalidNameType

(** Raised when the JSON has a value less than or equal to 0 for servings *)
exception InvalidServings

(** Raised when the JSON has no servings field *)
exception NoServings

(** Raised when the JSON does not have servings of type int *)
exception InvalidServingType

(** Raised when the JSON has an empty string as the recipe cuisine *)
exception EmptyCuisine

(** Raised when the JSON has no cuisine field *)
exception NoCuisine

(** Raised when the JSON does not have a cuisine of type string *)
exception InvalidCuisineType

(** Raised when the JSON has an empty string as the recipe course *)
exception EmptyCourse

(** Raised when the JSON has no course field *)
exception NoCourse

(** Raised when the JSON does not have a course of type string *)
exception InvalidCourseType

(** Raised when the JSON step has no number field *)
exception NoStepNum

(** Raised when the JSON does not have a step number of type int *)
exception InvalidStepNumType

(** Raised when the JSON steps don't start with 1 or increment by 1 each time *)
exception InvalidStepNums

(** Raised when the JSON has an empty string as the recipe instruction *)
exception EmptyStepInstr

(** Raised when the JSON step has no instruction field *)
exception NoStepInstr

(** Raised when the JSON step does not have instructions of type string *)
exception InvalidStepInstrType

(** Raised when the JSON step has no ingredients field *)
exception NoStepIng

(** Raised when the JSON step does not have ingredients of type string list *)
exception InvalidStepIngsType

(** Raised when the JSON has an empty list as the recipe steps *)
exception EmptySteps

(** Raised when the JSON has no steps field *)
exception NoSteps

(** Raised when the JSON does not have steps of type list *)
exception InvalidStepsType

(** [from_json json] is the recipe that [json] represents.
    Requires: [json] is a valid JSON recipe representation. 

    Example: 
    [from_json "apple_pie.json"] will return the recipe of 
    "apple_pie.json" *)
val from_json : Yojson.Basic.json -> t

(** [get_step step_num rd] returns the step of [step_num] from recipe [rd] 

    Example: 
    [get_step 1 apple] will return 
    "In a small bowl, combine the sugars, flour and spices; set aside.
    In a large bowl, toss apples with lemon juice. Add sugar mixture; 
    toss to coat." (the first step of the apple pie recipe) if [apple] 
    is a read type of "apple_pie.json" *)
val get_step : int -> t -> instruction

(** [get_step_ingredients step_num rd] returns the ingredients 
    of [step_num] from recipe [rd] 

    Example: 
    [get_step_ingredients 3 apple] will return 
    ["egg"] (the ingredients associated with the third step of the apple pie 
    recipe) if [apple] is a read type of "apple_pie.json" *)
val get_step_ingredients : int -> t -> ingredient list

(** [get_key_words assoc_lst] returns all of the values from recipe [assoc_lst]
    that will be used as keys in the hashtable representation 

    Example: 
    [get_key_words creme] will return ["dessert"; "french"; 
    "heavy cream"; "vanilla bean"; "vanilla sugar"; "egg"; "water"] if
    [creme] is a read type of "creme_brulee.json" *)
val get_key_words : t -> key list

(** [get_name assoc_lst] returns the name of the recipe [assoc_lst] 

    Example: 
    [get_name creme] will return "creme brulee" if [creme] 
    is a read type of "creme_brulee.json" *)
val get_name : t -> name

(** [increment step_num] returns the value of [step_num] incremented by 1 

    Example: 
    [increment 1] returns 2 *)
val increment : step_num -> step_num

(** [decrement step_num] returns the value of [step_num] decremented by 1 

    Example: 
    [increment 2] returns 1*)
val decrement : step_num -> step_num

(** [get_ingredients assoc_lst step acc] returns a list of all of the 
    ingredients from recipe [assoc_lst] 

    Example: 
    [get_ingredients creme 1 []] will return ["heavy cream"; 
    "vanilla bean"; "vanilla sugar"; "egg"; "water"] if
    [creme] is a read type of "creme_brulee.json" *)
val get_ingredients : t -> int -> ingredient list list -> ingredient list

(** [get_cuisine assoc_lst] returns the cuisine of the recipe [assoc_lst] 

    Example: 
    [get_cuisine creme] will return "french" if [creme] 
    is a read type of "creme_brulee.json"*)
val get_cuisine : t -> cuisine

(** [get_course assoc_lst] returns the course of the recipe [assoc_lst] 

    Example: 
    [get_course creme] will return "dessert" if [creme] 
    is a read type of "creme_brulee.json"*)
val get_course : t -> course

(** [get_servings assoc_lst] returns the servings of the recipe [assoc_lst] 

    Example: 
    [get_servings creme] will return 6 if [creme] is a read type 
    of "creme_brulee.json"*)
val get_servings : t -> servings

(** [check rd] returns true if [rd] is based off of a valid JSON file and false
    otherwise

    Example: 
    [check creme] returns true if [creme] is a read type of
    "creme_brulee.json" since "creme_brulee.json" is a valid recipe JSON *)
val check : t -> bool