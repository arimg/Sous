(** [t] is the type of a cookbook *)
type t

(** [key] is the key in cookbook *)
type key = string

(** [value] is the value in the cookbook *)
type value = string list

(** Raised when an unknown search term is encountered *)
exception UnknownSearchTerm of key

(** Raised when an unknown ingredient term is encountered *)
exception UnknownIngredient of string

(** Raised when the cookbook is empty *)
exception EmptyCookbook

(** Raised when there is an ingredient with no recipe associated with it *)
exception IngWithNoRecipe

(** Raised when the recipe name is unknown when the user tries to remove
    the recipe *)
exception UnknownRecipe of string

(** [start_cook_book name] is a new cookbook named [name]

    Example:
    [start_cook_book "panda"] returns a new cookbook of type t called "panda" *)
val start_cook_book : string -> t

(** [search book key_list acc] finds value associated with [key_list] 
    in cookbook [book]

    Example: 
    [search book ["flour"] []], where "chocolate_chip_cookies.json" is a recipe
    in cookbook [book], would return ["chocolate_chip_cookies.json"] *)
val search : t -> key list -> value -> value

(** [rewrite_book book key val_lst] is an updated version of [book]
    all of the values of [val_lst] with key [key] have been added and 
    returns a unit *)
val rewrite_book : t -> key -> value -> unit

(** [save_book book username] saves the cookbook [book] into a file 
    name [username]_book.txt and returns a unit *)
val save_book : t -> string -> unit

(** [add_book book ingredients json_name] adds the recipe [json_name] to the
    hashtable with keys [ingredients] in cookbook [book] and returns a unit *)
val add_book : t -> Read.key list -> string -> unit

(** [remove_book cookbook name] removes the recipe [name] from the 
    cookbook [book] and returns a cookbook 

    Example: [remove_book book "apple_pie.json"], where "apple_pie.json" is a 
    recipe in cookbook [book], would return cookbook [book] but with the recipe
    "apple_pie.json" removed from it *)
val remove_book : t -> string -> t

(** [all_recipes book] returns the list of recipes in cookbook [book]

    Example: [all_recipes book], where "apple_pie.json" and "mac_and_cheese.json"
    are recipes in cookbook [book], would return 
    ["apple_pie.json"; "mac_and_cheese.json"] *)
val all_recipes: t -> string list

(** [add_to_recipes bk recipe] returns cookbook [bk] with recipe [recipe] added
    to it 

    Example: [add_to_recipes book "apple_pie.json"] returns cookbook [book]
    with "apple_pie.json" added to its recipes *)
val add_to_recipes : t -> string -> t

(** [recipe_mem book recipe] returns true if recipe [recipe] is in cookbook
    [book] and returns false otherwise

    Example: [recipe_mem book "apple_pie.json"], where "apple_pie.json" is a 
    recipe in cookbook [book], would return true *)
val recipe_mem: t -> string -> bool