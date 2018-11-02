
(** [quantity] is the measurement of how much of a
    given ingredient is currently in the pantry *)
type quantity = None | Low | High

(** [key] is the key in pantry *)
type key = Read.ingredient

(** [value] is the quantity of an item in the pantry *)
type value = quantity

(** [t] is the type of a pantry *)
type t 

(** Raised when a key can not be found in the hashtable *)
exception NotFound

(** Raised when a string representation of a quantity is misspelled *)
exception InvalidQuantity of string

(** Raised when an unknown ingredient term is encountered in the hashtable *)
exception ItemNotFound of string

(** Raised when an unknown ingredient term is encountered in the blacklist *)
exception ItemNotFoundB of string

(** Raised when all items in the pantry bound to values of Low or None are 
    on the blacklist and the user calls the [recommend] function *)
exception AllBlacklistOverlap

(** Raised when the current pantry has no ingredients and the user
    calls the [recommend] function *)
exception EmptyPantry

(** Raised when all items in the pantry are bound to values of High and the user
    calls the [recommend] function *)
exception FullPantry

(** Raised when the user input is malformed *)
exception Malformed

(** Raised when an item is trying to be added to the blacklist but is not
    currently in the hashtable *)
exception BlacklistItemNotInPantry of string

(** [start_pantry name] creates a new pantry of type t with name [name]

    Example:
    [start_pantry "panda"] returns a new pantry of type t called "panda" *) 
val start_pantry: string -> t

(** [all_ings pant] returns a list of all the ingredients currently in [pant]

    Example:
    [all_ings pant] returns ["eggs"; "apples"] if "eggs" and "apples" are 
    ingredients in pantry [pant] *)
val all_ings: t -> key list

(** [grocery_list pant] returns a list of tuples of all the ingredients
    currently in [pant] that have values of Low or None and their accompanying 
    value.

    Example:
    [grocery_list pant] returns [("apples", "low"); ("eggs", "none")] if 
    ingredients "apples" and "eggs" have quantities "low" and "none", 
    respectively in pantry [pant] *)
val grocery_list: t -> (key*string) list

(** [update pant name q] changes the value of [name] to [q] in [pant].

    Example: 
    [update pant "eggs" High changes the value of "eggs" to High in pantry 
    [pant] *)
val update: t -> key -> value -> unit

(** [rewrite_pantry pant key value] re-creates a given pantry from strings
    pulled from a [.txt] file and returns a unit *)
val rewrite_pantry : t -> string -> string -> unit

(** [save_pantry pantry username] saves [pant] in a file titled [username.txt]
    and returns a unit *)
val save_pantry : t -> string -> unit


(** [add_pantry pantry lst] adds any new ingredients in
    [lst] to the hashtable of [pantry] with initial values of None.

    Example: 
    [add_pantry pantry ["egg";"milk"]], where "egg" and "milk" are strings
    would return [pantry], where [pantry] now contains "egg" and "milk" *)
val add_pantry : t -> key list -> unit

(** [check pantry ing] returns the value of [ing] in [pantry].

    Example: 
    [check pantry "egg"], where "egg" is a string and the hashtable in 
    [pantry] has a binding for "egg", would return the quantity bound to "egg"
    in the hashtable. *)
val check : t -> key -> value

(** [pantry_to_string pantry] returns a string representation of [pantry] where 
    the string has each key in the hashtable of [pantry] on a new line,
    preceded by a dash, followed by a colon and
    a list of the values bound to the given key. 

    Example: 
    [pantry_to_string pantry], [pantry] contains "egg" valued at Low and
    "milk" valued at High would return the string
    "- egg: low
     - milk: high" *)
val pantry_to_string: t -> string

(** [add_list_to_blacklist pantry lst] adds the ingredients of [lst]
    to the blacklist of [pantry].

    Example: 
    [add_list_to_blacklist pantry ["egg";"milk"]], where "egg" and "milk" are
    strings would return [pantry], where the blacklist of [pantry] now
    contains "egg" and "milk" *)
val add_list_to_blacklist: t -> string list -> t

(** [add_to_blacklist pantry ing] adds the ingredient [ing]
    to the blacklist of [pantry].

    Example: 
    [add_to_blacklist pantry "egg"], where "egg" is a string would return
    [pantry], where the blacklist of [pantry] now contains "egg" *)
val add_to_blacklist: t -> string -> t

(** [add_to_blacklist pantry ing] removes the ingredient [ing]
    from the blacklist of [pantry].

    Example: 
    [add_to_blacklist pantry "egg"], where "egg" is a string in the blacklist of
    [pantry] would return [pantry], where the blacklist of [pantry]
    no longer contains "egg" *)
val rem_from_blacklist: t -> string -> t

(** [post_cook_update pantry lst] promts the user to update the values of each 
    ingredient of [lst] stored in [pantry].

    Example: 
    [post_cook_update pantry ["egg";"milk"], where "egg" and "milk" are strings
    would ask the user to first update "egg" and store the given value
    in [pantry] and then ask the user to update "milk" and store the
    given value in [pantry] *)
val post_cook_update : t -> string list -> unit

(** [get_blacklist pantry] returns the ingredients currently in the blacklist of 
    [pantry].

    Example: 
    [get_blacklist pantry], where "egg" is a string in the blacklist of
    [pantry] would return "egg" *)
val get_blacklist : t -> key list 