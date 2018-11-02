(** [object_phrase] is a list of the strings that
    follow the command in the user's input *)
type object_phrase = string list

(** [command] is the command to be execited based on the user's input *)
type command = 
  |Cook of object_phrase
  |Next
  |Previous
  |Update of object_phrase
  |Search of object_phrase
  |Add of object_phrase
  |Check of object_phrase
  |Unrecommend of object_phrase
  |Rerecommend of object_phrase
  |Recommend 
  |Remove of object_phrase
  |Pantry
  |Recipes
  |Cancel
  |Exit


(** Raised when a user's input is empty
    Example:
    [parse [Browsing] ""] is [Empty] *)
exception Empty

(** Raised when a user's input is malformed
    Example:
    [parse [Browsing] "checkk"] is [Malformed] *)
exception Malformed

(** Raised when a user's input is has a command but too few arguments
    Example:
    [parse [Browsing] "update egg"] is [TooFewInputs "update"] *)
exception TooFewInputs of string

(** Raised when a user's input is has a command but too many arguments
    Example:
    [parse [Browsing] "check apples, butter"] is [TooManyInputs "check"] *)
exception TooManyInputs of string

(** Raised when a user's input needs an ingredient input but is given no 
    arguments
    Example:
    [parse [Browsing] "check"] is [NoInputIng "check"] *)
exception NoInputIng of string

(** Raised when a user's input needs a recipe input but is given no arguments
    Example:
    [parse [Browsing] "cook"] is [NoInputRecp "cook"] *)
exception NoInputRecp of string

(** Raised when a user types a command that requires no inputs but has some
    Example:
    [parse [Cooking 2] "cancel"] is [HasInput "cancel"]  *)
exception HasInput of string

(** Raised when a user tries to call the exit command while in cooking state 
    Example:
    [parse [Cooking 2] "exit"] is [ExitWhileCooking] *)
exception ExitWhileCooking

(** Raised when a user tries to call a cooking command while in browsing state 
    Example:
    [parse [Browsing] "next"] is [CookingInput] *)
exception CookingInput

(** Raised when a user tries to call a browsing command while in cooking state
    Example:
    [parse [Cooking 2] "pantry"] is [BrowsingInput] *)
exception BrowsingInput

(** [parse status str] parses the user's input (passed in as a string) and
    returns the [Command] found in the user's input. 
    Examples:
    [parse Browsing "exit"] is [Exit] 
    [parse Browsing "update apples, high"] is [Update [apples; high]] 
    [parse Cooking "next"] is [Next] *)
val parse: State.status -> string -> command

