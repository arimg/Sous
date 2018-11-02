
type object_phrase = string list

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

exception Empty
exception Malformed
exception NoInputIng of string
exception NoInputRecp of string
exception HasInput of string
exception TooManyInputs of string
exception TooFewInputs of string
exception ExitWhileCooking
exception CookingInput
exception BrowsingInput


(** [head lst] is the first element of [lst].
    Examples:
    - [head [0; 3; 7]] is 0
    - [head ["eat"; "apple"; "pie"]] is "eat" 
    Raises: [Malformed] if [lst] is empty. *)
let head = function
  |[] ->  raise Malformed
  |h::t -> h

(** [combine lst] is the json name resulting from combining the elements
    of [lst].
    Examples:
    - [combine ["cookies"]] is "cookies.json"
    - [combine ["apple"; "pie"]] is "apple_pie.json" *)
let rec combine lst = 
  match lst with 
  |[] -> ".json"
  |h::[] -> h^(combine [])
  |h::t -> h^"_"^(combine t) 

(** [parse_help_browsing h t] is the command formed from the players' input [h] 
    and [t] while in the Browsing state. [h] is the verb in the input; [t] is
     the words following [h].

    Requires: [h] and [t] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.)

    Raises: 
    [NoInput] if "cook" or "eat" or "update" or "search" are called 
    without any following input words from the user.
    [NoInputRecp s] if a command [s] that needs a recipe input is given no
     arguments.
    [NoInputIng s] if a command [s] that needs an ingredient input is given no
     arguments.
    [CookingInput] if a command of State.status [Cooking n] is called.
    [TooFewInputs s] if [s] has one input when it requires more.
    [HasInput s] if [s] has an input when it should not have one. 
    [TooManyInputs s] if [s] is called and followed by too many 
     input words from the user.
    [Malformed] if the user input is invalid in any other way.*) 
let parse_help_browsing h t = match t with 
  |[] -> begin match h with
      |"recommend" -> Recommend
      |"exit" -> Exit
      |"pantry" -> Pantry
      |"recipes" -> Recipes
      |"remove"|"add"|"cook" as s-> raise (NoInputRecp s)
      |"update" |"search"  |"check" |"unrecommend"|"rerecommend" as s -> raise (NoInputIng s)
      |"next" |"previous"|"cancel" -> raise CookingInput
      |_ -> raise Malformed end
  |e::[] as t -> begin match h with
      |"cook" -> Cook ([(t |> head |> String.split_on_char ' ' |> combine)])
      |"update" as s -> raise (TooFewInputs s)
      |"search" -> Search (t)
      |"check" -> Check(t)
      |"unrecommend" -> Unrecommend (t)
      |"rerecommend" -> Rerecommend (t)
      |"remove" -> Remove ([(t |> head |> String.split_on_char ' ' |> combine)])
      |"add" -> Add ([(t |> head |> String.split_on_char ' ' |> combine)])
      |"recommend" |"exit" |"pantry" |"recipes" as s -> raise (HasInput s)
      |_ -> raise Malformed end
  |e::f::[] as t -> begin match h with
      |"update" -> Update(t) 
      |"search" -> Search (t)
      |"add" -> Add ([(t |> head |> String.split_on_char ' ' |> combine)])
      |"cook" |"check" |"unrecommend" |"rerecommend"as s -> raise (TooManyInputs s)
      |"recommend" |"exit" |"pantry" |"recipes" as s -> raise (HasInput s)
      |"substitute" |"next" |"previous" |"cancel" -> raise CookingInput
      |_ -> raise Malformed end
  |t -> match h with
    |"search" -> Search (t)
    |"add" -> Add ([(t |> head |> String.split_on_char ' ' |> combine)])
    |"cook" |"check" |"update" as s -> raise (TooManyInputs s)
    |"recommend" |"exit" as s -> raise (HasInput s)
    |"substitute" |"next" |"previous" |"cancel" -> raise CookingInput
    |_ -> raise Malformed


(** [trim] is the list without the elements that equal "". 
    Examples:
    - [trim ["item"]] is ["item"]
    - [trim ["eat"; ""; ""; "apple"; "pie", ""]] is ["eat"; "apple"; "pie"] *)
let rec trim = function
  |[] -> []
  |h::t -> if h = "" then trim t else h::trim t


(** [lowercase acc lst] is [lst] with every element made lowercase.
    Examples:
    - [lowercase ["item"]] is ["item"]
    - [lowercase ["Eat"; "APPLE"; "pie"]] is ["eat"; "apple"; "pie"] *)
let rec lowercase acc = function
  |[] -> acc
  |h::t -> lowercase ((String.lowercase_ascii h)::acc) t

(** [split_commas word lst] returns a version of [lst] in which successive 
    elements are combined until one of them ends in a comma or [lst] ends, with
    accumulator [word].
    Examples:
    [split_commas "" ["recipe"] is ["recipe"]]
    [split_commas "" ["chocolate";"chips,";"eggs,";"green";"apples"]]
    is ["chocolate chips";"eggs";"green apples"]*)
let rec split_commas word lst =
  match lst with
  |[] -> []
  |h::[] -> [word^h]
  |h::t -> 
    if h = ","(* case with spaces before comma *)then word::(split_commas "" t) 
    else if Str.last_chars h 1 = "," 
    then (word^(String.sub h 0 ((String.length h) - 1)))::(split_commas "" t )
    else if word = "" then split_commas (h^" ") t
    else split_commas (word^h^" ") t

(** [parse_help_cooking h t] is the command formed from the players' input [h]
    and [t] while in the Cooking state. [h] is the verb in the input; [t] is the
     words following [h].

    Requires: [h] and [t] contain only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.)

    Raises: 
    [HasInput s] if [s] has an input when it should not have one. 
    [BrowsingInput] if a command of State.status [Browsing] is called.
    [ExitWhileCooking] if "exit" is called.
    [Malformed] if the user input is invalid in any other way.*) 
let parse_help_cooking h t = match t with
  |[] -> begin match h with
      |"next" -> Next
      |"previous" -> Previous
      |"cancel" -> Cancel
      |"exit" -> raise ExitWhileCooking
      |"recommend" |"recipes" |"pantry" -> raise BrowsingInput
      | _ -> raise Malformed end
  |t -> match h with
    |"next" |"previous" |"cancel" as s -> raise (HasInput s)
    |"cook"|"update" |"search" |"add" | "remove" 
    |"rerecommend" |"unrecommend" |"check" -> raise BrowsingInput
    |_ -> raise Malformed


(** [tail lst] returns [lst] without its first element.
    Examples:
    - [tail ["apple"]] is []]
    - [tail ["eat"; "apple"; "pie"]] is ["apple"; "pie"] 
    Raises: [Malformed] if [lst] is empty. *)
let tail = function
  |[] ->  raise Malformed
  |h::t -> t

let parse (status : State.status) str =
  if str = "" then raise Empty
  else let sub = (String.split_on_char ' ' str) |> trim |> lowercase []
                 |> List.rev in
    let sub_str = (head sub)::(split_commas "" (tail sub)) in
    match sub_str with
    |[] -> raise Empty
    |h::t -> match status with
      |Cooking i -> parse_help_cooking h t
      |Browsing -> parse_help_browsing h t