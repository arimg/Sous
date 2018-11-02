type quantity = None| Low | High
type key = Read.ingredient
type value = quantity
type t = {ingredients: (key, value) Hashtbl.t; blacklist: key list}

(* AF: The hashtable {(k1,v1);(k2,v2);...;(kn,vn)} 
     * represents the pantry with values v1,...,vn associated with 
     * keys k1,...,kn. 
     *
 * RI: When there is a duplicate key, the new value is added to a list of 
     * old values previously associated with that key. *)    

exception NotFound
exception InvalidQuantity of string
exception ItemNotFound of string
exception ItemNotFoundB of string
exception AllBlacklistOverlap
exception EmptyPantry
exception FullPantry
exception Malformed
exception BlacklistItemNotInPantry of string

(** [quantity_to_string q] returns the string that is spelled the
    same way as the quantity [q] 

    Example: 
    [quantity_to_string High] returns "high"

    [quantity_to_string Low] returns "low" *)
let quantity_to_string q  =
  match q with 
  |High -> "high"
  |Low -> "low"
  |None -> "none"

let start_pantry name = 
  let name = Hashtbl.create 5000 in {ingredients = name; blacklist = []}

let all_ings (pant: t) =
  Hashtbl.fold (fun k v acc -> k::acc) pant.ingredients []

let check (pant: t) (name: key)= 
  if not (Hashtbl.mem pant.ingredients name) 
  then
    raise (ItemNotFound name)
  else 
    Hashtbl.find pant.ingredients name

(** [groc_help k v acc] adds the [k] to the list [acc] if [v] is Low or None 

    Example:
    [groc_help "eggs" Low []] will add "eggs" to the list [acc] since its 
    value [v] is Low *)
let groc_help k v acc = 
  if ((v = Low || v = None))
  then let x = quantity_to_string v in (k,x)::acc
  else acc 

(** [groc_rem_blacklist lst pant acc] checks the grocery_list [lst] against the 
    blacklist of [pantry], removing any items in [lst] that are also in 
    the blacklist of [pantry].

    Example:
    [groc_rem_blacklist ["apples"; "eggs"] pant []] will return  *)
let rec groc_rem_blacklist lst pant acc=
  match lst with
  |[] -> acc
  |h::t -> if List.mem (fst h) pant.blacklist then groc_rem_blacklist t pant acc
    else groc_rem_blacklist t pant (h::acc)

let grocery_list (pant: t) =
  let x = Hashtbl.fold groc_help pant.ingredients []  in
  if List.length x = 0 then 
    let x = (start_pantry "a") in
    if pant.ingredients = x.ingredients then 
      raise EmptyPantry
    else 
      raise FullPantry 
  else 
    let x = groc_rem_blacklist x pant [] in
    if x = [] then raise AllBlacklistOverlap 
    else x

let update pant name q  =
  let key = String.trim name in
  if (Hashtbl.mem pant.ingredients key) 
  then
    Hashtbl.replace pant.ingredients key q
  else Hashtbl.add pant.ingredients key q

let rewrite_pantry (pant: t) (key: string) value =
  if Hashtbl.mem pant.ingredients key then ()
  else let quantity value =
         match value with
         |"none" -> None
         |"low" -> Low
         |"high" -> High
         |x ->  raise (InvalidQuantity x) in 
    Hashtbl.add pant.ingredients key (quantity value)

let save_pantry pantry username = 
  let file = open_out (username^"_pantry.txt") in
  List.iter (fun x -> Printf.fprintf file "%s" (x^" ")) pantry.blacklist;
  Printf.fprintf file "%s" "\n";
  Hashtbl.iter 
    (fun k v -> Printf.fprintf file "%s" (k^" "^(quantity_to_string v^"\n")))
    pantry.ingredients

let rec add_pantry pantry ingredients =
  match ingredients with
  |[] -> ()
  |h::t -> 
    if Hashtbl.mem pantry.ingredients h then
      add_pantry pantry t
    else Hashtbl.add pantry.ingredients h None; 
    add_pantry pantry t

let pantry_to_string pant =
  let pantry = Hashtbl.fold (fun k v acc -> acc^" - "^k^": "^(quantity_to_string v)^"\n")
      pant.ingredients "" in 
  if pantry = "" then raise EmptyPantry else pantry

let rec add_list_to_blacklist (pant: t) (lst: string list) = 
  match lst with 
  |[] -> pant
  |h::t -> add_list_to_blacklist {pant with blacklist=h::pant.blacklist} t

let add_to_blacklist (pant: t) (i: string ) = 
  if (Hashtbl.mem pant.ingredients i) then
    {pant with blacklist=i::pant.blacklist}
  else raise (BlacklistItemNotInPantry i)

(** [rem_help blacklist name acc] returns the list [acc] which contains all the 
    elements of [blacklist] except [name]. 

    Example:
    [rem_help ["eggs"; "apples"] "apples"] will return ["eggs"] *)
let rec rem_help blacklist name acc=
  match blacklist with 
  |[] -> acc
  |h::t when h = name -> rem_help t name acc
  |h::t -> rem_help t name (h::acc)

let rem_from_blacklist (pant: t) (name: string ) = 
  {pant with blacklist = rem_help pant.blacklist name []}


(** [user_input_quan pant key] prompts the user to update the value of [key]
    stored in [pantry] and updates binding of [key] in the hashtable of [pantry]
    to the quantity that matches the user's input. 

    Example:
    [user_input_quan pant "eggs"] will update the quantity of "eggs" to High
    in pantry [pant] if the user inputs the quantity High *)
let rec user_input_quan pant key = 
  print_string(">");
  match read_line () with
  | exception End_of_file -> raise Malformed
  | value ->
    match value with
    |"none" -> None
    |"low" -> Low
    |"high" -> High
    |"" ->  Hashtbl.find pant.ingredients key 
    |x -> print_string ("Sorry, "^x^" isn't a valid quantity.
  Please try again with 'high', 'low', or 'none'. \n> ");
      user_input_quan pant key

let rec post_cook_update (pant: t) (ings) = 
  match ings with 
  |[] -> print_endline ("Thank you for updating your pantry.\n>")
  |h::t ->
    print_endline("Please update "^h^" in your pantry");
    try 
      let x = user_input_quan pant h
      in 
      update pant h x; post_cook_update pant t
    with
    |NotFound ->
      let x = user_input_quan pant h in 
      Hashtbl.add pant.ingredients h x

(** [lst_add acc x] adds [x] to the front of [acc] 

    Example: 
    [lst_add ["apples"] "pie"] will return ["pie"; "apples"] *)
let lst_add acc x =
  x::acc

let get_blacklist pant = 
  List.fold_left lst_add [] pant.blacklist