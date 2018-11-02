type key = Read.key
type value = string list
type t = {book : (key, value) Hashtbl.t; recipes : string list}

(* AF: The hashtable {(k1,v1);(k2,v2);...;(kn,vn)} 
     * represents the cookbook with values v1,...,vn associated with 
     * keys k1,...,kn. 
     *
 * RI: When there is a duplicate key, the new value is added to a list of 
     * old values previously associated with that key. *)    

exception UnknownSearchTerm of string

exception UnknownIngredient of string

exception IngWithNoRecipe

exception EmptyCookbook

exception UnknownRecipe of string

let start_cook_book name = 
  let name = Hashtbl.create 5000 in 
  {book = name; recipes = []}

let rec add_book cbook ingredients (json_name : string) =
  let book = cbook.book in
  match ingredients with
  |[] -> ()
  |h::t -> 
    begin if Hashtbl.mem book h then
        let old_val = Hashtbl.find book h in
        Hashtbl.replace book h (json_name::old_val);
        add_book {book = book; recipes = cbook.recipes} t json_name

      else (Hashtbl.add book h [json_name];
            add_book {book = book; recipes = cbook.recipes} t json_name)
    end

let rewrite_book book key val_lst =
  Hashtbl.add book.book key val_lst

let rec search (book: t) key_list acc = 
  match key_list with
  |[] -> List.sort_uniq String.compare acc
  |h::t -> try search book t ((Hashtbl.find (book.book) h)@acc)
    with
    |Not_found -> raise (UnknownIngredient h)

(** [list_to_string lst acc] transforms [lst] from type list to type string

    Example input: [list_to_string ["hello";"Kenneth"] ""] would return
    "hello\nKenneth " where the \n would move Kenneth to a new line *)
let rec list_to_string lst acc =
  match lst with 
  |[] -> "\n"
  |h::[] -> acc^h^"\n"
  |h::t -> list_to_string t (acc^h^" ")

let save_book book username = 
  let file = open_out (username^"_book.txt") in
  let recipe = list_to_string book.recipes ""  in
  Printf.fprintf file "%s" recipe;
  Hashtbl.iter 
    (fun k v -> Printf.fprintf file "%s" (k^" "^(list_to_string v ""))) book.book


(** [lst_rem v acc name] returns a version of [v] that does not include [name].

    Examples:
    [lst_rem ["chocolate"; "pizza"] [] "chocolate"] is ["pizza"] *)
let rec lst_rem v acc name=
  match v with 
  |[] -> acc
  |h::t when h = name -> lst_rem t acc name
  |h::t -> lst_rem t (h::acc) name


let remove_book cookbook name =
  if List.mem name cookbook.recipes then 
    let rem_help k v = begin
      if List.mem name v then  
        match v with
        |[]-> raise IngWithNoRecipe (*should not occur based on what [book] is*)
        |h::[]-> Hashtbl.replace cookbook.book k []
        |h::t->
          let lst = lst_rem v [] name in 
          Hashtbl.replace cookbook.book k lst
      else 
        () end in
    Hashtbl.iter (rem_help) cookbook.book;
    let x = lst_rem cookbook.recipes [] name in
    let cookbook={book= cookbook.book; recipes=x} in cookbook
  else raise (UnknownRecipe name)

let all_recipes book = 
  book.recipes

let add_to_recipes bk recipe = 
  {book = bk.book; recipes = recipe::bk.recipes}

(** [book_check book name] returns a boolean. The boolean is false if [name] does
    not appear in any bindinng of any key in the hashtable of [book]. If [name]
    occurs at least once in any any bindinng of any key in the hashtable of [book],
    then the boolean is true 

    Example input: [book_check [book] "chocolate_chip_cookies.json"] would return
    true if one of the bindins in [book] contained "chocolate_chip_cookies.json"
    and false otherwise *)
let book_check book name= 
  let b = Hashtbl.fold (fun k v acc -> (List.mem name v)::acc) book [] in
  (List.mem true b)

let recipe_mem cookbook recipe =
  (List.mem recipe cookbook.recipes)&&(book_check cookbook.book recipe)
