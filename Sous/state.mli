(** [status] is [Cooking n] if the user is cooking and is on step [n].
    [status] is [Browsing] otherwise *)
type status = Cooking of int | Browsing

(** [existing] is [Old s] for a returning user with username [s].
    [existing] is [New s] for a new user with username [s]. *)
type existing = Old of string | New of string

(** [t] is the type of a state *)
type t

(** [AlreadyCooking] is raised if the user calls [cook] while status is
    [Cooking n]. *)
exception AlreadyCooking

(** [NotCooking] is raised if the user calls a cook command while status is
    [Browsing]. *)
exception NotCooking

(** [AddQuantity] is raised if the user fails to input a quantity. *)
exception AddQuantity

(** [NoNextStep] is raised if the user calls [next] on a finished recipe. *)
exception NoNextStep

(** [InvalidFile s] is raised if the user inputs an invalid file [s]. *)
exception InvalidFile of string

(** [InvalidQuantty s] is raised if the user inputs an invalid quantity [s]. *)
exception InvalidQuantity of string

(** [OnFirstStep] is raised if the user calls [previous] when they are on the 
    first step of a recipe *)
exception OnFirstStep

(** [AlreadyAdd s] is raised if the user inputs a recipe [s] that is already in
    the cookbook *)
exception AlreadyAdded of string

(** [init_state existing] is an initial state based on whether [existing] is
    [Old s] or [New s] *)
val init_state : existing  -> t

(** [search_dir name d] is the appropriate [existing] value based on whether or 
    not [name] is already in [d]. 

    Example: 
    [search_dir "panda" "."] will return [Old "panda"] if 
    "panda" is in the current directory, and will return [New "panda"] 
    otherwise *)
val search_dir : string -> string -> existing

(** [cookbook st] is the cookbook associated with [st] 

    Example: 
    [cookbook st] will return [panda], if 
    st = {cookbook : panda; status : status; pantry: Pantry.t} *)
val cookbook : t -> Cookbook.t

(** [pantry st] is the pantry associated with [st]

    Example: 
    [pantry st] will return [panda], if 
    st = {cookbook : Cookbook.t; status : status; pantry: panda} *) 
val pantry : t -> Pantry.t

(** [status st] is the status associated with [st] 

    Example: 
    [status st] will return [Browsing], if 
    st = {cookbook : Cookbook.t; status : Browsing; pantry: Pantry.t} *)
val status : t -> status

(** [cook st] is [st] with status changed to [Cooking n] 

    Example: 
    [cook st] will return 
    {cookbook : Cookbook.t; status : Cooking n; pantry: Pantry.t}, if st 
    originally was 
    {cookbook : Cookbook.t; status : Browsing; pantry: Pantry.t} *)
val cook : t -> t

(** [next st] is [st] with status changed to [Cooking n+1] 

    Example: 
    [next st] will return 
    {cookbook : Cookbook.t; status : Cooking 2; pantry: Pantry.t}, if st 
    originally was 
    {cookbook : Cookbook.t; status : Cooking 1; pantry: Pantry.t} *)
val next : t -> t

(** [step_instr st rd] is the instruction from [rd] associated with the
    current step 

    Example: 
    [step_instr st rd] will return "Bake at 375° for 25 minutes.
    Remove foil and bake until crust is golden brown and filling is bubbly, 
    20-25 minutes longer. Cool on a wire rack." if st is 
    {cookbook : Cookbook.t; status : Cooking 4; pantry: Pantry.t}
    and rd contains "apple_pie.json" *)
val step_instr : t -> Read.t -> Read.instruction

(** [previous st] is [st] with status changed to [Cooking n-1]  

    Example: 
    [previous st] will return 
    {cookbook : Cookbook.t; status : Cooking 2; pantry: Pantry.t}, if st 
    originally was 
    {cookbook : Cookbook.t; status : Cooking 1; pantry: Pantry.t} *)
val previous : t -> t

(** [quit_help book pantry username] saves the cookbook [book] and pantry
    [pantry] for future use and returns a unit *)
val quit_help : Cookbook.t -> Pantry.t -> string -> unit

(** [cancel st] is [st] with status changed to [Browsing] 

    Example: 
    [cancel st] will return 
    {cookbook : Cookbook.t; status : Browsing; pantry: Pantry.t}, if st 
    originally was 
    {cookbook : Cookbook.t; status : Cooking 1; pantry: Pantry.t} *)
val cancel : t -> t

(** [add st json_name] adds the recipe in [json_name] and its
    ingredients to [book] and [pantry] *)
val add : t -> string -> t

(** [update st item amount] updates the pantry in [st] to have value [amount]
    associated with key [item] and returns a unit 

    Example input: [update st "egg" "high"] *)
val update : t -> Pantry.key -> string -> unit

(** [set_book st book] returns a new state that has the same status and pantry
    as [st] but with a new cookbook [book].

    Example input: [update st book] returns {[book], st.status, st.pantry} *)
val set_book: t -> Cookbook.t -> t