(*Problem Set 1*)

(*Problem 1*)
(*returns if a list of ints is monotonically increasing *)
let rec is_mon_inc(lst: int list):bool=
	match lst with
	[]->true
	|[x]->true
	|h::t-> if h> List.hd t then false else is_mon_inc(t)



(*Problem 2*)
(*returns if a list of ints is monotonically decreasing*)
let rec is_mon_dec (lt :int list):bool = 
	match lt with
	[]->true
	|[x]->true
	|h::t-> if h< List.hd t then false else is_mon_dec(t)
(*returns if a list of ints is uniomodal*)
let rec is_unimodal (lst:int list) :bool=
	match lst with 
	[]-> true
	|[x]-> true
	|h::t-> if h< List.hd t then is_unimodal(t)
	else is_mon_dec(t)
	
(*Problem 3*)
(*prepends an int to every int list in an int list list *)
let rec prepend_all (lst :int list list) (x: int): int list list=
	match lst with 
	[]->[[x]]
	|[xs]-> [x::xs]::[xs]
	|h::t ->(x::h) :: h :: prepend_all t x

(*returns the power set of an int list*)
let rec powerset (lst: int list) :int list list =
	match lst with
	[]->[]
	|[x]-> [[];[x]]
	|h::t -> prepend_all (powerset t) h



(*Problem 4*)
let rec str_to_list (str :string) :char list=
	match str with
	""->[]
	|_-> (String.get str 0)::(str_to_list (String sub 1 ((String.length str)-1)))

let rec char_to_str_list (lst :char list) :string list=
	match lst with
	[]->[]
	|[x]-> [Char.escaped x]
	|h::t -> (Char.escaped h) :: char_to_str_list t

let rec list_to_str (lst :string list) :string =
	match lst with 
	[]->""
	|[x]->x
	|h::t-> h ^ list_to_str t


let rec rev_int (x:int) :int=
	let lst= char_to_str_list (str_to_list (string_of_int  x)) in
	match lst with 
	[x]-> int_of_string x
	|h::t-> if h ="-" then int_of_string (list_to_str ("-"::List.rev t)) else int_of_string(list_to_str(List.rev lst));;
