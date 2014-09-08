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
	|[xs]-> [x::xs]@[xs]
	|h::t ->(x::h) :: h :: prepend_all t x

(*returns the power set of an int list*)
let rec powerset (lst: int list) :int list list =
	match lst with
	[]->[]
	|[x]-> [[];[x]]
	|h::t -> prepend_all (powerset t) h



(*Problem 4*)
let rec str_to_list (str :string) :string list=
	match str with
	""->[]
	| _-> (String.sub str 0 1)::(str_to_list (String.sub str 1 ((String.length str)-1)))

let rec list_to_str (lst :string list) :string =
	match lst with 
	[]->""
	|[x]->x
	|h::t-> h ^ list_to_str t


let rec rev_int (x:int) :int=
	let lst= str_to_list (string_of_int  x) in
	match lst with 
	[]-> 0
	|[r]-> int_of_string r
	|h::t-> if h ="-" then int_of_string (list_to_str ("-"::List.rev t)) 
	else int_of_string(list_to_str(List.rev lst))
	
	
(*Problem 5*)
let rec unflatten_helper ((k:int), (lst: 'a list), (counter:int), (sublist: 'a list)) : 'a list list =
	match lst with
	[] -> [sublist]
	| h::t -> 
		if ((float_of_int(counter/k) = (float_of_int(counter) /. float_of_int(k))) && counter > 0) then
			sublist::unflattenhelper(k,List.tl lst, counter+1, [(List.hd lst)])
		else
			unflattenhelper(k,(List.tl lst), (counter+1), (sublist @ [(List.hd lst)]))
	


let unflatten ((k:int), (lst: 'a list)) :'a list list option =
	if k <= 0 then None else Some (unflatten_helper(k,lst,0,[]))


(*Problem 6*)
￼type numeral = I | V | X | L | C | D | M 
 type roman = numeral list
 let rec int_of_roman (r : roman) : int = let int_of_numeral = function
| I -> 1
| V -> 5
| X -> 10
| L -> 50
| C -> 100
| D -> 500
| M -> 1000 in

match r with 
[]->0
|[x]-> int_of_numeral x
|h::t-> if h>= List.hd t then int_of_numeral h + int_of_roman(t)
else (int_of_numeral (List.hd t)) - (int_of_numeral h) + (int_of_roman (List.tl t));;

