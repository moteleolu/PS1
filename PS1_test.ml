(*tests for ps1*)
open Assertions
open Ps1

(*Problem 1*)

TEST_UNIT "is_mon_inc1" =
	let bool1 = is_mon_inc [1;3;5;6] in
	let bool2 = true in
	assert_true (bool1 = bool2)


TEST_UNIT "is_mon_inc2" = 
	let bool1 = is_mon_inc [1;4;2;3] in
	let bool2 = false in
	assert_true (bool1 = bool2)


TEST_UNIT "is_mon_inc3" = 
	let bool1 = is_mon_inc [1] in
	let bool2 = true in
	assert_true (bool1 = bool2)



TEST_UNIT "is_mon_inc4" = 
	let bool1 = is_mon_inc [] in
	let bool2 = true in
	assert_true (bool1 = bool2)


TEST_UNIT "is_mon_inc5" =
	let bool1 = is_mon_inc [1;1;1;1] in
	let bool2 = true in
	assert_true (bool1 = bool2)


(*Problem 2*)
TEST_UNIT "is_unimodal_test"=
let p= is_unimodal [1;2;3;4;5;4;3;2] in
let s= true in
assert_true (p=s)

TEST_UNIT "is_unimodal_test_2"=
let p= is_unimodal [1;3;5;7;5;6] in
let s= false in
assert_true (p=s)

TEST_UNIT "is_unimodal_test_3"=
let p= is_unimodal [] in
let s= true in
assert_true (p=s)

TEST_UNIT "is_unimodal_test4"=
let p= is_unimodal [1;1;1] in
let s= true in
assert_true (p=s)

TEST_UNIT "is_unimodal_test5"=
let p= is_unimodal [1;2] in
let s= true in
assert_true (p=s)

TEST_UNIT "is_unimodal_test6"=
let p= is_unimodal [2;1;(-1)] in
let s= true in
assert_true (p=s)

(*Problem 3*)
TEST_UNIT "powerset_test1"=
let p= powerset [1;2;3;4] in
let s= [[1; 2; 3]; [2; 3]; [1; 3]; [3]; [1; 2]; [2]; [1]; []; [1; 2; 3; 4];[2; 3; 4]; [1; 3; 4]; [3; 4]; [1; 2; 4]; [2; 4]; [1; 4]; [4]] in
assert_true (p=s)

TEST_UNIT "powerset_test2"=
let p= powerset [] in
let s= [] in
assert_true (p=s)

TEST_UNIT "powerset_test3"=
let p= powerset [1] in
let s= [[]; [1]] in
assert_true (p=s)

(*Problem 4*)

TEST_UNIT "rev_int1" =
	let int1 = rev_int 123 in
	let int2 = 321 in
	assert_true (int1 = int2)


TEST_UNIT "rev_int2" =
	let int1 = rev_int 1 in
	let int2 = 1 in
	assert_true (int1 = int2)


TEST_UNIT "rev_int3" =
	let int1 = rev_int (-351) in
	let int2 = (-153) in
	assert_true (int1 = int2)


TEST_UNIT "rev_int4" =
	let int1 = rev_int 10 in
	let int2 = 1 in
	assert_true (int1 = int2)


(*Problem 5*)

TEST_UNIT "unflatten1" =
	let list1 = unflatten 2 [5;3;6;1;5;2;7] in
	let list2 = Some [[5;3];[6;1];[5;2];[7]] in
	assert_true (list1 = list2)


TEST_UNIT "unflatten2" =
	let list1 = unflatten 2 [] in
	let list2 = Some [[]] in
	assert_true (list1= list2)


TEST_UNIT "unflatten3" =
	let list1 = unflatten 0 [5;3;6;1;5;2;7] in
	let list2 = None in
	assert_true (list1 =list2)


TEST_UNIT "unflatten4" =
	let list1 = unflatten (-1) [5;3;6;1;5;2;7] in
	let list2 = None in
	assert_true (list1 =list2)

(*Problem 6*)
TEST_UNIT "int_of_roman_test1"=
let p= int_of_roman [I;X] in
let s= 9 in
assert_true (p=s)

TEST_UNIT "int_of_roman_test2"=
let p= int_of_roman [M;M;X;I;V] in
let s= 2014 in
assert_true (p=s)

TEST_UNIT "int_of_roman_test3"=
let p= int_of_roman [] in
let s= 0 in
assert_true (p=s)
