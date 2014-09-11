open Assertions
open PS1


(*Problem 1*)

TEST_UNIT "is_mon_inc1" =
	let bool1 = is_mon_inc [1;3;5;6]
	let bool2 = true
	assert_true (matches bool1 bool2)


TEST_UNIT "is_mon_inc2" = 
	let bool1 = is_mon_inc [1;4;2;3]
	let bool2 = false
	assert_true (matches bool1 bool2)


TEST_UNIT "is_mon_inc3" = 
	let bool1 = is_mon_inc [1]
	let bool2 = true
	assert_true (matches bool1 bool2)



TEST_UNIT "is_mon_inc4" = 
	let bool1 = is_mon_inc []
	let bool2 = true
	assert_true (matches bool1 bool2)


TEST_UNIT "is_mon_inc5" =
	let bool1 = is_mon_inc [1;1;1;1]
	let bool2 = true
	assert_true (matches bool1 bool2)


(*Problem 4*)

TEST_UNIT "rev_int1" =
	let int1 = rev_int 123
	let int2 = 321
	assert_true (matches int1 int2)


TEST_UNIT "rev_int2" =
	let int1 = rev_int 1
	let int2 = 1
	assert_true (matches int1 int2)


TEST_UNIT "rev_int3" =
	let int1 = rev_int -351
	let int2 = -153
	assert_true (matches int1 int2)


TEST_UNIT "rev_int4" =
	let int1 = rev_int 10
	let int2 = 1
	assert_true (matches int1 int2)


(*Problem 5*)

TEST_UNIT "unflatten1" =
	let list1 = unflatten 2 [5;3;6;1;5;2;7]
	let list2 = Some [[5;3];[6;1];[5;2];[7]]
	assert_true (matches list1 list2)


TEST_UNIT "unflatten2" =
	let list1 = unflatten 2 []
	let list2 = Some [[]]
	assert_true (matches list1 list2)


TEST_UNIT "unflatten3" =
	let list1 = unflatten 0 [5;3;6;1;5;2;7]
	let list2 = None
	assert_true (matches list1 list2)


TEST_UNIT "unflatten4" =
	let list1 = unflatten (-1) [5;3;6;1;5;2;7]
	let list2 = None
	assert_true (matches list1 list2)
