test_allPassingSiteswaps4 :- 
	allPassingSiteswaps(2, 4, 4, 4, 0, 0, _, [[[1,p(1)]]], [[[0],[1,1]]], [[]], [[]]).

printTest_allPassingSiteswaps4 :- 
	format("allPassingSiteswaps(2, 4, 4, 4, 0, 0, _, [[[1,p(1)]]], [[[0],[1,1]]], [[]], [[]]).").
	
test_allPassingSiteswaps6 :- 
	allPassingSiteswaps(2, 4, 6, 4, 0, 1, _, [[[1,p(1)],[p(3)]]], [[[0],[1,1],[2]]], [[]], [[]]).

printTest_allPassingSiteswaps6 :- 
	format("allPassingSiteswaps(2, 4, 6, 4, 0, 1, _, [[[1,p(1)],[p(3)]]], [[[0],[1,1],[2]]], [[]], [[]]).").


test_print_pattern_info :-
	print_pattern_info([p(2.6, 2, 4), p(2.3, 1, 5), p(2.3, 1, 5), p(2, 0, 2)], 3).
	
printtest_print_pattern_info :-
	format("print_pattern_info([p(2.6, 2, 4), p(2.3, 1, 5), p(2.3, 1, 5), p(2, 0, 2)], 3).").