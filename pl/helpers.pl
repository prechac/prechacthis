%cleanRotations([], []).
%cleanRotations([Head| Tail], CleanBag) :-
%    cleanRotations(Tail, CleanTail),
%    joinRotation(Head, CleanTail, CleanBag).
%
%joinRotation(Head, BagWithHead, BagWithHead) :- 
%    containsARotation(BagWithHead, Head), !.
%joinRotation(Head, BagWithOutHead, [Head | BagWithOutHead]).
%
%containsARotation([Head| _Tail], Elem) :- 
%    rotate(Head, Elem), !.
%containsARotation([_Head| Tail], Elem) :- 
%    containsARotation(Tail, Elem).



rotate(List, Rotated) :-
    append(Left, Right, List),
	length(Right,LengthRight),
	LengthRight > 0,
    append(Right, Left, Rotated).

rotate_left([Head|Tail], Rotated) :-
	append(Tail, [Head], Rotated),!.

rotate_right(List, Rotated) :-
	append(Liat, [Daeh], List),
	append([Daeh], Liat, Rotated),!.


sort_list_of_expr(List, Sorted) :-
	predsort(compare_expr,List,Sorted).

compare_expr(=,R1,R2) :-
	R1 is R2.
compare_expr(<,R1,R2) :-
	R1 < R2.
compare_expr(>,R1,R2) :-
	R1 > R2.

even(Int) :-
	0 is Int mod 2.
odd(Int) :- 
	1 is abs(Int) mod 2.
	
infimum(_,[]).
infimum(Inf, [Head|Tail]) :-
	Inf =< Head,
	infimum(Inf, Tail).
	
min_of_list(Min, List) :-
	member(Min, List),
	infimum(Min, List).

supremum(_,[]).
supremum(Sup, [Head|Tail]) :-
	Sup >= Head,
	supremum(Sup, Tail).

max_of_list(Max, List) :-
	member(Max, List),
	supremum(Max, List).
	
rational_to_number(Number, Number) :-	
	number(Number),!.
rational_to_number(Rational, Number) :-
	rational(Rational),
	Number is float(Rational).
		

zeros(0, []).
zeros(Length, Zeros) :-
  Length > 0,
  OneShorter is Length - 1,
  zeros(OneShorter, Oneless),
  append([0], Oneless, Zeros).

