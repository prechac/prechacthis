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

cleanEquals([],[]).
cleanEquals([Head|Tail], CleanBag) :-
	containsEqual(Tail, Head),!,
	cleanEquals(Tail,CleanBag).
cleanEquals([Head|Tail], [Head|CleanBag]) :-
	cleanEquals(Tail, CleanBag).

containsEqual([Head|_Tail],Siteswap) :-
	areEqual(Siteswap,Head),!.
containsEqual([_Head|Tail],Siteswap) :-
	containsEqual(Tail,Siteswap).

areEqual(P1,P2) :-
	permutateMultiplexes(P1, PTemp),
	rotate(PTemp,P2).

listOfHights([],[]).
listOfHights([Throw|Siteswap],[Hight|List]) :-
	hight(Throw,Hight),
	listOfHights(Siteswap,List).

compare_hights(Order,P1,P2) :-
	listOfHights(P1,H1),
	listOfHights(P2,H2),
	compare(Order,H1,H2).

is_biggest(Siteswap,List) :-
	member(Siteswap,List),
	is_bigger_than_list(Siteswap,List).

is_bigger_than_list(_Siteswap,[]).
is_bigger_than_list(Siteswap,[Head|Tail]) :-
	compare_hights(Order,Siteswap,Head),
	Order \= <,
	is_bigger_than_list(Siteswap,Tail).

rotateHighestFirst(Siteswap,Rotated) :-
	findall(R,rotate(Siteswap,R),ListOfRotations),
	is_biggest(Rotated,ListOfRotations).

	
permutateMultiplexes([],[]).
permutateMultiplexes([Head|Tail],[NewHead|NewTail]) :-
	permutateMultiplexes(Tail,NewTail),
	is_list(Head),!,
	permutation(Head,NewHead).
permutateMultiplexes([Head|Tail],[Head|NewTail]) :- 
	permutateMultiplexes(Tail,NewTail).


rotate(List, Rotated) :-
    append(Left, Right, List),
	length(Right,LengthRight),
	LengthRight > 0,
    append(Right, Left, Rotated).

%int_val(Int, Int) :-
%   number(Int),
%   integer(Int).
%
%int_val(Float, Int) :-
%   number(Float),
%   0.0 is float_fractional_part(Float),
%   Int is floor(Float).


even(Int) :-
	0 is Int mod 2.
odd(Int) :- 
	1 is abs(Int) mod 2.
	
