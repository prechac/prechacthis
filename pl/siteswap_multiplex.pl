:- module(siteswap_multiplex, 
	[
		multiplex/2,
		multiplex/3,
		permutateMultiplexes/2,
		orderMultiplexes/2,
		checkMultiplexes/1,
		noMultiplex/1
	]
).

:- use_module(helpers).
:- use_module(siteswap_helpers).
:- use_module(siteswap_constraints).


multiplex(OldPattern, NewPattern) :-
	length(OldPattern, Length),
	Length >= 3,

	between(1,Length,FirstIndex),
	nth1(FirstIndex, OldPattern, OldFirstThrow),
	number(OldFirstThrow),
	OldFirstThrow > 2,
	
	SecondIndex is ((FirstIndex + 1) mod Length) + 1,
	nth1(SecondIndex, OldPattern, OldSecondThrow),
	number(OldSecondThrow), % unnecessary
	OldSecondThrow > 0,
	
	FirstMultiplexThrow is OldFirstThrow - 2,
	SecondMultiplexThrow is OldSecondThrow,
	Multiplex = [FirstMultiplexThrow,SecondMultiplexThrow],
	% Multiplex is m(OldSecondThrow, SecondMultiplexThrow),

	nth1(FirstIndex, NewPattern, 2),
	nth1(SecondIndex, NewPattern, Multiplex),

	fillIn(OldPattern, NewPattern).

multiplex(Pattern, Pattern, 0).
multiplex(OldPattern, NewPattern, NumberOfMultiplexes) :-
	NumberOfMultiplexes > 0,
	multiplex(OldPattern, TempNewPattern),
	RemainingNumberOfMultiplexes is NumberOfMultiplexes - 1,
	multiplex(TempNewPattern, NewPattern, RemainingNumberOfMultiplexes).



permutateMultiplexes([],[]) :- !.
permutateMultiplexes([Head|Tail],[NewHead|NewTail]) :-
	is_list(Head),!,
	permutateMultiplexes(Tail,NewTail),
	permutation(Head,NewHead).
permutateMultiplexes([Head|Tail],[Head|NewTail]) :- 
	permutateMultiplexes(Tail,NewTail).


orderMultiplexes([],[]) :- !.
orderMultiplexes([Multiplex|Pattern], [MultiplexOrderedDesc|NewPattern]) :-
	is_list(Multiplex), !,
	msort(Multiplex, MultiplexOrdered),
	reverse(MultiplexOrdered, MultiplexOrderedDesc),	
	orderMultiplexes(Pattern, NewPattern).
orderMultiplexes([Throw|Pattern], [Throw|NewPattern]) :-
	orderMultiplexes(Pattern, NewPattern).
	
checkMultiplexes([]) :- !.
checkMultiplexes([Multiplex|Pattern]) :-
	is_list(Multiplex), !,
	(is_set(Multiplex); listOf(p(2,0,2), Multiplex)),
	dontcontain(Multiplex, [p(0,0,0)]),
	checkMultiplexes(Pattern).
checkMultiplexes([_Throw|Pattern]) :-
	checkMultiplexes(Pattern).
	
	
noMultiplex([]) :- !.
noMultiplex([p(_,_,_)|Pattern]) :-
	noMultiplex(Pattern).