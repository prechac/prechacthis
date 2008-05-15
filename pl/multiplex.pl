
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



permutateMultiplexes([],[]).
permutateMultiplexes([Head|Tail],[NewHead|NewTail]) :-
	permutateMultiplexes(Tail,NewTail),
	is_list(Head),!,
	permutation(Head,NewHead).
permutateMultiplexes([Head|Tail],[Head|NewTail]) :- 
	permutateMultiplexes(Tail,NewTail).