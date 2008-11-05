


pattern_to_string(Pattern, PatternStr) :-
	list_to_string(Pattern, PatternStr).
list_to_string(Pattern, PatternStr) :-
	format(atom(TempStr), "~w", [Pattern]),
	string_to_list(TempStr, TempLst),
	remove_whitespace(TempLst, PatternStr).




pStyle(_Length, _Origen, 0, self) :- !.
pStyle(Length, Origen, _Index, classic) :- 
	even(Length),
	odd(Origen).
pStyle(Length, Origen, _Index, equi) :-
	even(Length),
	even(Origen).
pStyle(Length, Origen, Index, bi) :-
	odd(Length),
	even(Origen), 
	odd(Index).
pStyle(Length, Origen, Index, bi) :-
	odd(Length),
	odd(Origen), 
	even(Index).
pStyle(Length, Origen, Index, instantbi) :-
	odd(Length),
	even(Origen),
	even(Index).
pStyle(Length, Origen, Index, instantbi) :-
	odd(Length),
	odd(Origen),
	odd(Index).

