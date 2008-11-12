
preprocessConstraint(ConstraintString, Period, NumberOfJugglers, MaxHeight, Constraint) :-
	preprocessConstraint(ConstraintString, positiv, Period, NumberOfJugglers, MaxHeight, Constraint).


preprocessConstraint(ConstraintString, ConstraintType, Period, NumberOfJugglers, MaxHeight, Constraint) :-
	string2Constraint(ConstraintString, ConstraintBagShort),
	member(ConstraintShort, ConstraintBagShort),
	convertShortPasses(ConstraintShort, ConstraintType, Period, NumberOfJugglers, MaxHeight, Constraint).


string2Constraint(ConstraintString, Constraint) :-
	(
		dcg_constraint(Constraint, ConstraintString, []);
		throw(constraint_unclear)
	),!.

preprocessMultiplexes(Pattern) :-
	preprocessMultiplexes(Pattern, Pattern, 0).

preprocessMultiplexes([], _OrigPattern, _Position) :- !.
preprocessMultiplexes([Multiplex|Tail], OrigPattern, Position) :-
	is_list(Multiplex), !,
	length(Multiplex, Length),
	set2s(Length, Length, OrigPattern, Position),
	NewPosition is Position + 1,
	preprocessMultiplexes(Tail, OrigPattern, NewPosition).
preprocessMultiplexes([_Head|Tail], OrigPattern, Position) :-
	NewPosition is Position + 1,
	preprocessMultiplexes(Tail, OrigPattern, NewPosition).
	
set2s(1, _, _, _) :- !.
set2s(Distance, MultiplexLength, OrigPattern, Position) :-
	length(OrigPattern, Period),
	PositionOf2 is Period - 1 - ((Period - 1 - Position + (Distance - 1) * 2) mod Period),
	calcThe2(Distance, MultiplexLength, The2),
	nth0(PositionOf2, OrigPattern, The2),
	%(
	%	nth0(PositionOf2, OrigPattern, p(2,0,2));
	%	(
	%		nth0(PositionOf2, OrigPattern, Multiplex),
	%		member(p(2,0,2), Multiplex)    %% what happens in second round (2 has to be member twice) !!!
	%	),!
	%),
	NewDistance is Distance - 1,
	set2s(NewDistance, MultiplexLength, OrigPattern, Position).
	
calcThe2(MultiplexLength, MultiplexLength, p(2,0,2)) :- !.
calcThe2(Distance, MultiplexLength, The2) :-
	Length is MultiplexLength - Distance + 1,
	listOf(p(2,0,2), Length, The2).
	
% --- preprocess numbers --- %

preprocess_number(Constraint, Number) :-
	preprocess_number(Constraint, Number, []).

preprocess_number([], Number, Options) :-
	select(default(Default), Options, NewOptions),!,
	preprocess_number(Default, Number, NewOptions).
preprocess_number(Var, Var, Options) :-
	var(Var), !,
	memberchk(dontknow, Options, [name(to_come), optional]).
preprocess_number(Number, Number, Options) :-
	number(Number), !,
	memberchk(0, Options, [name(to_come), optional]).
preprocess_number(Number, Number, Options) :-
	rational(Number), !,
	memberchk(0, Options, [name(to_come), optional]).
preprocess_number(Constraint, Number, Options) :-
	is_list(Constraint), !,
	string2Numbers(Constraint, NumberConstraint),
	preprocess_number_options(NumberConstraint, NumberConstraintWorkedOn, Options),
	member_NC(Number, NumberConstraintWorkedOn, Options).
preprocess_number(Atom, Number, Options) :-
	atom(Atom),!,
	name(Atom, String),
	preprocess_number(String, Number, Options).

string2Numbers(ConstraintString, Constraint) :-
	(
		dcg_number_constraint(Constraint, ConstraintString, []);
		throw(constraint_unclear)
	),!.

preprocess_number_options(Constraint, Constraint, []) :- !.
preprocess_number_options(Constraint, NewConstraint, [Option|Options]) :-
	preprocess_number_option(Constraint, ConstraintTmp, Option),
	preprocess_number_options(ConstraintTmp, NewConstraint, Options).

preprocess_number_option(Constraint, NewConstraint, max(Max)) :-
	numlist(1, Max, List),
	MaxConstraint = [list(List)],
	dcgh_merge_number_constraints_and(Constraint, MaxConstraint, NewConstraint), !.
preprocess_number_option(Constraint, Constraint, _) :- !.	

member_NC(_Number, _NumberConstraint, Options) :-
	memberchk(stop_if(Goal), Options),
	call(Goal), !,
	fail.
member_NC(Number, NumberConstraint, Options) :-
	memberchk(infinity(_Min), NumberConstraint), !,
	member_NC_infinity(Number, NumberConstraint, Options),
	memberchk(infinity, Options, [name(to_come), optional]).
member_NC(Number, NumberConstraint, Options) :-
	not(memberchk(infinity(_Min), NumberConstraint)),
	memberchk(list(List), NumberConstraint),
	length(List, Length),
	sort(List, ListSorted),
	member_NC_list(Number, ListSorted, Length, Options).
	
member_NC_list(Number, List, Length, Options) :-
	nth1_restricted(Pos, List, Number, Options),
	ToCome is Length - Pos,
	memberchk(ToCome, Options, [name(to_come), optional]).

member_NC_infinity(Number, NumberConstraint, Options) :-
	memberchk(list(List), NumberConstraint),
	sort(List, ListSorted),
	member_restricted(Number, ListSorted, Options).
member_NC_infinity(Number, NumberConstraint, Options) :-
	memberchk(infinity(Min), NumberConstraint),
	integer_gen(Min, Number, Options).

%%% --- short passes ---

float_to_shortpass(Throw, ShortPass) :-
	(number(Throw);rational(Throw)),!,
	ShortPass is round(Throw * 10)/10.
float_to_shortpass(p(Throw,Index,Original), p(ShortPass,Index,Original)) :-
	float_to_shortpass(Throw, ShortPass).

float_to_shortpass([],[]).
float_to_shortpass([Throw|Rest],[ShortPass|RestShort]) :-
	float_to_shortpass(Throw,ShortPass),
	float_to_shortpass(Rest, RestShort).

shortpass_to_pass(ShortPass,_,_,_,ShortPass) :- var(ShortPass),!.
shortpass_to_pass(Self, _, _, _, p(Self, 0, Self)) :- integer(Self).
shortpass_to_pass(p(Self, Zero, _), _, _, _, p(Self, Zero, Self)) :- 
	%nonvar(Zero),
	Zero = 0,
	integer(Self).
shortpass_to_pass(p(ShortThrow), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	shortpass_to_pass(p(ShortThrow, _, _), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)),
	Index > 0.
shortpass_to_pass(p(ShortThrow, Index), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	integer(Index),
	shortpass_to_pass(p(ShortThrow, Index, _), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)).
shortpass_to_pass(p(ShortThrow, Index, Origen), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	(number(ShortThrow); rational(ShortThrow)),
	Prechator is Length rdiv Jugglers,
	IndexMax is Jugglers - 1,
	MaxHeightSolo is MaxHeight + Length,
	float_to_shortpass(ShortThrow, ShortThrowShortend),
	between(1, IndexMax, Index),
	between(1, MaxHeightSolo, Origen),
    Throw is Origen - (Jugglers - Index) * Prechator,
	float_to_shortpass(Throw, ShortThrowShortend). 
shortpass_to_pass(p(Var), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	var(Var),
	shortpass_to_pass(p(Var, Index), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)).
shortpass_to_pass(p(Var, Zero), _, _, MaxHeight, p(Self, Zero, Self)) :-
	var(Var),
	Zero = 0,
	between(0, MaxHeight, Self).
shortpass_to_pass(p(Var, Index), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	var(Var),
	Prechator is Length rdiv Jugglers,
	IndexMax is Jugglers - 1,
	MaxHeightSolo is MaxHeight + Length,
	between(1, IndexMax, Index),
	between(0, MaxHeightSolo, Origen),
    Throw is Origen - (Jugglers - Index) * Prechator,
	Throw >= 1,
	Throw =< MaxHeight.


shortpass_to_pass_dont(ShortPass,_,_,_,ShortPass) :- var(ShortPass), !.
shortpass_to_pass_dont(Self, _, _, _, p(Self, 0, Self)) :- integer(Self), !.
shortpass_to_pass_dont(p(Self, Zero, Self), _, _, _, p(Self, Zero, Self)) :- 
	nonvar(Zero),
	Zero = 0,
	integer(Self),!.
shortpass_to_pass_dont(p(ShortThrow), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	shortpass_to_pass_dont(p(ShortThrow, _, _), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)).
shortpass_to_pass_dont(p(ShortThrow, Index), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	shortpass_to_pass_dont(p(ShortThrow, Index, _), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)).
shortpass_to_pass_dont(p(ShortThrow, Index, Origen), Length, Jugglers, MaxHeight, p(Throw, NewIndex, NewOrigen)) :-
	number(ShortThrow),
	Prechator is Length rdiv Jugglers,
	IndexMax is Jugglers - 1,
	MaxHeightSolo is MaxHeight + Length,
	float_to_shortpass(ShortThrow, ShortThrowShortend),
	(nonvar(Index) -> NewIndex = Index; true),
	(nonvar(Origen) -> NewOrigen = Origen; true),
	between(1, IndexMax, Index),
	between(1, MaxHeightSolo, Origen),
    Throw is Origen - (Jugglers - Index) * Prechator,
	float_to_shortpass(Throw, ShortThrowShortend), !.

	
convertShortPasses(ShortPass,Length,Persons,Max,Pass) :-
	convertShortPasses(ShortPass,positiv,Length,Persons,Max,Pass).
convertShortPassesDont(ShortPass,Length,Persons,Max,Pass) :-
	convertShortPasses(ShortPass,negativ,Length,Persons,Max,Pass).

convertShortPasses(Var,positiv,_,_,_,Var) :- var(Var), !.
convertShortPasses([],_,_,_,_,[]) :- !.
convertShortPasses([HeadShort|TailShort],ConstraintType,Length,Persons,Max,[Head|Tail]) :-
	convertShortPasses(HeadShort,ConstraintType,Length,Persons,Max,Head),
	convertShortPasses(TailShort,ConstraintType,Length,Persons,Max,Tail).
convertShortPasses(ShortPass,positiv,Length,Persons,Max,Pass) :-
	not(is_list(ShortPass)),
	shortpass_to_pass(ShortPass,Length,Persons,Max,Pass).
convertShortPasses(ShortPass,negativ,Length,Persons,Max,Pass) :-
	not(is_list(ShortPass)),!,
	(
		shortpass_to_pass_dont(ShortPass,Length,Persons,Max,Pass);
		Pass is -1 %% unconvertable
	),!.



%%% Siteswap Constraints Grammar rules %%%

% Bedingung -> Pattern [and Pattern]*
% Bedingung -> Pattern [or  Pattern]*
% Pattern   -> [Wurf | (Bedingung) ]+
% Wurf -> ...

% (4 4 and 1 (1p or 2p)) or 4 4 1
% ==> [[[4,4],[1,p(1)]],[[4,4],[1,p(2)]],[[4 4 1]]]

% [] --> [[[p(1)]]]
% [[[p(1)]]] --> [[[p(1)]],[[p(2)]]]
% [[[p(1)]],[[p(2)]]] --> [[[1,p(1)]],[[1,p(2)]]]
% [[[1,p(1)]],[[1,p(2)]]] --> [[[4,4],[1,p(1)]],[[4,4],[1,p(2)]]]
% [[[4,4],[1,p(1)]],[[4,4],[1,p(2)]]] --> [[[4,4],[1,p(1)]],[[4,4],[1,p(2)]],[[4 4 1]]]


% (1 and 2) (3 or 4): [] --> [[[3]],[[4]]] --> [[[1,3],[2,3]],[[1,4],[2,4]]]


%%% DCG Grammar %%%

dcg_constraint([[]]) -->
	dcg_whitespaces.
dcg_constraint(Constraint) -->
	dcg_pattern(Pattern),
	dcg_and_patterns(Patterns),
	{
		dcgh_merge_constraints(Pattern, Patterns, Constraint, and)
	}.
dcg_constraint(Constraint) -->
	dcg_pattern(Pattern),
	dcg_or_patterns(Patterns),
	{
		dcgh_merge_constraints(Pattern, Patterns, Constraint, or)
	}.

dcg_pattern(Constraint) -->
	dcg_whitespaces,
	dcg_throw_or_constraint(Throw),
	dcg_whitespaces,
	dcg_throws_or_constraints(Pattern),
	dcg_whitespaces,
	{
		dcgh_merge_constraints(Throw, Pattern, Constraint, concat)
	}.
	
dcg_and_patterns(NewConstraint) -->
	dcg_and,
	dcg_pattern(Pattern),
	dcg_and_patterns(Patterns),
	{
		dcgh_merge_constraints(Pattern, Patterns, NewConstraint, and)
	}.
dcg_and_patterns([]) -->
	[].
	
dcg_or_patterns(NewConstraint) -->
	dcg_or,
	dcg_pattern(Pattern),
	dcg_or_patterns(Patterns),
	{
		dcgh_merge_constraints(Pattern, Patterns, NewConstraint, or)
	}.
dcg_or_patterns([]) -->
	[].

dcg_throw_or_constraint([[[Throw]]]) -->
	dcg_throw(Throw).
dcg_throw_or_constraint(Multiplex) -->
	dcg_multiplex(Multiplex).
dcg_throw_or_constraint(Constraint) -->
	dcg_left_parenthesis,
	dcg_constraint(Constraint),
	dcg_right_parenthesis.
	
dcg_throws_or_constraints(NewConstraint) -->
	dcg_whitespace,
	dcg_whitespaces,
	dcg_throw_or_constraint(Constraint),
	dcg_throws_or_constraints(Constraints),
	{
		dcgh_merge_constraints(Constraint, Constraints, NewConstraint, concat)
	}.
dcg_throws_or_constraints([]) -->
	[].
	
dcg_multiplex(Multiplex) -->
	dcg_left_bracket,
	dcg_constraint(Constraint),
	dcg_right_bracket,
	{
		dcgh_constraint_to_multiplex(Constraint, Multiplex)
	}.


dcg_throw(T) -->
	dcg_self(T).
dcg_throw(T) -->
	dcg_pass(T).
	
dcg_self(p(_S,0)) -->
	dcg_underscore,
	dcg_s.	
dcg_self(p(S,0)) -->
	dcg_integer(S),
	dcg_s.
dcg_self(S) -->
	dcg_integer(S).
dcg_self(_) -->
	dcg_underscore.

dcg_pass(p(T,I,O)) -->	
	dcg_p,
	dcg_left_parenthesis,
	dcg_float(T),
	dcg_comma,
	dcg_integer(I),
	dcg_comma,
	dcg_integer(O),
	dcg_right_parenthesis.
dcg_pass(p(T,I)) -->	
	dcg_p,
	dcg_left_parenthesis,
	dcg_float(T),
	dcg_comma,
	dcg_integer(I),
	dcg_right_parenthesis.
dcg_pass(p(F,I)) -->
	dcg_number(F),
	dcg_p,
	dcg_integer(I).
dcg_pass(p(F))  -->
	dcg_number(F),
	dcg_p.
dcg_pass(p(F))  -->
	dcg_number(F),
	dcg_p,
	dcg_underscore.
dcg_pass(p(_))  -->
	dcg_underscore,
	dcg_p.
dcg_pass(p(_))  -->
	dcg_underscore,
	dcg_p,
	dcg_underscore.
dcg_pass(p(_,I))  -->
	dcg_underscore,
	dcg_p,
	dcg_integer(I).
dcg_pass(_) -->
	dcg_underscore.






dcg_number_constraint([]) -->
	dcg_whitespaces.
dcg_number_constraint(NewListOfNumbers) -->
	dcg_whitespaces,
	dcg_numbers(ListOfNumbers),
	dcg_whitespaces,
	dcg_and_numbers(AndListOfNumbers),
	{
		dcgh_merge_number_constraints_and(ListOfNumbers, AndListOfNumbers, NewListOfNumbers)
	}.
dcg_number_constraint(NewListOfNumbers) -->	
	dcg_whitespaces,
	dcg_numbers(ListOfNumbers),
	dcg_whitespaces,
	dcg_or_numbers(OrListOfNumbers),
	{
		dcgh_merge_number_constraints_or(ListOfNumbers, OrListOfNumbers, NewListOfNumbers)
	}.

dcg_and_numbers(NewListOfNumbers) -->
	dcg_and,
	dcg_whitespaces,
	dcg_numbers(ListOfNumbers),
	dcg_whitespaces,
	dcg_and_numbers(AndListOfNumbers),
	{
		dcgh_merge_number_constraints_and(ListOfNumbers, AndListOfNumbers, NewListOfNumbers)
	}.
dcg_and_numbers([infinity(1)]) -->
	[].
	
dcg_or_numbers(NewListOfNumbers) -->
	dcg_or,
	dcg_whitespaces,
	dcg_numbers(ListOfNumbers),
	dcg_whitespaces,
	dcg_or_numbers(OrListOfNumbers),
	{
		dcgh_merge_number_constraints_or(ListOfNumbers, OrListOfNumbers, NewListOfNumbers)
	}.
dcg_or_numbers([]) -->
	[].


dcg_numbers(ListOfNumbers) -->
	dcg_left_parenthesis,
	dcg_number_constraint(ListOfNumbers),
	dcg_right_parenthesis.


dcg_numbers([list([I])]) -->
	dcg_integer(I).
dcg_numbers([list(List)]) -->
	dcg_integer(N),
	dcg_whitespaces,
	dcg_minus,
	dcg_whitespaces,
	dcg_integer(M),
	{
		numlist(N,M,List)
	}.
dcg_numbers([list(List)]) -->
	dcg_lt,
	dcg_whitespaces,
	dcg_integer(I),
	{
		Max is I - 1,
		numlist(1, Max, List)
	}.
dcg_numbers([infinity(Min)]) -->
	dcg_gt,
	dcg_whitespaces,
	dcg_integer(I),
	{
		Min is I + 1
	}.
dcg_numbers([infinity(Min)]) -->	
	dcg_integer(I),
	dcg_whitespaces,
	dcg_lt,
	{
		Min is I + 1
	}.
dcg_numbers([list(List)]) -->
	dcg_integer(I),
	dcg_whitespaces,
	dcg_gt,
	{
		Max is I - 1,
		numlist(1, Max, List)
	}.


dcg_float(I) -->
	dcg_integer(I).
dcg_float(R) -->
	{
		var(R), !
	},
	dcg_integer(I),
	dcg_dot,
	dcg_integer(F),
	{
		number_chars(F, NumberChars),
		length(NumberChars, Length),
		R is I + F / (10^Length)
	}.
dcg_float(R) -->
	{
		nonvar(R),
		I is round(float_integer_part(R)),
		F is float_fractional_part(R),
		F \= 0,
		F10 is round(F * 10)
	},
	dcg_integer(I),
	dcg_dot,
	dcg_integer(F10).
	
	
dcg_rational(Z) -->
	{
		var(Z)
	},
	dcg_integer(N1),
	dcg_slash,
	dcg_integer(N2),
	{
		Z is N1 / N2
	}.
dcg_rational(Z) -->
	{
		var(Z)
	},
	dcg_integer(N1),
	dcg_plus,
	dcg_integer(N2),
	dcg_slash,
	dcg_integer(N3),
	{
		Z is N1 + (N2 / N3)
	}.
dcg_rational(Z) -->
	{
		var(Z)
	},
	dcg_integer(N1),
	dcg_minus,
	dcg_integer(N2),
	dcg_slash,
	dcg_integer(N3),
	{
		Z is N1 - (N2 / N3)
	}.
dcg_rational(I) -->
	dcg_integer(I).

dcg_number(R) -->
	dcg_float(R).
dcg_number(Z) -->
	dcg_rational(Z).



dcg_and -->
	"and".
dcg_and -->
	"AND".
dcg_and -->
	",".
	
dcg_or -->
	"or".
dcg_or -->
	"OR".
dcg_or -->
	";".
	
	
dcg_left_parenthesis -->
	"(".
dcg_right_parenthesis -->
	")".

dcg_right_bracket -->
	"]".
dcg_left_bracket -->
	"[".	
	

dcg_underscore -->
	"_".
dcg_underscore -->
	"?".
dcg_underscore -->
	"*".
	
dcg_slash -->
	"/".

dcg_plus -->
	"+".

dcg_minus -->
	"-".
	
dcg_comma -->
	",".
	
dcg_gt -->
	">".
	
dcg_lt -->
	"<".
	

dcg_p -->
	"p".
dcg_p -->
	"P".
dcg_p -->
	"r".
dcg_p -->
	"R".
	

dcg_s -->
	"s".
dcg_s -->
	"S".

dcg_dot -->
	".".
	
dcg_whitespaces -->
	dcg_whitespace,
	dcg_whitespaces.
dcg_whitespaces -->
	[].
	
dcg_whitespace -->
	[W],
	{
		code_type(W, white)
	}.


dcg_integer(I) -->
	{
		var(I)
	},
	dcg_digit(D0),
	dcg_digits(D),
    { 
		number_codes(I, [D0|D])
    }.
dcg_integer(I) -->
    { 
		nonvar(I),
		number_codes(I, [D0|D])
    },
	dcg_digit(D0),
	dcg_digits(D).

dcg_digits([D0|D]) -->
	dcg_digit(D0), !,
	dcg_digits(D).
dcg_digits([]) -->
	[].

dcg_digit(D) -->
	{
		var(D)
	},
	[D],
	{ 
		code_type(D, digit)
	}.
dcg_digit(D) -->	
	{ 
		nonvar(D),
		code_type(D, digit)
	},
	[D].



%%% DCG Helpers %%%


%% Constraints in Normal Form!!!
%% Constraint = [[[a,b,c],[d]],[[g,h],[i,j,k]],[[l,m]]] = (a b c and d) or (g h and i j k) or l m
%% merge_Or( [[[a,b],[c]],[[d]]] , [[[e]],[[f,g],[h]]] ) = [[[a, b], [c]], [[d]], [[e]], [[f, g], [h]]]
%% merge_And( [[[a,b],[c]],[[d]]] , [[[e]],[[f,g],[h]]] ) = [[[a, b], [c], [e]], [[a, b], [c], [f, g], [h]], [[d], [e]], [[d], [f, g], [h]]]
%% merge_Concat( [[[a,b],[c]],[[d]]] , [[[e]],[[f,g],[h]]] ) = [[[a, b, e], [c, e]], [[a, b, f, g], [a, b, h], [c, f, g], [c, h]], [[d, e]], [[d, f, g], [d, h]]]

dcgh_merge_constraints([], C, C, _) :- !.
dcgh_merge_constraints(C, [], C, _) :- !.

dcgh_merge_constraints(ConstraintA, ConstraintB, NewConstraint, or) :- 
	append(ConstraintA, ConstraintB, NewConstraint), !.

dcgh_merge_constraints(ConstraintA, ConstraintB, NewConstraint, and) :-
	findall(
		NewOr,
		(
			member(OrA, ConstraintA),
			member(OrB, ConstraintB),
			append(OrA, OrB, NewOr)
		),
		NewConstraint
	), !.

dcgh_merge_constraints(ConstraintA, ConstraintB, NewConstraint, concat) :-
	findall(
		NewOr,
		(
			member(OrA, ConstraintA),
			member(OrB, ConstraintB),
			findall(
				NewAnd,
				(
					member(AndA, OrA),
					member(AndB, OrB),
					append(AndA, AndB, NewAnd)
				),
				NewOr
			)
		),
		NewConstraint
	), !.


dcgh_constraint_to_multiplex(Constraint, Multiplex) :-
	findall(
		NewOr,
		(
			member(Or, Constraint),
			findall(
				NewAnd,
				(
					member(And, Or),
					flatten(And, FlatAnd),
					NewAnd = [FlatAnd]
				),
				NewOr
			)
		),
		Multiplex
	), !.


%%%% merge Number Constraints %%%%

%%% NumberConstraint = [list([...]), infinity(N)]   "elements from list or greater equal N"

dcgh_merge_number_constraints_or([], C, C) :- !.
dcgh_merge_number_constraints_or(C, [], C) :- !.


dcgh_merge_number_constraints_or(ConstraintA, ConstraintB, NewConstraint) :- 
	dcgh_merge_number_lists_or(ConstraintA, ConstraintB, NewConstraintList),
	dcgh_merge_number_infinity_or(ConstraintA, ConstraintB, NewConstraintInfinity),
	dcgh_merge_number_list_infinity_or(NewConstraintList, NewConstraintInfinity, NewConstraint).
	
dcgh_merge_number_lists_or(ConstraintA, ConstraintB, [list(NewList)]) :-
	memberchk(list(ListA), ConstraintA),
	memberchk(list(ListB), ConstraintB), !,
	union(ListA, ListB, NewList).
dcgh_merge_number_lists_or(ConstraintA, _ConstraintB, [list(ListA)]) :-
	memberchk(list(ListA), ConstraintA), !.
dcgh_merge_number_lists_or(_ConstraintA, ConstraintB, [list(ListB)]) :-
	memberchk(list(ListB), ConstraintB), !.
dcgh_merge_number_lists_or(_ConstraintA, _ConstraintB, []) :- !.

dcgh_merge_number_infinity_or(ConstraintA, ConstraintB, [infinity(NewMin)]) :-
	memberchk(infinity(MinA), ConstraintA),
	memberchk(infinity(MinB), ConstraintB), !,
	NewMin is min(MinA, MinB).
dcgh_merge_number_infinity_or(ConstraintA, _ConstraintB, [infinity(MinA)]) :-
	memberchk(infinity(MinA), ConstraintA), !.
dcgh_merge_number_infinity_or(_ConstraintA, ConstraintB, [infinity(MinB)]) :-
	memberchk(infinity(MinB), ConstraintB), !.
dcgh_merge_number_infinity_or(_ConstraintA, _ConstraintB, []) :- !.

dcgh_merge_number_list_infinity_or([], ConstraintInfinity, ConstraintInfinity) :- !.
dcgh_merge_number_list_infinity_or(ConstraintList, [], ConstraintList) :- !.
dcgh_merge_number_list_infinity_or([list(List)], [infinity(Min)], [list(NewList), infinity(Min)]) :-
	copyList_if_smaller(List, Min, NewList).
	
	
	
dcgh_merge_number_constraints_and([], _C, []) :- !.
dcgh_merge_number_constraints_and(_C, [], []) :- !.
dcgh_merge_number_constraints_and([infinity(1)], C, C) :- !.
dcgh_merge_number_constraints_and(C, [infinity(1)], C) :- !.

dcgh_merge_number_constraints_and(ConstraintA, ConstraintB, NewConstraintClean) :- 
	dcgh_merge_number_intersect_all(ConstraintA, ConstraintB, NewConstraint),
	dcgh_merge_number_clean(NewConstraint, NewConstraintClean).

dcgh_merge_number_intersect_all(ConstraintA, ConstraintB, NewConstraint) :-
	findall(
		Intersection,
		(
			member(PartA, ConstraintA),
			member(PartB, ConstraintB),
			dcgh_merge_number_intersect_parts(PartA, PartB, Intersection)
		),
		NewConstraint
	).
	
dcgh_merge_number_intersect_parts(list(ListA), list(ListB), list(List)) :-
	intersection(ListA, ListB, List).
dcgh_merge_number_intersect_parts(infinity(MinA), infinity(MinB), infinity(Min)) :-
	Min is max(MinA, MinB).
dcgh_merge_number_intersect_parts(infinity(Min), list(ListB), list(List)) :-
	dcgh_merge_number_intersect_parts(list(ListB), infinity(Min), list(List)).
dcgh_merge_number_intersect_parts(list(ListA), infinity(Min), list(List)) :-
	copyList_if_bigger(ListA, Min, List).


dcgh_merge_number_clean(Constraint, [list(NewList), infinity(Min)]) :-
	memberchk(infinity(Min), Constraint),
	memberchk(list(_), Constraint), !,
	dcgh_merge_number_clean_lists(Constraint, NewList).
dcgh_merge_number_clean(Constraint, [list(NewList)]) :-
	memberchk(list(_), Constraint), !,
	dcgh_merge_number_clean_lists(Constraint, NewList).
dcgh_merge_number_clean(Constraint, Constraint) :- !.
	
dcgh_merge_number_clean_lists(Constraint, NewList) :-
	findall(
		List,
		member(list(List), Constraint),
		ListOfLists
	),
	flatten(ListOfLists, ListOfListsFlat),
	list_to_set(ListOfListsFlat, NewList).
	
	


%% http://gollem.science.uva.nl/SWI-Prolog/pldoc/doc_for?object=section%282%2c%20%274.12%27%2c%20%27%2fusr%2flib%2fpl-5.6.51%2fdoc%2fManual%2fDCG.html%27%29

