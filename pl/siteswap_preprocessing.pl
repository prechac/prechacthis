
preprocessConstraint(ConstraintString, Period, NumberOfJugglers, MaxHeight, Constraint) :-
	preprocessConstraint(ConstraintString, positiv, Period, NumberOfJugglers, MaxHeight, Constraint).
	
preprocessConstraint(ConstraintString, ConstraintType, Period, NumberOfJugglers, MaxHeight, Constraint) :-
	string2Constraint(ConstraintString, ConstraintBagShort),
	member(ConstraintShort, ConstraintBagShort),
	convertShortPasses(ConstraintShort, ConstraintType, Period, NumberOfJugglers, MaxHeight, Constraint).


string2Constraint(ConstraintString, Constraint) :-
	dcg_constraint(Constraint, ConstraintString, []), !.



%%% --- short passes ---

float_to_shortpass(Throw,ShortPass) :-
	(number(Throw);rational(Throw)),!,
	ThrowTen is Throw * 10,
	ShortPass is truncate(ThrowTen)/10.
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
	number(ShortThrow),
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
	shortpass_to_pass_dont(p(ShortThrow, _, _), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)),
	(nonvar(Index) -> Index > 0; true). %?????!!!!!!!!!!!!!!!!!!!!!!!
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
dcg_throw_or_constraint(Constraint) -->
	dcg_bracket_open,
	dcg_constraint(Constraint),
	dcg_bracket_close.
	
dcg_throws_or_constraints(NewConstraint) -->
	dcg_whitespace,
	dcg_throw_or_constraint(Constraint),
	dcg_throws_or_constraints(Constraints),
	{
		dcgh_merge_constraints(Constraint, Constraints, NewConstraint, concat)
	}.
dcg_throws_or_constraints([]) -->
	[].
	
dcg_throw(T) -->
	dcg_self(T).
dcg_throw(T) -->
	dcg_pass(T).
	
dcg_self(S) -->
	dcg_integer(S).
dcg_self(_) -->
	dcg_underscore.

dcg_pass(p(F,I)) -->
	dcg_float(F),
	dcg_p,
	dcg_integer(I).
dcg_pass(p(F))  -->
	dcg_float(F),
	dcg_p.
dcg_pass(p(F))  -->
	dcg_float(F),
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

dcg_float(R) -->
	dcg_integer(I),
	dcg_dot,
	dcg_integer(F),
	{
		R is I + F/10
	}.
dcg_float(I) -->
	dcg_integer(I).

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
	
dcg_bracket_open -->
	"(".
dcg_bracket_open -->
	"[".
	
dcg_bracket_close -->
	")".
dcg_bracket_close -->
	"]".

dcg_underscore -->
	"_".
dcg_underscore -->
	"?".
dcg_underscore -->
	"*".

dcg_p -->
	"p".
dcg_p -->
	"P".

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
	dcg_digit(D0),
	dcg_digits(D),
    { 
		number_chars(I, [D0|D])
    }.

dcg_digits([D0|D]) -->
	dcg_digit(D0), !,
	dcg_digits(D).
dcg_digits([]) -->
	[].

dcg_digit(D) -->
	[D],
	{ 
		code_type(D, digit)
	}.



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





%% http://gollem.science.uva.nl/SWI-Prolog/pldoc/doc_for?object=section%282%2c%20%274.12%27%2c%20%27%2fusr%2flib%2fpl-5.6.51%2fdoc%2fManual%2fDCG.html%27%29

