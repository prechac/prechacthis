preprocessConstraint(ConstraintString, Constraint) :-
	dcg_constraint(Constraint, ConstraintString, []).



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

