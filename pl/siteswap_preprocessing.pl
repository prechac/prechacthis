preprocessConstraint(_ConstraintString, _Constraint) :- !.

	
	
tokenizeConstraint(_, []) :- !.
tokenizeConstraint(ConstraintString, [Token|ConstraintTokens]) :-
	phrase(isThrow(Token), ConstraintString, Rest),
	tokenizeConstraint(Rest, ConstraintTokens).
	
%%% Siteswap Constraints Grammar rules %%%

% Bedingung -> Pattern [and Pattern]*
% Bedingung -> Pattern [or  Pattern]*
% Pattern   -> [Wurf | (Bedingung) ]+
% Wurf -> ...

% (4 4 and 1 (1p or 2p)) or 4 4 1
% ==> [[[4,4],[1,p(1)]],[[4,4],[1,p(2)]],[[4 4 1]]]

% or(A,B)

g_constraint -->
	g_pattern,
	(g_and_patterns; g_or_patterns).

g_pattern -->
	g_whitespaces,
	g_throw_or_constraint,
	g_whitespaces,
	g_throws_or_constraints,
	g_whitespaces.
	
g_and_patterns -->
	g_and,
	g_pattern,
	g_and_patterns.
g_and_patterns -->
	[].
	
g_or_patterns -->
	g_or,
	g_pattern,
	g_or_patterns.
g_or_patterns -->
	[].

g_throw_or_constraint -->
	g_throw.
g_throw_or_constraint -->
	g_bracket_open,
	g_constraint,
	g_bracket_close.
	
g_throws_or_constraints -->
	g_whitespace,
	g_throw_or_constraint,
	g_throws_or_constraints.
g_throws_or_constraints -->
	[].
	
g_throw -->
	g_self.
g_throw -->
	g_pass.
	
g_self -->
	g_integer.
g_self -->
	g_underscore.

g_pass -->
	g_rational,
	g_p,
	g_integer.
g_pass  -->
	g_rational,
	g_p.
g_pass -->
	g_underscore.

g_rational -->
	g_integer,
	g_dot,
	g_integer.
g_rational -->
	g_integer.

g_and -->
	"and".
g_and -->
	"AND".
g_and -->
	",".
	
g_or -->
	"or".
g_or -->
	"OR".
g_or -->
	";".
	
g_bracket_open -->
	"(".
g_bracket_open -->
	"[".
	
g_bracket_close -->
	")".
g_bracket_close -->
	"]".

g_underscore -->
	"_".

g_p -->
	"p".
g_p -->
	"P".

g_dot -->
	".".
	
g_whitespaces -->
	g_whitespace,
	g_whitespaces.
g_whitespaces -->
	[].
	
g_whitespace -->
	[W],
	{
		code_type(W, white)
	}.

g_integer -->
	g_digit,
	g_digits.

g_digits -->
	g_digit, !,
	g_digits.
g_digits -->
	[].

g_digit -->
	[D],
	{ 
		code_type(D, digit)
	}.











ag_self(S) -->
		g_integer(S).

ag_rational(R) -->
		g_integer(I),
		g_dot(D),
		g_integer(F),
		{
			append(I,[D|F],R)
		}.

ag_integer(I) -->
        g_digit(D0),
        g_digits(D),
        { 
			number_chars(I, [D0|D])
        }.

ag_digits([D|T]) -->
        g_digit(D), !,
        g_digits(T).
ag_digits([]) -->
        [].

ag_digit(D) -->
        [D],
        { 
			code_type(D, digit)
        }.






%% http://gollem.science.uva.nl/SWI-Prolog/pldoc/doc_for?object=section%282%2c%20%274.12%27%2c%20%27%2fusr%2flib%2fpl-5.6.51%2fdoc%2fManual%2fDCG.html%27%29

