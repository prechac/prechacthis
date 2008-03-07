
%%%  --- list operations ---

allMembersUnique([]) :- !.
allMembersUnique([Head | Tail]) :-
   var(Head),!,
   allMembersUnique(Tail).
allMembersUnique([Head | Tail]) :-
   nonvar(Head),
   forall(member(X, Tail), (var(X);(nonvar(X),Head\=X))),
   allMembersUnique(Tail).


%% fillIn(Original, Copy, StartingPosition, ListOfNotChangingPositions)
fillIn([],[], _, _) :- !.
fillIn( [_Orig_Head | Orig_Rest], [_Copy_Head | Copy_Rest], Position, DontChange) :-
   member(Position, DontChange),!,
   NextPosition is Position + 1,
   fillIn(Orig_Rest, Copy_Rest, NextPosition, DontChange).
fillIn( [Orig_Head | Orig_Rest], [Copy_Head | Copy_Rest], Position, DontChange) :-
   nonvar(Orig_Head),!,
   Copy_Head = Orig_Head,
   NextPosition is Position + 1,
   fillIn(Orig_Rest, Copy_Rest, NextPosition, DontChange).
fillIn( [Orig_Head | Orig_Rest], [_ | Copy_Rest], Position, DontChange) :-
   var(Orig_Head),!,
   NextPosition is Position + 1,
   fillIn(Orig_Rest, Copy_Rest, NextPosition, DontChange).

fillIn([],[]) :- !.
fillIn( [_Orig_Head | Orig_Rest], [Copy_Head | Copy_Rest]) :-
   nonvar(Copy_Head),
   fillIn(Orig_Rest, Copy_Rest).
fillIn( [Orig_Head | Orig_Rest], [Copy_Head | Copy_Rest]) :-
   var(Copy_Head),
   Copy_Head = Orig_Head,
   fillIn(Orig_Rest, Copy_Rest).


fillInAndCopy([],[],[]) :- !.
fillInAndCopy( [Orig_Head | Orig_Rest], [_FillIn_Head | FillIn_Rest], [Copy_Head | Copy_Rest]) :-
   nonvar(Orig_Head),!,
   Copy_Head = Orig_Head,
   fillInAndCopy(Orig_Rest, FillIn_Rest, Copy_Rest).
fillInAndCopy( [Orig_Head | Orig_Rest], [FillIn_Head | FillIn_Rest], [Copy_Head | Copy_Rest]) :-
   var(Orig_Head),!,
   Copy_Head = FillIn_Head,
   fillInAndCopy(Orig_Rest, FillIn_Rest, Copy_Rest).
fillInAndCopy( [Orig_Head | Orig_Rest], [FillIn_Head | FillIn_Rest], [_Copy_Head | Copy_Rest]) :-
   var(Orig_Head),
   var(FillIn_Head),!,
   fillInAndCopy(Orig_Rest, FillIn_Rest, Copy_Rest).


multiply([], _Factor, []).
multiply([HeadIn | TailIn], Factor, [HeadOut | TailOut]) :-
   HeadOut is HeadIn * Factor,
   multiply(TailIn, Factor, TailOut).


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


zeros(0, []).
zeros(Length, Zeros) :-
  Length > 0,
  OneShorter is Length - 1,
  zeros(OneShorter, Oneless),
  append([0], Oneless, Zeros).

oneToN(0, []) :- !.
oneToN(Period, OneToN) :-
	PeriodMinus1 is Period - 1,
	oneToN(PeriodMinus1, OldOneToN),
	append(OldOneToN, [Period], OneToN).


%%%  --- number operations ---

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

rational_to_number(Number, Number) :-	
	number(Number),!.
rational_to_number(Rational, Number) :-
	rational(Rational),
	Number is float(Rational).
		
substract_var(_Minuend, Subtrahend, _Difference) :-
	var(Subtrahend),!.
substract_var(Minuend, _Subtrahend, _Difference) :-
	var(Minuend),!.
substract_var(Minuend, Subtrahend, Difference) :- 
	Difference is Minuend - Subtrahend.
	
substractUntilNonPositiv(Minuend, _Subtrahend, Minuend) :-
	Minuend =< 0, !.
substractUntilNonPositiv(Minuend, Subtrahend, Difference) :-
	TempDifference is Minuend - Subtrahend,
	substractUntilNonPositiv(TempDifference, Subtrahend, Difference).


betweenRandom(Lower, Upper, X) :-
   n2m_shuffled(Lower, Upper, Shuffled),!,
   member(X, Shuffled).

n2m_shuffled(Lower, Upper, Shuffled) :-
   findall(X, between(Lower, Upper, X), N2M),
   permutationRandom(N2M, Shuffled).

permutationRandom([], []).
permutationRandom([Head | Rest], Shuffled) :-
   permutationRandom(Rest, RestShuffled),
   length(RestShuffled, Length),
   Gaps is Length+1,
   InsertAfter is random(Gaps),
   length(FirstPart, InsertAfter),
   append(FirstPart, SecondPart, RestShuffled),
   append(FirstPart, [Head|SecondPart], Shuffled).

fillSetPermutation(Set, Permutation) :-
	is_set(Set),
	removeVars(Permutation, DontPermutate),
	subtract(Set, DontPermutate, DoPermutate),
	permutation(DoPermutate, PermutatedGaps),
	fillInVars(Permutation, PermutatedGaps).
	
removeVars([],[]) :- !.
removeVars([Var|ListWithVars], ListWithoutVars) :-
	var(Var), !,
	removeVars(ListWithVars, ListWithoutVars).
removeVars([NonVar|ListWithVars], [NonVar|ListWithoutVars]) :-
	nonvar(NonVar), !,
	removeVars(ListWithVars, ListWithoutVars).

fillInVars(_, []) :- !.
fillInVars([NonVar|ListWithVars], ListOfGaps) :-
	nonvar(NonVar), !,
	fillInVars(ListWithVars, ListOfGaps).
fillInVars([Var|ListWithVars], [Gap|ListOfGaps]) :-
	var(Var), !,
	Var = Gap,
	fillInVars(ListWithVars, ListOfGaps).

findAtMostNUnique(X, Goal, MaxNumberOfResults, Bag, Flag) :- 
	initFindAtMostNUnique,
	post_it_unique(X, Goal, MaxNumberOfResults, Flag),
	gather([], Bag).

initFindAtMostNUnique :-
	retractall(counterNumberOfResults(_)),
	retractall(dataResult(_)),
	asserta(counterNumberOfResults(0)),!.

findAllUnique(X, Goal, Bag) :- 
	initFindAllUnique,
	post_it_unique(X, Goal),
	gather([], Bag). 
	
initFindAllUnique :-
	retractall(dataResult(_)),!.
		
post_it_unique(X, Goal, MaxNumberOfResults, some) :- 
	call(Goal),
	not(dataResult(X)),
	counterNumberOfResults(NumberOfResults),
	retract(counterNumberOfResults(NumberOfResults)),
	(
		(
			NumberOfResults >= MaxNumberOfResults,!
		);
		(
			NewNumberOfResults is NumberOfResults + 1,
			asserta(counterNumberOfResults(NewNumberOfResults)),
			asserta(dataResult(X)),
			fail  % force backtrack if not enough results
		)
	).
post_it_unique(_, _, _, all).


post_it_unique(X, Goal) :- 
	call(Goal),
	not(dataResult(X)),
	asserta(dataResult(X)),
	fail.  % force backtrack if not enough results
post_it_unique(_, _).


gather(B,Bag) :-  
	dataResult(X),
	retract(dataResult(X)),
	gather([X|B],Bag),
	!.
gather(S,S).
