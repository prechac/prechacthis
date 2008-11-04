
%%%  --- list operations ---

changeDimension([], [], []) :- !.
changeDimension([X|XList], [Y|YList], [[X,Y]|XYList]) :-
	changeDimension(XList, YList, XYList).
	

allMembersUnique([]) :- !.
allMembersUnique([Head | Tail]) :-
   var(Head),!,
   allMembersUnique(Tail).
allMembersUnique([Head | Tail]) :-
   nonvar(Head),
   forall(member(X, Tail), (var(X);(nonvar(X),Head\=X))),
   allMembersUnique(Tail).

firstVar0([Var|_List], 0) :-
	var(Var), !.
firstVar0([_NonVar|List], Pos1) :-
	firstVar0(List, Pos),
	Pos1 is Pos + 1.

firstNoGround0([Var|_List], 0) :-
	not(ground(Var)), !.
firstNoGround0([_NonVar|List], Pos1) :-
	firstNoGround0(List, Pos),
	Pos1 is Pos + 1.

justVars([]) :- !.
justVars([Var|Vars]) :-
	var(Var),
	justVars(Vars).


positionsInList(List, Object, Positions) :-
	positionsInList(List, Object, 0, Positions).
positionsInList([], _, _, []) :- !.
positionsInList([Object|Tail], Object, Pos, [Pos|Positions]) :-
	!,
	nextPos(Pos, NextPos),
	positionsInList(Tail, Object, NextPos, Positions).
positionsInList([InnerList|List], Object, Pos, PosList) :-
	is_list(InnerList), !,
	nextPos(Pos, PosInner, inner),
	positionsInList(InnerList, Object, PosInner, InnerPosList),
	nextPos(Pos, NextPos),
	positionsInList(List, Object, NextPos, NextPosList),
	append(InnerPosList, NextPosList, PosList).
positionsInList([_Object|Tail], Object, Pos, Positions) :-	
	nextPos(Pos, NextPos),
	positionsInList(Tail, Object, NextPos, Positions).

nextPos(Pos, NextPos) :-
	number(Pos), !,
	NextPos is Pos + 1.
nextPos(Pos, NextPos) :-
	is_list(Pos),
	length(Pos, Length),
	Length0 is Length - 1,
	nth0(Length0, Pos, Last),
	NextLast is Last + 1,
	changeOnePosition(Pos, Length0, NextLast, NextPos).
nextPos(Pos, NextPos, inner) :- 
	number(Pos), !,
	nextPos([Pos], NextPos, inner).
nextPos(Pos, NextPos, inner) :-
	is_list(Pos), !,
	append(Pos, [0], NextPos).


% !!! Multiplex ToDo
nth0List([], _, []) :- !.
nth0List([Pos|PosList], List, [X|Xs]) :-
	nth0(Pos, List, X),
	nth0List(PosList, List, Xs).

nth0ListOfLists(Pos, List, X) :-
	number(Pos), !,
	nth0(Pos, List, X).
nth0ListOfLists([Pos], List, X) :-
	number(Pos), !,
	nth0(Pos, List, X).
nth0ListOfLists([Pos|Positions], List, X) :-
	nth0(Pos, List, InnerList),
	nth0ListOfLists(Positions, InnerList, X).

changeOnePosition(List, Pos, X, NewList) :-
	number(Pos), !,
	changeOnePosition(List, [Pos], X, NewList).
changeOnePosition(List, [Pos], X, NewList) :-
	number(Pos), !,
	length(List, Length),
	length(NewList, Length),
	PreLength is Pos,
	PostLength is Length - Pos - 1,
	length(PreList, PreLength),
	length(PostList, PostLength),
	append(PreList, _, List),
	append(_, PostList, List),
	append(PreList, _, NewList),
	append(_, PostList, NewList),
	nth0(Pos, NewList, X).
changeOnePosition(List, [Pos|Positions], X, NewList) :-
	length(List, Length),
	length(NewList, Length),
	PreLength is Pos,
	PostLength is Length - Pos - 1,
	length(PreList, PreLength),
	length(PostList, PostLength),
	append(PreList, _, List),
	append(_, PostList, List),
	append(PreList, _, NewList),
	append(_, PostList, NewList),
	nth0(Pos, List, InnerList),
	changeOnePosition(InnerList, Positions, X, NewInnerList),
	nth0(Pos, NewList, NewInnerList).
	


changePositions(List, [], _X, List) :- !.
changePositions(List, [Pos|Positions], X, NewList) :-
	changePositions(List, Positions, X, TmpList),
	changeOnePosition(TmpList, Pos, X, NewList).
	

posList(List, PosList) :- 
	posList(List, [0], PosList), !.
posList([], _Pos, []) :- !.
posList([InnerList|List], Pos, PosList) :-
	is_list(InnerList),!,
	append(Pos, [0], InnerPos),
	posList(InnerList, InnerPos, InnerPosList),
	length(Pos, Length),
	Length0 is Length - 1,
	nth0(Length0, Pos, EndPos),
	NextEndPos is EndPos + 1,
	changeOnePosition(Pos, Length0, NextEndPos, NextPos),	
	posList(List, NextPos, NextPosList),
	append(InnerPosList, NextPosList, PosList).
posList([_X|List], Pos, [Pos|PosList]) :-	
	length(Pos, Length),
	Length0 is Length - 1,
	nth0(Length0, Pos, EndPos),
	NextEndPos is EndPos + 1,
	changeOnePosition(Pos, Length0, NextEndPos, NextPos),	
	posList(List, NextPos, PosList).


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
fillInAndCopy( [Orig_Multiplex | Orig_Rest], [FillIn_Multiplex | FillIn_Rest], [Copy_Multiplex | Copy_Rest]) :-
   is_list(Orig_Multiplex),!,
   length(Orig_Multiplex, Length),
   length(FillIn_Multiplex, Length),
   length(Copy_Multiplex, Length),
   fillInAndCopy(Orig_Multiplex, FillIn_Multiplex, Copy_Multiplex),
   fillInAndCopy(Orig_Rest, FillIn_Rest, Copy_Rest).
fillInAndCopy( [Orig_Head | Orig_Rest], [_FillIn_Head | FillIn_Rest], [Copy_Head | Copy_Rest]) :-
   nonvar(Orig_Head),!,
   Copy_Head = Orig_Head,
   fillInAndCopy(Orig_Rest, FillIn_Rest, Copy_Rest).
fillInAndCopy( [Orig_Head | Orig_Rest], [FillIn_Head | FillIn_Rest], [Copy_Head | Copy_Rest]) :-
   var(Orig_Head),!,
   Copy_Head = FillIn_Head,
   fillInAndCopy(Orig_Rest, FillIn_Rest, Copy_Rest).
fillInAndCopy( [Orig_Head | Orig_Rest], [FillIn_Head | FillIn_Rest], [_Copy_Head | Copy_Rest]) :- %%%???
   var(Orig_Head),
   var(FillIn_Head),!,
   fillInAndCopy(Orig_Rest, FillIn_Rest, Copy_Rest).

copyList([], []) :- !.
copyList([Head|List], [HeadCopy|ListCopy]) :-
	var(HeadCopy), !,
	HeadCopy = Head,
	copyList(List, ListCopy).
copyList([_Head|List], [_HeadCopy|ListCopy]) :-
	copyList(List, ListCopy).
	
	
copyList_if_smaller([], _Sup, []) :- !.
copyList_if_smaller([Head|Tail], Sup, [Head|TailCopy]) :-
	Head < Sup, !,
	copyList_if_smaller(Tail, Sup, TailCopy).
copyList_if_smaller([_Head|Tail], Sup, TailCopy) :-
	copyList_if_smaller(Tail, Sup, TailCopy).
	
	
copyList_if_bigger([], _Min, []) :- !.
copyList_if_bigger([Head|Tail], Min, [Head|TailCopy]) :-
	Head >= Min, !,
	copyList_if_bigger(Tail, Min, TailCopy).
copyList_if_bigger([_Head|Tail], Min, TailCopy) :-
	copyList_if_bigger(Tail, Min, TailCopy).
	
	
numberOfX([], _, 0) :- !.
numberOfX([Var|Tail], X, Number) :-
	var(Var), !,
 	numberOfX(Tail, X, Number).
numberOfX([X|Tail], X, Number) :-
	!,
	numberOfX(Tail, X, OldNumber),
	Number is OldNumber + 1.
numberOfX([_Y|Tail], X, Number) :-
	numberOfX(Tail, X, Number).

numberOfVars([], 0) :- !.
numberOfVars([Head|Tail], Number) :-
	var(Head), !,
	numberOfVars(Tail, NumberTail),
	Number is NumberTail + 1.
numberOfVars([Head|Tail], Number) :-
	is_list(Head), !,
	numberOfVars(Head, NumberHead),
	numberOfVars(Tail, NumberTail),
	Number is NumberTail + NumberHead.
numberOfVars([_Head|Tail], Number) :-
	numberOfVars(Tail, Number).


multiply(Var, _Factor, _Product) :-
	var(Var), !.
multiply(Number, Factor, Product) :-
	(number(Number); rational(Number)),
	Product is Number * Factor, !.
multiply([], _Factor, []) :- !.
multiply([HeadIn | TailIn], Factor, [HeadOut | TailOut]) :-
   multiply(HeadIn, Factor, HeadOut),
   multiply(TailIn, Factor, TailOut).

add(Var, _Summand, _Sum) :-
	var(Var), !.
add(Number, Summand, Sum) :-
	(number(Number); rational(Number)),
	Sum is Number + Summand, !.
add([Head|Tail], Summand, [NewHead|NewTail]) :-
	add(Head, Summand, NewHead),
	add(Tail, Summand, NewTail), !.
add([], _, []) :- !.

memberOrEqual(X, List) :-
	is_list(List),!,
	member(X, List).
memberOrEqual(X, X) :- !.

memberOrEqual(X, List, Pos) :-
	is_list(List),!,
	nth0(Pos, List, X).
memberOrEqual(X, X, -1) :- !.
	
rotate(List, Rotated) :-
    append(Left, Right, List),
	length(Right,LengthRight),
	LengthRight > 0,
    append(Right, Left, Rotated).

rotate_right([Head|Tail], Rotated) :-
	append(Tail, [Head], Rotated),!.

rotate_left(List, Rotated) :-
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

	
realSubtract(List, [], List) :- !.	
realSubtract(List, [Head|Delete], Remaining) :-
	removeOnce(List, Head, RemainingForNow),
	realSubtract(RemainingForNow, Delete, Remaining).



%removeOnce([], _, []) :- !.
removeOnce([Head|List], Head, List) :- !.
removeOnce([Head|Tail], Element, [Head|List]) :- 
	removeOnce(Tail, Element, List), !.
	
listOfNumber(Number, Length, List) :- listOf(Number, Length, List).
listOfNumber(Number, List) :- listOf(Number, List).
	
listOf(X, Length, List) :-
	length(List, Length),
	listOf(X, List).
listOf(_X, []) :- !.
listOf(X, [X|Tail]) :-
	listOf(X, Tail).
	

nth0_gen(Pos, List, X, NewList) :-
	var(List), !,
	length(TmpList, Pos),
	append(TmpList, [X], NewList).
nth0_gen(Pos, List, X, List) :-
	length(List, Length),
	Pos < Length, !,
	nth0(Pos, List, X).
nth0_gen(Pos, List, X, NewList) :-
	length(List, Length),
	Delta is Pos - Length + 1,
	length(AddList, Delta),
	append(List, AddList, NewList),
	nth0(Pos, NewList, X).


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

%%% fillPermutation 
%% ?- L = [4, _, 1, _], fillPermutation([1,2,3,4], L).
%% L = [4, 2, 1, 3]
%% L = [4, 3, 1, 2]
%%
%% ?- L = [[4, _], 2, 1, _], fillPermutation([1,1,2,3,4], L).
%% L = [[4,1], 2, 1, 3]
%% L = [[4,3], 2, 1, 1]
%%
fillPermutation(List, Permutation) :-
	flatten(List, FlatList),
	flatten(Permutation, FlatPermutation),
	removeVars(FlatPermutation, DontPermutate),
	realSubtract(FlatList, DontPermutate, DoPermutate),
	permutation(DoPermutate, PermutatedGaps),
	fillInVars(Permutation, PermutatedGaps).
	

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

fillInVars(List, Gaps) :-
	fillInVars(List, Gaps, _), !.



fillInVars([], Gaps, Gaps) :- !.
fillInVars(_, [], []) :- !.
fillInVars([InnerList|ListWithVars], ListOfGaps, RemainingGaps) :-
	is_list(InnerList), !,
	fillInVars(InnerList, ListOfGaps, RemainingGapsFromInnerList),
	fillInVars(ListWithVars, RemainingGapsFromInnerList, RemainingGaps).
fillInVars([NonVar|ListWithVars], ListOfGaps, RemainingGaps) :-
	nonvar(NonVar), !,
	fillInVars(ListWithVars, ListOfGaps, RemainingGaps).
fillInVars([Var|ListWithVars], [Gap|ListOfGaps], RemainingGaps) :-
	var(Var), !,
	Var = Gap,
	fillInVars(ListWithVars, ListOfGaps, RemainingGaps).

findAtMostNUnique(X, Goal, MaxNumberOfResults, Bag, Flag) :-
	findAtMostNUnique(X, Goal, MaxNumberOfResults, _MaxTime, Bag, Flag), !.
	
findAtMostNUnique(X, Goal, MaxNumberOfResults, MaxTime, Bag, Flag) :- 
	initFindAtMostNUnique,
	post_it_unique(X, Goal, MaxNumberOfResults, MaxTime, Flag),
	gather([], Bag).

initFindAtMostNUnique :-
	retractall(counterNumberOfResults(_)),
	retractall(dataResult(_)),
	retractall(searchStartingTime(_)),
	get_time(Time),
	asserta(searchStartingTime(Time)),
	asserta(counterNumberOfResults(0)),!.
	

findAllUnique(X, Goal, Bag) :- 
	initFindAllUnique,
	post_it_unique(X, Goal),!,
	gather([], Bag). 
	
initFindAllUnique :-
	retractall(dataResult(_)),!.

post_it_unique(X, Goal, MaxNumberOfResults, MaxTime, Flag) :- 
	call(Goal),
	not(dataResult(X)),
	counterNumberOfResults(NumberOfResults),
	retract(counterNumberOfResults(NumberOfResults)),
	(
		(
			NumberOfResults >= MaxNumberOfResults,!,
			Flag = some
		);
		(
			NewNumberOfResults is NumberOfResults + 1,
			asserta(counterNumberOfResults(NewNumberOfResults)),
			asserta(dataResult(X)),
			number(MaxTime),
			searchStartingTime(Start), 
			get_time(Now),
			(MaxTime < Now - Start -> (Flag = time, !); fail)  % force backtrack if not enough results
		)
	).
post_it_unique(_, _, _, _, all).


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


%%% string operations %%%

remove_whitespace([], []) :- !.
remove_whitespace([White|String], CleanString) :-
	char_type(White, white), !,
	remove_whitespace(String, CleanString).
remove_whitespace([NonWhite|String], [NonWhite|CleanString]) :-
	remove_whitespace(String, CleanString).

%%% sort %%% 

keysort(List, Keys, Sorted) :-
	keys(List, Keys, ListWithKeys),
	keysort(ListWithKeys, SortedWithKeys),
	keys(Sorted, _Keys, SortedWithKeys).

keys([], [], []) :- !.
keys([Head|Tail], [Key|Keys], [Key-Head|TailWithKeys]) :-
	keys(Tail, Keys, TailWithKeys).



%%% convert types %%%

a2Number(Number, Number) :- 
	number(Number), !.
a2Number(Atom, Number) :-
	atom(Atom), !,
	atom_number(Atom, Number).
a2Number(String, Number) :-
	string(String), !,
	string_to_atom(String, Atom),
	atom_number(Atom, Number).

a2Atom(Atom, Atom) :-
	atom(Atom), !.
a2Atom(Number, Atom) :-
	number(Number), !,
	atom_number(Atom, Number).
a2Atom(String, Atom) :-
	string(String), !,
	string_to_atom(String, Atom).
a2Atom(List, Atom) :-
	is_list(List), !,
	a2Atom_list(List, ListOfAtoms),
	concat_atom(ListOfAtoms, ',', InnerAtom),
	format(atom(Atom), "[~w]", [InnerAtom]).
a2Atom(X, Atom) :-
	format(atom(Atom), "~w", [X]).
	
a2Atom_list([], []) :- !.
a2Atom_list([A|List], [Atom|ListOfAtoms]) :-
	a2Atom(A, Atom),
	a2Atom_list(List, ListOfAtoms).

a2String(String, String) :-
	string(String), !.
a2String(X, String) :-
	format(string(String), "~w", [X]).
	
	
%% generating integers

integer_gen(Min, Min).
integer_gen(Min, Number) :-
	integer_gen(Min, Befor),
	Number is Befor + 1.