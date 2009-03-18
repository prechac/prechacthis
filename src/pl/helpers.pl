:- module(helpers, 
	[
		changeDimension/3,
		allMembersUnique/1,
		firstVar0/2,
		firstNoGround0/2,
		justVars/1,
		positionsInList/3,
		nth0List/3,
		nth0ListOfLists/3,
		changeOnePosition/4,
		changePositions/4,
		posList/2,
		fillIn/2,
		fillIn/4,
		fillInAndCopy/3,
		copyList/2,
		copyList_if_smaller/3,
		copyList_if_bigger/3,
		numberOfX/3,
		numberOfVars/2,
		multiply/3,
		add/3,
		memberOrEqual/2,
		memberOrEqual/3,
		rotate/2,
		rotate_right/2,
		rotate_left/2,
		sort_list_of_expr/2,
		infimum/2,
		min_of_list/2,
		supremum/2,
		max_of_list/2,
		zeros/2,
		oneToN/2,
		realSubtract/3,
		removeOnce/3,
        removeAll/3,
		listOfNumber/2,
		listOfNumber/3,
		listOf/2,
		listOf/3,
		nth0_gen/4,
		compare_expr/3,
		even/1,
		odd/1,
		rational_to_number/2,
		substract_var/3,
		substractUntilNonPositiv/3,
		betweenRandom/3,
		permutationRandom/2,
		fillPermutation/2,
		fillSetPermutation/2,
		removeVars/2,
		fillInVars/2,
		fillInVars/3,
		findall_restricted/4,
		memberchk/3,
		member_restricted/3,
		nth1_restricted/4,
		remove_whitespace/2,
		keysort/3,
		a2Number/2,
		a2Number/3,
		a2Atom/2,
		a2Atom_list/2,
		a2String/2,
		integer_gen/2,
		integer_gen/3,
        lcm/3,
        gcd/3,
        leastCommonMultipleIsProduct/2
	]
).

:- use_module(siteswap_preprocessing).
:- use_module(siteswap_constraints).


:- dynamic
	dataResult/2.


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
	

removeAll([], _Head, []) :- !.
removeAll([Head|List], Head, NewList) :-
    removeAll(List, Head, NewList), !.
removeAll([XHead|List], Head, [XHead|NewList]) :-
    removeAll(List, Head, NewList), !.

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





%%%%%% ------ replacements ------ %%%%%%


%%% --- findall replacements --- %%%

	
/*** findall_unique_restricted
*
* findall_restricted(X, Goal, Bag, Options)
* Options:
* unique
* time(+Seconds) - max time
* results(+Results) - max number of results
* flag(-Flag) - returns one of {all, some, time}
*
*/	

findall_restricted(X, Goal, Bag, Options) :-
	Session is random(10^10),
	init_findall_restricted(Options, Session),
	post_it_timecheck(X, Goal, Options, Session),
	gather([], Bag, Session).
	

init_findall_restricted([], Session) :- 
	retractall(dataResult(Session, _)),
	(recorded(findall_active, _OtherSession) ->
		true;
		forall(recorded(time_limit_exceeded, _, R), erase(R))
	),
	recorda(findall_active, Session), !.
init_findall_restricted([Option|Options], Session) :-
	init_findall_restricted_option(Option, Session), 
	init_findall_restricted(Options, Session).
init_findall_restricted_option(results(_), Session) :-	
	forall(recorded(findall_result_counter, results(Session, _), Ref), erase(Ref)),
	recorda(findall_result_counter, results(Session, 0)), !.
init_findall_restricted_option(_, _) :- !.

	

post_it_timecheck(X, Goal, Options, Session) :-
	memberchk(time(MaxTime), Options),!,
	catch(
		call_with_time_limit(MaxTime, 
			post_it(X, Goal, Options, Session)
		),
		time_limit_exceeded,
		memberchk(time, Options, [name(flag), optional])
	).
	
post_it_timecheck(X, Goal, Options, Session) :-
	recorded(findall_active, _OtherSession), !,
	catch(
		post_it(X, Goal, Options, Session),
		time_limit_exceeded,
		(
			memberchk(time, Options, [name(flag), optional]),
			recorda(time_limit_exceeded, Session)
		)
	).
post_it_timecheck(X, Goal, Options, Session) :-	
	post_it(X, Goal, Options, Session).


post_it(X, Goal, Options, Session) :- 
	call(Goal),
	not(fail_posting(X, Options, Session)),
	asserta(dataResult(Session, X)),
	stop_posting(Options, Session), !.
post_it(_, _, Options, _Session) :-
	memberchk(all, Options, [name(flag), optional]).


fail_posting(X, Options, Session) :-
	member(Option, Options),
	fail_posting_option(X, Option, Session).
	
fail_posting_option(X, unique, Session) :-
	dataResult(Session, X).
	
stop_posting(Options, Session) :-
	member(Option, Options),
	stop_posting_option(Option, Options, Session), !.
stop_posting(Options, _Session) :-
	recorded(time_limit_exceeded, _S), 
	memberchk(time, Options, [name(flag), optional]), !.
	
stop_posting_option(results(MaxResults), Options, Session) :-
	number(MaxResults),
	recorded(findall_result_counter, results(Session, NumberOfResults), Ref),
	erase(Ref),
	NewNumberOfResults is NumberOfResults + 1,
	recorda(findall_result_counter, results(Session, NewNumberOfResults)),
	NumberOfResults >= MaxResults - 1,
	memberchk(some, Options, [name(flag), optional]).
	
	
gather(B, Bag, Session) :-
	dataResult(Session, X),
	retract(dataResult(Session, X)),
	gather([X|B],Bag, Session),
	!.
gather(S,S, Session) :-
	recorded(findall_active, Session, Rev),
	erase(Rev).




%%% --- member replacements --- %%%

/*** memberchk
* memberchk(X, List, Options)
* Options: 
* name(Name)
* default(Default)
* optional
*/
memberchk(X, List, Options) :-
	select(name(Name), Options, RestOptions),
	Y =.. [Name, X],
	memberchk(Y, List, RestOptions), !.
memberchk(X, List, Options) :-
	not(memberchk(name(_), Options)),
	memberchk(X, List), !.
memberchk(X, _List, Options) :-
	memberchk(default(X), Options), !.
memberchk(_X, _List, Options) :-
	memberchk(optional, Options), !.
	
	

member_restricted(_X, _List, Options) :-
	memberchk(stop_if(Goal), Options),
	call(Goal), !,
	fail.
member_restricted(X, [X|_List], _Options).
member_restricted(X, [_Y|List], Options) :-
	member_restricted(X, List, Options).
	

nth1_restricted(_Pos, _List, _X, Options) :-
	memberchk(stop_if(Goal), Options),
	call(Goal), !,
	fail.
nth1_restricted(1, [X|_List], X, _Options).
nth1_restricted(Pos, [_Y|List], X, Options) :-
	nonvar(Pos),
	Pos1 is Pos - 1,
	nth1_restricted(Pos1, List, X, Options).
nth1_restricted(Pos, [_Y|List], X, Options) :-
	var(Pos),
	nth1_restricted(Pos1, List, X, Options),
	Pos is Pos1 + 1.
	
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
a2Number(A, Number) :-
	a2Number(A, Number, []).
	
a2Number(Number, Number, _Options) :- 
	(number(Number); rational(Number)), !.
a2Number(Atom, Number, Options) :-
	atom(Atom), !,
	name(Atom, List),
	a2Number(List, Number, Options).
a2Number(String, Number, _Options) :-
	is_list(String),
	phrase(dcg_number_neg(Number), String), !.
a2Number(_A, Default, Options) :-
	memberchk(default(Default), Options),
	(number(Default); rational(Default)), !.

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

integer_gen(Min, Number) :-
	integer_gen(Min, Number, []).

integer_gen(_Min, _Number, Options) :-
	memberchk(stop_if(Goal), Options),
	call(Goal), !,
	fail.
integer_gen(Min, Min, _Options).
integer_gen(Min, Number, Options) :-
	integer_gen(Min, Befor, Options),
	Number is Befor + 1.
	


/* gcd(X,Y,Z) is true if Z is the greatest common divisor of X and Y.      */
gcd(X, X, X) :- 
    X > 0.
gcd(X, Y, G) :- 
    X > Y, 
    plus(Y,X1,X),
    gcd(X1,Y,G).
gcd(X, Y, G):- 
    Y > X,
    plus(X,Y1,Y),
    gcd(X,Y1,G).


lcm(X,Y,LCM) :-
    gcd(X,Y,GCD),
    LCM is abs(X*Y)/GCD.


leastCommonMultipleIsProduct(X, Y) :-
    P is X * Y,
    lcm(X,Y,P).
