swapThrows(Pattern, PosA, PosB, NewPattern) :-
	posList(Pattern, PosList),
	nth0(PosA, PosList, Pos1),
	nth0(PosB, PosList, Pos2),
	distanceOfThrows(Pos1, Pos2, Diff),
	Diff >= 0, !,
	length(Pattern, Length),
	length(NewPattern, Length),
	nth0ListOfLists(Pos1, Pattern, p(Throw1, Index1, Origen1)),
	nth0ListOfLists(Pos2, Pattern, p(Throw2, Index2, Origen2)),
	NewThrow1 is Throw2 + Diff,
	NewIndex1 is Index2,
	(NewIndex1 = 0 -> 
		(NewThrow1 >= 0);
		(NewThrow1 >= 1)
	),
	NewOrigen1 is Origen2 + Diff,
	NewThrow2 is Throw1 - Diff,
	NewIndex2 is Index1,
	(NewIndex2 = 0 -> 
		(NewThrow2 >= 0);
		(NewThrow2 >= 1)
	),
	NewOrigen2 is Origen1 - Diff,
	changeOnePosition(Pattern, Pos1, p(NewThrow1, NewIndex1, NewOrigen1), TmpPattern),
	changeOnePosition(TmpPattern, Pos2, p(NewThrow2, NewIndex2, NewOrigen2), NewPattern).
swapThrows(Pattern, Pos2, Pos1, NewPattern) :-
	swapThrows(Pattern, Pos1, Pos2, NewPattern).

	
distanceOfThrows(Pos1, Pos2, Diff) :-
	number(Pos1),
	number(Pos2), !,
	Diff is Pos2 - Pos1.
distanceOfThrows([Pos1|_], Pos2, Diff) :-
	!, distanceOfThrows(Pos1, Pos2, Diff).
distanceOfThrows(Pos1, [Pos2|_], Diff) :-
	!, distanceOfThrows(Pos1, Pos2, Diff).


addKey(Pattern, Heights-Pattern) :-
	listOfHeights(Pattern, Heights).

removeKey(_Key-Pattern, Pattern).

lengthK(_Key-Pattern, Length) :-
	length(Pattern, Length), !.
lengthK(Pattern, Length) :-
	is_list(Pattern),!,
	length(Pattern, Length).


averageNumberOfClubs(Pattern, Clubs) :- 
	length(Pattern, Period),
	flatten(Pattern, PatternFlat),
	listOfThrows0(PatternFlat, Throws),
	sumlist(Throws, SumThrows),
	Clubs is SumThrows rdiv Period.
	
listOfThrows([], []) :- !.
listOfThrows([p(Throw,_Index,_Origen)|Pattern], [Throw|Throws]) :-
	listOfThrows(Pattern, Throws).

listOfThrows0([], []) :- !.
listOfThrows0([Var|Pattern], [0|Throws]) :-
	var(Var), !,
	listOfThrows0(Pattern, Throws).
listOfThrows0([p(Throw,_Index,_Origen)|Pattern], [Throw|Throws]) :-
	listOfThrows0(Pattern, Throws).


throw2list(Multiplex, Multiplex) :-
	is_list(Multiplex),!.
throw2list(Throw, [Throw]) :- !.


pattern2Lists([], []) :- !.
pattern2Lists([Throw|Pattern], [List|PatternOfLists]) :-
	throw2list(Throw, List),
	pattern2Lists(Pattern, PatternOfLists).


%%% List of Point in Time

is_lpt(LPT, NumberOfJugglers) :-
	LPT = [LPTA, LPTB],
	length(LPTA, NumberOfJugglers),
	length(LPTB, NumberOfJugglers).
	
fill_lpt(LPT, X, NumberOfJugglers) :-
	is_lpt(LPT, NumberOfJugglers),
	LPT = [LPTA, LPTB],	
	listOf(X, LPTA),
	listOf(X, LPTB).

lpt_nth0(LPT, X, Juggler, a) :-
	LPT = [LPTA, _LPTB],
	nth0(Juggler, LPTA, X).
lpt_nth0(LPT, X, Juggler, b) :-
	LPT = [_LPTA, LPTB],
	nth0(Juggler, LPTB, X).
	
lpt_copy(LPT, LPTCopy) :-
	LPT = [LPTA, LPTB],
	LPTCopy = [LPTAC, LPTBC],
	copyList(LPTA, LPTAC),
	copyList(LPTB, LPTBC).

lpt_add(LPTOld, X, Juggler, Hand, NumberOfJugglers, LPTNew) :- 
	lpt_nth0(LPTOld, XOld, Juggler, Hand),
	XNew is XOld + X,
	is_lpt(LPTNew, NumberOfJugglers),
	lpt_nth0(LPTNew, XNew, Juggler, Hand),
	lpt_copy(LPTOld, LPTNew).

lpt_append(LPTOld, X, Juggler, Hand, NumberOfJugglers, LPTNew) :- 
	lpt_nth0(LPTOld, XOld, Juggler, Hand),
	is_list(XOld),
	append(XOld, [X], XNew),
	is_lpt(LPTNew, NumberOfJugglers),
	lpt_nth0(LPTNew, XNew, Juggler, Hand),
	lpt_copy(LPTOld, LPTNew).

	
%%% --- landing sites ---

possibleLandingSites(Pattern, PossibleLandingSites) :-
	possibleLandingSites(Pattern, PossibleLandingSites, 0).
possibleLandingSites([], [], _Period) :- !.
possibleLandingSites([Multiplex|Pattern], PossibleLandingSites, Position) :-
	is_list(Multiplex), !,
	length(Multiplex, Length),
	listOfNumber(Position, Length, ListOfPosition),
	NextPosition is Position + 1,
	possibleLandingSites(Pattern, RestLandingSites, NextPosition),
	append(ListOfPosition, RestLandingSites, PossibleLandingSites).
possibleLandingSites([_Throw|Pattern], [Position|PossibleLandingSites], Position) :-
	NextPosition is Position + 1,
	possibleLandingSites(Pattern, PossibleLandingSites, NextPosition).
	
	
possibleLandingSites1(Pattern, LandingSites) :-
	possibleLandingSites1(Pattern, LandingSites, 1).
possibleLandingSites1(Pattern, LandingSites1, Position1) :- 
	Position is Position1 - 1,
	possibleLandingSites(Pattern, LandingSites, Position),
	add(LandingSites, 1, LandingSites1).
	
	
	

landingSite(_, Throw, _, _) :-
	var(Throw), !,
	fail.
landingSite(Site, Throw, Length, LandingSite) :- %self
   number(Throw),!,
   LandingSite is (Site + Throw) mod Length.
landingSite(Site, p(_,_,Origen), Length, LandingSite) :- %pass
	landingSite(Site, Origen, Length, LandingSite).
landingSite(Site, Multiplex, Length, LandingSite) :- %multiplex
	is_list(Multiplex), !,
	landingSiteMultiplex(Site, Multiplex, Length, LandingSite).

landingSiteMultiplex(_, [], _, []) :- !.
landingSiteMultiplex(Site, [Head|Multiplex], Period, [_Landing|LandingSites]) :-
	var(Head), !,
	landingSiteMultiplex(Site, Multiplex, Period, LandingSites).
landingSiteMultiplex(Site, [Head|Multiplex], Period, [Landing|LandingSites]) :-
	landingSite(Site, Head, Period, Landing),
	landingSiteMultiplex(Site, Multiplex, Period, LandingSites).

landingSite1(Site1, Throw, Length, LandingSite1) :-
	Site0 is Site1 - 1,
	landingSite(Site0, Throw, Length, LandingSite0),
	LandingSite1 is LandingSite0 + 1.	
	
landingSites(Pattern, LandingSites) :-
	length(Pattern, Period),
	landingSites(Pattern, Period, LandingSites).
	
landingSites([], _, []) :- !.
landingSites([Throw|Pattern], Period, [_Site|LandingSites]) :-
	var(Throw),!,
	landingSites(Pattern, Period, LandingSites).
landingSites([Throw|Pattern], Period, [Site|LandingSites]) :-
	length(Pattern, Length),
	Position is Period - Length - 1,
	landingSite(Position, Throw, Period, Site),
	landingSites(Pattern, Period, LandingSites).
	
	
landingSites1(Pattern, LandingSites) :-
	length(Pattern, Period),
	landingSites1(Pattern, Period, LandingSites).
landingSites1(Pattern, Period, LandingSites1) :- 
	landingSites(Pattern, Period, LandingSites),
	add(LandingSites, 1, LandingSites1).
	

uniqueLandingSites(Pattern) :-
	landingSites1(Pattern, Sites),
	allMembersUnique(Sites).


%%% --- heigts ---
	
height(Var, _) :- var(Var),!.
height( [],  0) :- !.
height( Throw, Throw) :- number(Throw),!.
height( Throw, Height) :- rational_to_number(Throw,Height),!.
height(p(Throw,_,_), Height) :- height(Throw,Height),!.
height(List,Height) :-
	is_list(List),!,
	maxHeight(List,Height).
	

maxHeight(Pattern, MaxHeight) :-
	is_list(Pattern),!,
	addKey(Pattern, KeyPattern),
	maxHeight(KeyPattern, MaxHeight).

maxHeight([]-_, 0) :- !.
maxHeight([HeadHeight|ListOfHeights]-Pattern, MaxHeight) :-
	maxHeight(ListOfHeights-Pattern, MaxHeight),
	MaxHeight >= HeadHeight,!.
maxHeight([MaxHeight|ListOfHeights]-Pattern, MaxHeight) :-
	maxHeight(ListOfHeights-Pattern, RestHeight),
	MaxHeight > RestHeight,!.


allHeightsSmaller(Pattern, Max) :-
	is_list(Pattern),!,
	addKey(Pattern, KeyPattern),
	allHeightsSmaller(KeyPattern, Max).

allHeightsSmaller([]-_Pattern, _Max) :- !.
allHeightsSmaller([Height| RestHeights]-Pattern, Max) :- 
   Height =< Max,!,
   allHeightsSmaller(RestHeights-Pattern, Max).


allHeightsHeigher(Pattern, Min) :-
	is_list(Pattern),!,
	addKey(Pattern, KeyPattern),
	allHeightsHeigher(KeyPattern, Min).
	
allHeightsHeigher([]-_Pattern, _Min) :- !.
allHeightsHeigher([Height| RestHeights]-Pattern, Min) :- 
   Height >= Min,
   allHeightsHeigher(RestHeights-Pattern, Min).


allHeightsHeigherOr0(_Key-Pattern, Min) :-
	allHeightsHeigherOr0(Pattern, Min).

allHeightsHeigherOr0([], _Min) :- !.
allHeightsHeigherOr0([Throw|Pattern], Min) :- 
   Throw = 0,!,
   allHeightsHeigherOr0(Pattern, Min).
allHeightsHeigherOr0([Throw|Pattern], Min) :- 
   height(Throw, Height),
   Height >= Min,!,
   allHeightsHeigherOr0(Pattern, Min).


listOfHeights([],[]) :- !.
listOfHeights([Throw|Siteswap], [Height|List]) :-
	height(Throw, Height),
	listOfHeights(Siteswap, List).


compare_heights(Order,P1,P2) :-
	is_list(P1),
	is_list(P2),
	listOfHeights(P1,H1),
	listOfHeights(P2,H2),
	compare_swaps(Order,H1,H2),!.

compare_heights(Order,K1-_P1,K2-_P2) :-
	compare_swaps(Order,K1,K2).

is_biggest([Pattern], Pattern) :- !.
is_biggest([Pattern|ListOfPatterns], Biggest) :-
	is_biggest(ListOfPatterns, Biggest),
	compare_swaps(Order, Biggest, Pattern),
	Order \= <, !.
is_biggest([Biggest|ListOfPatterns], Biggest) :-
	is_biggest(ListOfPatterns, Pattern),
	compare_swaps(Order, Biggest, Pattern),
	Order = >, !.

is_bigger_than_list([], _Siteswap) :- !.
is_bigger_than_list([Head|Tail], Siteswap) :-
	compare(Order,Siteswap,Head),
	Order \= <,
	is_bigger_than_list(Tail, Siteswap).


is_smallest([Pattern], Pattern) :- !.
is_smallest([Pattern|ListOfPatterns], Smallest) :-
	is_smallest(ListOfPatterns, Smallest),
	compare_swaps(Order, Smallest, Pattern),
	Order \= >, !.
is_smallest([Smallest|ListOfPatterns], Smallest) :-
	is_smallest(ListOfPatterns, Pattern),
	compare_swaps(Order, Smallest, Pattern),
	Order = <, !.

is_smaller_than_list([], _Siteswap).
is_smaller_than_list([Head|Tail], Siteswap) :-
	compare_swaps(Order,Siteswap,Head),
	Order \= >,
	is_smaller_than_list(Tail, Siteswap).

cleanEquals([],[]) :- !.
cleanEquals([Head|Tail], CleanBag) :-
	containsEqual(Tail, Head),!,
	cleanEquals(Tail,CleanBag).
cleanEquals([Head|Tail], [Head|CleanBag]) :-
	cleanEquals(Tail, CleanBag).

containsEqual([Head|_Tail],Siteswap) :-
	areEqual(Siteswap,Head),!.
containsEqual([_Head|Tail],Siteswap) :-
	containsEqual(Tail,Siteswap).

areEqual(P1, P2) :-
	permutateMultiplexes(P1, PTemp),
	rotate(PTemp,PRot),
	isTheSame(PRot, P2).

isTheSame([], []) :- !.
isTheSame([T1|P1], [T2|P2]) :-
   nonvar(T1),
   nonvar(T2),!,
   T1 = T2,
   isTheSame(P1, P2).
isTheSame([T1|P1], [T2|P2]) :-
   var(T1),
   var(T2),!,
   isTheSame(P1, P2).


rotateAll([],[]) :- !.
rotateAll([Head|Tail],[HeadRotated|TailRotated]) :-
	rotateHighestFirst(Head,HeadRotated),
	rotateAll(Tail,TailRotated).

rotateHighestFirst(Siteswap, Rotated) :-
	findall(R,rotate(Siteswap,R), ListOfRotations),
	is_biggest(ListOfRotations, Rotated).

%compareSiteswap(Delta, p(Throw1, Index1, _Origen1), p(Throw2, Index2, _Origen2)) :-
%	compare(Delta, [Throw1, Index1], [Throw2, Index2]), !.
%compareSiteswap(Delta, Siteswap1, Siteswap2) :-
%	amountOfPasses(Siteswap1, Passes1),
%	amountOfPasses(Siteswap2, Passes2),
	

compare_swaps(Order, P1, P2) :-
	siteswapKey(P1, Key1),
	siteswapKey(P2, Key2),
	compare(Order, Key1, Key2).


cleanListOfSiteswaps(List,CleanList) :-
   cleanEquals(List, Swaps),
   rotateAll(Swaps,SwapsRotated),
   sortListOfSiteswaps(SwapsRotated, CleanList).

sortListOfSiteswaps(Swaps, SwapsSorted) :-
   addKeys(Swaps,SwapsWithKeys),
   keysort(SwapsWithKeys,SwapsSortedWithKeys),
   removeKeys(SwapsSortedWithKeys,SwapsSorted).


siteswapKey(Swap, Key) :-
	%amountOfPasses(Swap,Number),
	rat2float(Swap, SwapFloat),
	pattern2Lists(SwapFloat, SwapLists),
	Key = SwapLists.

addKeys([],[]) :- !.
addKeys([Head|Swaps],[Key-Head|SwapsWithKeys]) :-
	siteswapKey(Head, KeyTmp),
	amountOfPasses(Head,Number),
	Key = [Number|KeyTmp],
	addKeys(Swaps,SwapsWithKeys).

removeKeys([],[]) :- !.
removeKeys([_Key-Head|SwapsWithKeys],[Head|Swaps]) :-
	removeKeys(SwapsWithKeys,Swaps).



rat2float([],[]) :- !.	
rat2float([Var|PRat], [Var|PFloat]) :-
	var(Var),!,
	rat2float(PRat, PFloat).
rat2float([p(Var,Index,Origen)|PRat], [p(Var,Index,Origen)|PFloat]) :-
	var(Var),!,
	rat2float(PRat, PFloat).
rat2float([p(Rational,Index,Origen)|PRat], [p(Float,Index,Origen)|PFloat]) :-
	(number(Rational); rational(Rational)),!,
	Float is float(Rational),
	rat2float(PRat, PFloat).
rat2float([ListOfRational|PRat], [ListOfFloat|PFloat]) :-
	is_list(ListOfRational),
	rat2float(ListOfRational, ListOfFloat),
	rat2float(PRat, PFloat).


%succeeds if there is a pattern Merged that unifies P1 and a rotation of P2
merge2(P1, P2, Merged) :-
  rotate(P2, Merged),
  Merged = P1,
  uniqueLandingSites(Merged).


%mergeN(List, Pattern).
%succeeds if all Patterns in List can be unified with Pattern
mergeN([Pattern], Pattern).
mergeN([FirstPattern | Rest], Merged) :-
  mergeN(Rest, RestMerged),
  merge2(RestMerged, FirstPattern, Merged).


% objects([[4,2],2,1]) = 9/3 = 3
objects(Pattern, Objects) :-
   objects(Pattern, 1, Objects).

objects(Pattern, Jugglers, Objects) :-
   sumThrows(Pattern, SumThrows),
   length(Pattern, Period),
   Objects is round(Jugglers * SumThrows rdiv Period).


sumThrows([], 0) :- !.
sumThrows(Throw,Throw) :- number(Throw),!.
sumThrows(Throw, SumThrow) :- rational_to_number(Throw, SumThrow),!.
sumThrows(p(Throw,_,_), SumThrow) :- sumThrows(Throw, SumThrow),!.
sumThrows([Var|RestPattern], Sum) :-
   var(Var),!,
   sumThrows(RestPattern, Sum).
sumThrows([Throw|RestPattern], Sum) :-
   sumThrows(RestPattern, OldSum),
   sumThrows(Throw, SumThrow),
   Sum is OldSum + SumThrow.


sumHeights([], 0) :- !.
sumHeights([Var|RestPattern], Sum) :-
   var(Var),!,
   sumHeights(RestPattern, Sum).
sumHeights([Throw|RestPattern], Sum) :-
   sumHeights(RestPattern, OldSum),
   height(Throw, Height),
   Sum is OldSum + Height.

%%% orbits %%%

orbits(Pattern, Orbits) :-
	length(Pattern, Length),
	length(Orbits, Length),
	setOrbit(Pattern, Orbits, 0, 0, 0, juststarted), !.
	

setOrbit(Pattern, Orbits, FirstPos, OrbitNo, FirstPos, notjuststarted) :-
	(ground(Orbits), !);
	(
		NextOrbitNo is OrbitNo + 1,
		firstNoGround0(Orbits, NextPos),
		setOrbit(Pattern, Orbits, NextPos, NextOrbitNo, NextPos, juststarted)
	).
setOrbit(Pattern, Orbits, Pos, OrbitNo, FirstPos, _) :-
	nth0(Pos, Pattern, Throw),
	not(is_list(Throw)),!,
	nth0(Pos, Orbits, OrbitNoOrig),
	var(OrbitNoOrig),
	OrbitNoOrig = OrbitNo,
	length(Pattern, Length),
	landingSite(Pos, Throw, Length, LandingPos),
	setOrbit(Pattern, Orbits, LandingPos, OrbitNo, FirstPos, notjuststarted).
setOrbit(Pattern, Orbits, Pos, OrbitNo, FirstPos, _) :-
	nth0(Pos, Pattern, Multiplex),
	is_list(Multiplex),  % Multiplex
	nth0(Pos, Orbits, OrbitNoOrig),
	not(ground(OrbitNoOrig)),
	length(Multiplex, MultiplexLength),
	length(OrbitNoOrig, MultiplexLength),
	not((
		member(OrbitNoNonVar, OrbitNoOrig),
		nonvar(OrbitNoNonVar),
		OrbitNoNonVar = OrbitNo
	)),
	nth0(MPos, Multiplex, Throw),
	nth0(MPos, OrbitNoOrig, OrbitNo),
	length(Pattern, Length),
	landingSite(Pos, Throw, Length, LandingPos),
	setOrbit(Pattern, Orbits, LandingPos, OrbitNo, FirstPos, notjuststarted).


	
	


clubsInOrbits(Pattern, OrbitPattern, Clubs) :-
	flatten(OrbitPattern, OrbitsFlat),
	list_to_set(OrbitsFlat, OrbitsSet),
	sort(OrbitsSet, Orbits),
	clubsInOrbit(Pattern, OrbitPattern, Orbits, Clubs).

clubsInOrbit(_, _, [], []) :- !.
clubsInOrbit(Pattern, OrbitPattern, [Orbit|OrbitList], [Clubs| ClubsList]) :-
	clubsInOrbit(Pattern, OrbitPattern, Orbit, Clubs),!,
	clubsInOrbit(Pattern, OrbitPattern, OrbitList, ClubsList).
	
clubsInOrbit(Pattern, OrbitPattern, Orbit, Clubs) :-
	number(Orbit),!,
	justThisOrbit(Pattern, OrbitPattern, Orbit, JustThisOrbit, calc),
	averageNumberOfClubs(JustThisOrbit, Clubs).
	

justThisOrbit([], [], _, [], _) :- !.
justThisOrbit([Multiplex|Pattern], [Orbits|OrbitPattern], Orbit, [NewMultiplex|JustThisOrbit], Type) :-
	is_list(Multiplex), !,
	justThisOrbit(Multiplex, Orbits, Orbit, NewMultiplexTmp, Type),
	(justSpaces(NewMultiplexTmp) ->
		NewMultiplex = '&nbsp;';
		NewMultiplex = NewMultiplexTmp
	),
	justThisOrbit(Pattern, OrbitPattern, Orbit, JustThisOrbit, Type).
justThisOrbit([Throw|Pattern], [Orbit|OrbitPattern], Orbit, [Throw|JustThisOrbit], Type) :-
	!,
	justThisOrbit(Pattern, OrbitPattern, Orbit, JustThisOrbit, Type).
justThisOrbit([_Thorw|Pattern], [_OtherOrbit|OrbitPattern], Orbit, [p(0,0,0)|JustThisOrbit], calc) :-
	justThisOrbit(Pattern, OrbitPattern, Orbit, JustThisOrbit, calc).
justThisOrbit([_Thorw|Pattern], [_OtherOrbit|OrbitPattern], Orbit, ['&nbsp;'|JustThisOrbit], print) :-
	justThisOrbit(Pattern, OrbitPattern, Orbit, JustThisOrbit, print).

justSpaces([]) :- !.
justSpaces([Space|List]) :-
	(
		Space = "&nbsp;";
		Space = " ";
		Space = '&nbsp;';
		Space = ' '
	),
	justSpaces(List).

% !!! Multiplex ToDo
zeroOrbits(Pattern, Orbits) :-
	orbits(Pattern, OrbitPattern),
	positionsInList(Pattern, p(0,0,0), ZerroPositions),
	nth0List(ZerroPositions, OrbitPattern, Orbits).
	

magicOrbits(Pattern, NumberOfJugglers, MagicOrbits) :-
	magicOrbits(Pattern, NumberOfJugglers, _OrbitPattern, MagicOrbits), !.
magicOrbits(Pattern, NumberOfJugglers, OrbitPattern, MagicOrbits) :-
	orbits(Pattern, OrbitPattern),
	clubsInOrbits(Pattern, OrbitPattern, Clubs),
	MagicClubPerPerson is 1 rdiv NumberOfJugglers,
	positionsInList(Clubs, MagicClubPerPerson, MagicOrbits).
	

magicPositions(Pattern, NumberOfJugglers, MagicPositions) :-
	magicOrbits(Pattern, NumberOfJugglers, OrbitPattern, MagicOrbits),
	orbitPositions(MagicOrbits, OrbitPattern, MagicPositions).


orbitPositions(Orbits, OrbitPattern, Positions) :-
	is_list(Orbits), !,
	findall(
		Position,
		(
			member(Orbit, Orbits), 
			orbitPositions(Orbit, OrbitPattern, PositionsTmp),
			member(Position, PositionsTmp)
		),
		Positions
	).
orbitPositions(Orbit, OrbitPattern, Positions) :-
	number(Orbit), !,
	positionsInList(OrbitPattern, Orbit, Positions).
	
killOrbit(Pattern, Orbit, NewPattern) :-
	orbits(Pattern, OrbitPattern),
	orbitPositions(Orbit, OrbitPattern, Positions),
	changePositions(Pattern, Positions, p(0,0,0), NewPattern).

orbitOrder(Pattern, SiteswapPosition, OrbitPositions) :-
	length(Pattern, Length),
	FirstPos is SiteswapPosition mod Length,!,
	orbitOrder(Pattern, Length, FirstPos, OrbitPositions, FirstPos, juststarted).
orbitOrder(_Pattern, Length, SiteswapPosition, [], FirstPos, notjuststarted) :-
	FirstPos is SiteswapPosition mod Length, !.
orbitOrder(Pattern, Length, SiteswapPosition, [Pos|OrbitPositions], FirstPos, _) :-
	Pos is SiteswapPosition mod Length,
	nth0(Pos, Pattern, Throw),
	not(is_list(Throw)),!,
	landingSite(Pos, Throw, Length, LandingPos),
	orbitOrder(Pattern, Length, LandingPos, OrbitPositions, FirstPos, notjuststarted).
orbitOrderx(Pattern, Length, SiteswapPosition, [[Pos,MPos]|OrbitPositions], FirstPos, _) :-
	Pos is SiteswapPosition mod Length,
	nth0(Pos, Pattern, Multiplex),
	is_list(Multiplex),  % Multiplex
	nth0(MPos, Multiplex, Throw),
	landingSite(Pos, Throw, Length, LandingPos),
	orbitOrder(Pattern, Length, LandingPos, OrbitPositions, FirstPos, notjuststarted).
	
throw_was(Pattern, SiteswapPosition, ThrowPosition) :-
	orbitOrder(Pattern, SiteswapPosition, OrbitPositions),
	append(_, [ThrowPosition], OrbitPositions).
	
throw_reacts_to(Pattern, SiteswapPosition, ThrowPosition) :-
	ReactPosition is SiteswapPosition + 2,
	throw_was(Pattern, ReactPosition, ThrowPosition).

