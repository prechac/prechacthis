
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
	listOfThrows(Pattern, Throws),
	sumlist(Throws, SumThrows),
	Clubs is SumThrows rdiv Period.
	
listOfThrows([], []) :- !.
listOfThrows([p(Throw,_Index,_Origen)|Pattern], [Throw|Throws]) :-
	listOfThrows(Pattern, Throws).


%%% --- landing sites ---

landingSite(_, Throw, _, _) :-
	var(Throw), !,
	fail.
landingSite(Site, Throw, Length, LandingSite) :- %self
   number(Throw),!,
   LandingSite is (Site + Throw) mod Length.
landingSite(Site, p(_,_,Origen), Length, LandingSite) :- %pass
	landingSite(Site, Origen, Length, LandingSite).
landingSite(Site, Multiplex, Length, LandingSite) :- %multiplex
	is_list(Multiplex),
	member(Throw,Multiplex),
	landingSite(Site, Throw, Length, LandingSite).
	
landingSite1(Site1, Throw, Length, LandingSite1) :-
	Site0 is Site1 - 1,
	landingSite(Site0, Throw, Length, LandingSite0),
	LandingSite1 is LandingSite0 + 1.	
	
landingSites1(Pattern, LandingSites) :-
	length(Pattern, Period),
	landingSites1(Pattern, Period, LandingSites).
	
landingSites1([], _, []) :- !.
landingSites1([Throw|Pattern], Period, [_Site|LandingSites]) :-
	var(Throw),!,
	landingSites1(Pattern, Period, LandingSites).
landingSites1([Throw|Pattern], Period, [Site|LandingSites]) :-
	length(Pattern, Length),
	Position is Period - Length,
	landingSite1(Position, Throw, Period, Site),
	landingSites1(Pattern, Period, LandingSites).
	

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


compare_swaps(Order, P1, P2) :-
	rat2float(P1, P1F),
	rat2float(P2, P2F),
	compare(Order, P1F, P2F).

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


objects(Pattern, Objects) :-
   objects(Pattern, 1, Objects).

objects(Pattern, Jugglers, Objects) :-
   sumHeights(Pattern, SumHeights),
   length(Pattern, Period),
   Objects is round(Jugglers * SumHeights rdiv Period).

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
	setOrbit(Pattern, Orbits, 0, 0).
	
setOrbit(_, Orbits, _, _) :- ground(Orbits), !.
setOrbit(Pattern, Orbits, Pos, OrbitNo) :-
	nth0(Pos, Orbits, OrbitNoOrig),
	var(OrbitNoOrig),!,
	OrbitNoOrig = OrbitNo,
	nth0(Pos, Pattern, Throw),
	length(Pattern, Length),
	landingSite(Pos, Throw, Length, LandingPos),
	setOrbit(Pattern, Orbits, LandingPos, OrbitNo).
setOrbit(Pattern, Orbits, Pos, OrbitNo) :-
	nth0(Pos, Orbits, OrbitNoOrig),
	nonvar(OrbitNoOrig),!,
	NextOrbitNo is OrbitNo + 1,
	firstVar0(Orbits, NextPos),
	setOrbit(Pattern, Orbits, NextPos, NextOrbitNo).
%%% Multiplex Version !!!

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
justThisOrbit([Throw|Pattern], [Orbit|OrbitPattern], Orbit, [Throw|JustThisOrbit], Type) :-
	!,
	justThisOrbit(Pattern, OrbitPattern, Orbit, JustThisOrbit, Type).
justThisOrbit([_Thorw|Pattern], [_OtherOrbit|OrbitPattern], Orbit, [p(0,0,0)|JustThisOrbit], calc) :-
	justThisOrbit(Pattern, OrbitPattern, Orbit, JustThisOrbit, calc).
justThisOrbit([_Thorw|Pattern], [_OtherOrbit|OrbitPattern], Orbit, ['&nbsp;'|JustThisOrbit], print) :-
	justThisOrbit(Pattern, OrbitPattern, Orbit, JustThisOrbit, print).

	
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
	
