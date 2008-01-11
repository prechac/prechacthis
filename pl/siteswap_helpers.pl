
addKey(Pattern, Heights-Pattern) :-
	listOfHeights(Pattern, Heights).

removeKey(_Key-Pattern, Pattern).

lengthK(_Key-Pattern, Length) :-
	length(Pattern, Length), !.
lengthK(Pattern, Length) :-
	is_list(Pattern),!,
	length(Pattern, Length).



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
	compare(Order,H1,H2),!.

compare_heights(Order,K1-_P1,K2-_P2) :-
	compare(Order,K1,K2).

is_biggest([Pattern], Pattern) :- !.
is_biggest([Pattern|ListOfPatterns], Biggest) :-
	is_biggest(ListOfPatterns, Biggest),
	compare(Order, Biggest, Pattern),
	Order \= <, !.
is_biggest([Biggest|ListOfPatterns], Biggest) :-
	is_biggest(ListOfPatterns, Pattern),
	compare(Order, Biggest, Pattern),
	Order = >, !.

is_bigger_than_list([], _Siteswap) :- !.
is_bigger_than_list([Head|Tail], Siteswap) :-
	compare(Order,Siteswap,Head),
	Order \= <,
	is_bigger_than_list(Tail, Siteswap).


is_smallest([Pattern], Pattern) :- !.
is_smallest([Pattern|ListOfPatterns], Smallest) :-
	is_smallest(ListOfPatterns, Smallest),
	compare(Order, Smallest, Pattern),
	Order \= >, !.
is_smallest([Smallest|ListOfPatterns], Smallest) :-
	is_smallest(ListOfPatterns, Pattern),
	compare(Order, Smallest, Pattern),
	Order = <, !.

is_smaller_than_list([], _Siteswap).
is_smaller_than_list([Head|Tail], Siteswap) :-
	compare(Order,Siteswap,Head),
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

%succeeds if there is a pattern Merged that unifies P1 and a rotation of P2
merge2(P1, P2, Merged) :-
  rotate(P2, Merged),
  Merged = P1.

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