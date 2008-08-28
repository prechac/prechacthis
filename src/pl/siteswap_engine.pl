
%% example:
% 6 1 _ _  6 Pattern
% 2 3 _ _  1 LandingSites           landingSites(Pattern, LandingSites)
% 2 3 4 5  1 (Permutation)          permutation(OneToN, LandingSites)
% 1 1 1 1 -4 BasePattern            landingSites2Pattern(LandingSites, BasePattern)
% 


siteswap(Jugglers, Objects, MaxHeight, Pattern) :-
   length(Pattern, Period),
   landingSites1(Pattern, LandingSites),
   numlist(1,Period, OneToN), % OnetoN = [1,2,3,4,...,Period]
   fillSetPermutation(OneToN, LandingSites),
   landingSites2Pattern(LandingSites, BasePattern),
   fillInAndCopy(Pattern, BasePattern, ObjectsPattern),  % Pattern to calculate the Objects of the Constraints 
   objects(ObjectsPattern, Jugglers, ObjectsFromConstraints),
   MissingObjects is Objects - ObjectsFromConstraints,
   Prechator is Period rdiv Jugglers,
   addObjects(BasePattern, MissingObjects, Jugglers, MaxHeight, Prechator, Pattern).

addObjects([], 0, _Jugglers, _MaxHeight, _Prechator, []) :- !.
addObjects([_BaseHead|BaseRest], MissingObjects, Jugglers, MaxHeight, Prechator, [Throw|PatternRest]) :-
   nonvar(Throw),
   addObjects(BaseRest, MissingObjects, Jugglers, MaxHeight, Prechator, PatternRest).
addObjects([BaseHead|BaseRest], MissingObjects, Jugglers, MaxHeight, Prechator, [p(Throw, Index, Original)|PatternRest]) :-
   var(Throw),
   betweenRandom(0, MissingObjects, ObjectsToAdd),
   Throw is BaseHead + ObjectsToAdd * Prechator,
   Index is ObjectsToAdd mod Jugglers, 
   (
      (Throw = 0, Index = 0);
      Throw >= 1
   ),
   (
      var(MaxHeight);
      (number(MaxHeight), Throw =< MaxHeight)
   ),
   (Index = 0 ->
      Original = Throw;
      Original is Throw + (Jugglers - Index) * Prechator
   ),
   NewMissingObjects is MissingObjects - ObjectsToAdd,
   addObjects(BaseRest, NewMissingObjects, Jugglers, MaxHeight, Prechator, PatternRest).


landingSites2Pattern(LandingSites, BasePattern) :-
   length(LandingSites, Period),
   length(BasePattern, Period),
   oneToN(Period, OneToN),
   landingSites2Pattern(LandingSites, OneToN, Period, BasePattern).


landingSites2Pattern([], [], _Period, []) :- !.
landingSites2Pattern([Site|LandingSites], [Pos|OneToN], Period, [BaseThrow|BasePattern]) :-
	TmpThrow is Site - Pos,
	substractUntilNonPositiv(TmpThrow, Period, BaseThrow),
	landingSites2Pattern(LandingSites, OneToN, Period, BasePattern).
	
	
prechacThis(Pattern, Pos, UpDown, NumberOfJugglers, NewPattern) :-
	length(Pattern, Period),
	PreLength is Pos,
	PostLength is Period - Pos - 1,
	length(PrePattern, PreLength),
	length(PostPattern, PostLength),
	append(PrePattern, _, Pattern),
	append(_, PostPattern, Pattern),
	nth0(Pos, Pattern, Throw),
	prechacThisThrow(Throw, UpDown, NumberOfJugglers, Period, NewThrow),
	(NewThrow = false ->
		NewPattern = false;
		(	
			length(NewPattern, Period),
			append(PrePattern, _, NewPattern),
			append(_, PostPattern, NewPattern),
			nth0(Pos, NewPattern, NewThrow)
		)
	).
	
prechacThisThrow(p(Throw, Index, _Origen), up, NumberOfJugglers, Period, p(NewThrow, NewIndex, NewOrigen)) :-
	Prechator is Period rdiv NumberOfJugglers,
	NewThrow is Throw + Prechator,
	NewIndex is (Index + 1) mod NumberOfJugglers,
	(NewIndex = 0 ->
		NewOrigen = NewThrow;
		NewOrigen is NewThrow + (NumberOfJugglers - NewIndex) * Prechator
	).
prechacThisThrow(p(Throw, Index, _Origen), down, NumberOfJugglers, Period, p(NewThrow, NewIndex, NewOrigen)) :-
	Prechator is Period rdiv NumberOfJugglers,
	NewThrow is Throw - Prechator,
	(Index = 0 ->
		NewIndex is NumberOfJugglers - 1;
		NewIndex is Index - 1
	),
	(NewThrow >= 1; (NewThrow = 0, NewIndex = 0)), !,
	(NewIndex = 0 ->
		NewOrigen = NewThrow;
		NewOrigen is NewThrow + (NumberOfJugglers - NewIndex) * Prechator
	).
prechacThisThrow(_Throw, down, _NumberOfJugglers, _Period, false) :- !.
