
%% example:
% 6 1 _ _  6 Pattern
% 2 3 _ _  1 LandingSites           landingSites(Pattern, LandingSites)
% 2 3 4 5  1 (Permutation)          permutation(OneToN, LandingSites)
% 1 1 1 1 -4 BasePattern            landingSites2Pattern(LandingSites, BasePattern)
% 


siteswap(Jugglers, Objects, MaxHeight, Pattern) :-
   length(Pattern, Period),
   landingSites1(Pattern, LandingSites),
   possibleLandingSites1(Pattern, PossibleLandingSites), 
   fillPermutation(PossibleLandingSites, LandingSites),
   landingSites2Pattern(LandingSites, BasePattern),
   fillInAndCopy(Pattern, BasePattern, ObjectsPattern),  % Pattern to calculate the Objects of the Constraints 
   objects(ObjectsPattern, Jugglers, ObjectsFromConstraints),
   MissingObjects is Objects - ObjectsFromConstraints,
   Prechator is Period rdiv Jugglers,
   addObjects(BasePattern, MissingObjects, Jugglers, 0, MaxHeight, Prechator, Pattern),
   checkMultiplexes(Pattern).

addObjects([], 0, _Jugglers, _MinHeight, _MaxHeight, _Prechator, []) :- !.
addObjects([BaseMultiplex|BaseRest], MissingObjects, Jugglers, MinHeight, MaxHeight, Prechator, [Multiplex|PatternRest]) :-
   is_list(Multiplex),!,
   between(0, MissingObjects, ObjectsForMultiplex),
   ObjectsForRest is MissingObjects - ObjectsForMultiplex,
   MinHeightForMultiplex is max(MinHeight, 1),
   addObjects(BaseMultiplex, ObjectsForMultiplex, Jugglers, MinHeightForMultiplex, MaxHeight, Prechator, Multiplex),
   (
      is_set(Multiplex);
      listOf(p(2,0,2), Multiplex)
   ),
   addObjects(BaseRest, ObjectsForRest, Jugglers, MinHeight, MaxHeight, Prechator, PatternRest).
addObjects([_BaseHead|BaseRest], MissingObjects, Jugglers, MinHeight, MaxHeight, Prechator, [Throw|PatternRest]) :-
   nonvar(Throw),!,
   addObjects(BaseRest, MissingObjects, Jugglers, MinHeight, MaxHeight, Prechator, PatternRest).
addObjects([BaseHead|BaseRest], MissingObjects, Jugglers, MinHeight, MaxHeight, Prechator, [p(Throw, Index, Original)|PatternRest]) :-
   var(Throw),
   betweenRandom(0, MissingObjects, ObjectsToAdd),
   Throw is BaseHead + ObjectsToAdd * Prechator,
   Index is ObjectsToAdd mod Jugglers, 
   (
      (Throw = 0, Index = 0);
      Throw >= 1
   ),
   (
      var(MinHeight);
      (number(MinHeight), Throw >= MinHeight)
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
   addObjects(BaseRest, NewMissingObjects, Jugglers, MinHeight, MaxHeight, Prechator, PatternRest).


landingSites2Pattern(LandingSites, BasePattern) :-
   length(LandingSites, Period),
   length(BasePattern, Period),
   possibleLandingSites1(LandingSites, PossibleLandingSites),
   landingSites2Pattern(LandingSites, PossibleLandingSites, _, Period, BasePattern).


landingSites2Pattern([], Rest, Rest, _Period, []) :- !.
landingSites2Pattern([Multiplex|LandingSites], PossibleLandingSites, RemainingLandingSites, Period, [MultiplexBase|BasePattern]) :-
	is_list(Multiplex),!,
	landingSites2Pattern(Multiplex, PossibleLandingSites, RemainingLandingSitesForNow, Period, MultiplexBase),
	landingSites2Pattern(LandingSites, RemainingLandingSitesForNow, RemainingLandingSites, Period, BasePattern).
landingSites2Pattern([Site|LandingSites], [Pos|PossibleLandingSites], RemainingLandingSites, Period, [BaseThrow|BasePattern]) :-
	TmpThrow is Site - Pos,
	substractUntilNonPositiv(TmpThrow, Period, BaseThrow),
	landingSites2Pattern(LandingSites, PossibleLandingSites, RemainingLandingSites, Period, BasePattern).
	

	
prechacThis(Pattern, Pos, UpDown, NumberOfJugglers, NewPattern) :-
	number(Pos), !,
	prechacThis(Pattern, [Pos], UpDown, NumberOfJugglers, NewPattern).
prechacThis(Pattern, Pos, UpDown, NumberOfJugglers, NewPattern) :-
	is_list(Pos),
	length(Pattern, Period),
	nth0ListOfLists(Pos, Pattern, Throw),
	prechacThisThrow(Throw, UpDown, NumberOfJugglers, Period, NewThrow),
	(NewThrow = false ->
		NewPattern = false;
		changeOnePosition(Pattern, Pos, NewThrow, NewPattern)
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
