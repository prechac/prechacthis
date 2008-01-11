
%% example:
% 6 1 _ _  6 Pattern
% 2 3 _ _  1 LandingSites           landingSites(Pattern, LandingSites)
% 2 3 4 5  1 (Permutation)          permutation(OneToN, LandingSites)
% 1 1 1 1 -4 BasePattern            landingSites2Pattern(LandingSites, BasePattern)
% 


siteswap(Jugglers, Objects, MaxHeight, Pattern) :-
   length(Pattern, Period),
   landingSites1(Pattern, LandingSites),
   oneToN(Period, OneToN), % OnetoN = [1,2,3,4,...,Period]
   permutation(OneToN, LandingSites),
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
   maplist(substract, LandingSites, OneToN, BasePattern).