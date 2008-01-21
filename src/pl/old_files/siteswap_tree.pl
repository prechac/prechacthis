%:- ensure_loaded([helpers, siteswap_helpers, siteswap_multiplex]).

siteswap_tree(Objects, Length, NumberOfJugglers, MaxHeight, ListOfConstraints, Pattern) :-
   (var(ListOfConstraints); ListOfConstraints = []),
   length(Pattern, Length),
   length(ConstraintsPattern, Length),
   siteswap_tree(Objects, Length, NumberOfJugglers, MaxHeight, [ConstraintsPattern], Pattern).
siteswap_tree(Objects, Length, NumberOfJugglers, MaxHeight, ListOfConstraints, Pattern) :-
   length(Pattern, Length),
   addKey(Pattern, Key-Pattern),
   allPossibleStates(ListOfConstraints, ListOfStates),
   member(StatePattern, ListOfStates),
   state(Objects, Length, NumberOfJugglers, StatePattern),
   MaxStateHeight is Length + MaxHeight - 1,
   allHeightsSmaller(StatePattern, MaxStateHeight),
   addKey(StatePattern, StateKey-StatePattern),
   variations(StateKey-StatePattern, Variations),
   member(Key-Pattern, Variations),
   checkConstraints(Pattern, ListOfConstraints),
   allHeightsSmaller(Key-Pattern, MaxHeight).


siteswap(Objects, Length, NumberOfJugglers, MaxHeight, ListOfConstraints, Pattern) :-
   (var(ListOfConstraints); ListOfConstraints = []),
   length(Pattern, Length),
   length(ConstraintsPattern, Length),
   siteswap(Objects, Length, NumberOfJugglers, MaxHeight, [ConstraintsPattern], Pattern).
siteswap(Objects, Length, NumberOfJugglers, MaxHeight, ListOfConstraints, Pattern) :-
   length(Pattern, Length),
   allPossibleStates(ListOfConstraints, ListOfPossibleStates),
   member(StatePattern, ListOfPossibleStates),
   state(Objects, Length, NumberOfJugglers, StatePattern),
%   findall(S, state(Objects, Length, NumberOfJugglers, S), ListOfStates),
%   cleanEquals(ListOfStates, SetOfStates),
%   member(StatePattern, SetOfStates),
   MaxStateHeight is Length + MaxHeight - 1,
   allHeightsSmaller(StatePattern, MaxStateHeight),
   vary(StatePattern, [], Variations),
   member(Pattern, Variations),
   allHeightsSmaller(Pattern, MaxHeight),
   checkConstraints(Pattern, ListOfConstraints).

checkConstraints(Pattern, ListOfConstraints) :-
   member(ConstraintsPattern, ListOfConstraints),
   checkConstraintsPattern(Pattern, ConstraintsPattern),!.

checkConstraintsPattern([],[]).
checkConstraintsPattern([_Throw|Pattern], [Constraint|ConstraintsPattern]) :-
   var(Constraint),!,
   checkConstraintsPattern(Pattern, ConstraintsPattern).
checkConstraintsPattern([Throw|Pattern], [Constraint|ConstraintsPattern]) :-
   nonvar(Constraint),!,
   Throw = Constraint,
   checkConstraintsPattern(Pattern, ConstraintsPattern).
	

state(Objects, Length, NumberOfJugglers, StatePattern) :- 
   length(State, Length),
   distribute(State, Objects),
   stateToStatePattern(State, Length, NumberOfJugglers, StatePattern).

stateToStatePattern([], _Length, _NumberOfJugglers, []) :- !.
stateToStatePattern([StateHead|State], Length, NumberOfJugglers, [Throw|StatePattern]) :-
   stateIndexToThrow(StateHead, Length, NumberOfJugglers, Throw),
   stateToStatePattern(State, Length, NumberOfJugglers, StatePattern).


stateIndexToThrow(Index, _Length, _NumberOfJugglers, Throw) :-
   Index is 0,!,
   Throw is 0.
stateIndexToThrow(Index, Length, NumberOfJugglers, Throw) :-
   0 is Index mod NumberOfJugglers, !, %% Throw is a Self
   Throw is (Index * Length)/NumberOfJugglers.
stateIndexToThrow(Index, Length, NumberOfJugglers, Throw) :-  %% Throw is a pass
   Minuend is Length rdiv NumberOfJugglers,
   Pass is  Index * Minuend,
   Origen is Length * (1 + (Index // NumberOfJugglers)),
   PassIndex is Index mod NumberOfJugglers,
   Throw = p(Pass, PassIndex, Origen).
   

distribute([], 0).
distribute([Head | Tail],Amount) :-
   between(0, Amount, Head),
   RestAmount is Amount - Head,
   distribute(Tail, RestAmount).


vary(Pattern, Old, New) :-
   length(Pattern, Length),
   First_Upper is Length - 1,
   between(1, First_Upper, First),
   Second_Lower is First + 1,
   between(Second_Lower, Length, Second),
   swap(Pattern, First, Second, NewPattern),
   allHeightsHeigherOr0(NewPattern, 1),
   not((rotate(NewPattern, Rotation), member(Rotation, Old))),!,
%   not(member(NewPattern, Old)),!,
   vary(NewPattern, [NewPattern | Old], New).

vary(_, All, All).


variations(Key-Pattern, Variations) :-
  sumlist(Key, Max),
  length(Pattern, Length),
  OneShorter is Length-1,
  zeros(OneShorter, Zeros),
  append([Max], Zeros, LimitHi),
  append(Zeros, [Max], LimitLo),
  variations(Key-Pattern, LimitLo-LimitLo, LimitHi-LimitHi, Variations).

variations(Pattern, LimitLo, LimitHi, Variations) :-
  variationsLower(Pattern, LimitLo, VariationsLower),
  variationsUpper(Pattern, LimitHi, VariationsUpper),
  append(VariationsLower, [Pattern], LowerAndCurrent),
  append(LowerAndCurrent, VariationsUpper, Variations).


variationsLower(Pattern, LimitLo, LowerVariations) :-
  nextLower(Pattern, LimitLo, NextLowerPattern),!,
  variations(NextLowerPattern, LimitLo, Pattern, LowerVariations).

variationsLower(_Pattern, _LimitHi, []).

variationsUpper(Pattern, LimitHi, UpperVariations) :-
  nextUpper(Pattern, LimitHi, NextUpperPattern),!,
  variations(NextUpperPattern, Pattern, LimitHi, UpperVariations).

variationsUpper(_Pattern, _LimitHi, []).


nextLower(Pattern, LimitLo, NextLowerPattern) :-
  lengthK(Pattern, Length),
  LengthMinus1 is Length - 1,
  between(1, LengthMinus1, Offset),
  First is Length - Offset,
  findall(P, varySecond(Pattern, LimitLo, First, P), BagOfLowerPatterns),
  not(length(BagOfLowerPatterns, 0)),!, %if a lower pattern is found, dont look for more
  is_biggest(BagOfLowerPatterns, NextLowerPattern).

nextUpper(Pattern, LimitHi, NextHigherPattern) :-
  lengthK(Pattern, Length),
  LengthMinus1 is Length - 1,
  between(1, LengthMinus1, Offset),
  First is Length - Offset,
  findall(P, varySecond(Pattern, LimitHi, First, P), BagOfHigherPatterns),
  not(length(BagOfHigherPatterns, 0)),!, %if a lower pattern is found, dont look for more
  is_smallest(BagOfHigherPatterns, NextHigherPattern).

varySecond(Key-Pattern, Limit, First, NewKey-NewPattern) :-
  FirstPlus1 is First + 1,
  length(Pattern, Length),
  between(FirstPlus1, Length, Second),
  swap(Pattern, First, Second, NewPattern),
  addKey(NewPattern, NewKey-NewPattern),
  allHeightsHeigherOr0(NewPattern, 1),
  withinLimits(Key-Pattern, Limit, NewKey-NewPattern).

withinLimits(Pattern, Limit, NewPattern) :-
  compare_heights(Order, Limit, Pattern),!,
  compare_heights(Order, Limit, NewPattern),
  compare_heights(Order, NewPattern, Pattern).



swap(OldPattern, FirstIndex, SecondIndex, NewPattern) :-
   length(OldPattern, Length),
   length(NewPattern, Length),

   Delta is SecondIndex - FirstIndex,
   MinusDelta is 0 - Delta,

   nth1(FirstIndex,  OldPattern, OldFirstThrow),
   nth1(SecondIndex, OldPattern, OldSecondThrow),

   moveThrow(OldSecondThrow, Delta, NewFirstThrow),
   moveThrow(OldFirstThrow, MinusDelta, NewSecondThrow),
   
   nth1(FirstIndex,  NewPattern, NewFirstThrow),
   nth1(SecondIndex, NewPattern, NewSecondThrow),

   fillIn(OldPattern, NewPattern, 1, [FirstIndex, SecondIndex]),!.


moveThrow(OldThrow, _Delta, _NewThrow) :-
   var(OldThrow),!.
moveThrow(OldThrow, Delta, NewThrow) :-
   number(OldThrow),!,
   NewThrow  is OldThrow + Delta.
moveThrow(OldThrow, Delta, NewThrow) :-
   rational(OldThrow),!,
   NewThrow  is OldThrow + Delta.
moveThrow(p(OldPass, OldIndex, OldOrigen), Delta, p(NewPass, NewIndex, NewOrigen)) :-
   moveThrow(OldPass, Delta, NewPass),
   moveThrow(OldOrigen, Delta, NewOrigen),
   (var(OldIndex) -> NewIndex = _NewVar; NewIndex = OldIndex).
   

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


stateOfPattern(Pattern, State) :-
	swapToLandingSite(Pattern, 1, State).

swapToLandingSite(Pattern, Position, NextPattern) :-
	length(Pattern, Period),
	Position > Period,!,
	fillIn(Pattern, NextPattern, 1, []).
	
swapToLandingSite(Pattern, Position, NextPattern) :-
	findThrowToThisSite(Pattern, Position, PositionNow),!, %if no throw is found throw is a var. -> go on
	swap(Pattern, Position, PositionNow, NewPattern),
	NextPosition is Position + 1,
	swapToLandingSite(NewPattern, NextPosition, NextPattern).
swapToLandingSite(Pattern, Position, NextPattern) :-	
	NextPosition is Position + 1,
	swapToLandingSite(Pattern, NextPosition, NextPattern).
	

findThrowToThisSite(Pattern, LandingSite, Position) :-
	length(Pattern, Period),
	between(1, Period, Position),
	nth1(Position, Pattern, Throw),
	landingSite1(Position, Throw, Period, LandingSite),!.

	
allPossibleStates([], []) :- !.
allPossibleStates([Pattern|ListOfPatterns], [State|ListOfStates]) :-
	allPossibleStates(ListOfPatterns, ListOfStates),
	stateOfPattern(Pattern, State),
	not(member(Rotation, ListOfStates)),!. %% rotate ????????
allPossibleStates([_Pattern|ListOfPatterns], ListOfStates) :-
	allPossibleStates(ListOfPatterns, ListOfStates).
