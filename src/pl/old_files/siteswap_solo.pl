:- ensure_loaded([helpers, siteswap_helpers]).

siteswap(Objects, Pattern, MaxHeight) :-
	siteswap(Objects, Pattern, MaxHeight, _). 

siteswap(Objects, Pattern, MaxHeight, ConstraintsPattern) :-
   length(Pattern, Length),
   length(ConstraintsPattern, Length),
%   addKey(Pattern, Key-Pattern),
   stateOfPattern(ConstraintsPattern, StatePattern),
   state(Objects, Length, StatePattern),
   MaxStateHeight is Length + MaxHeight - 1,
   allHeightsSmaller(StatePattern, MaxStateHeight),
%   addKey(StatePattern, StateKey-StatePattern),
%   variations(StateKey-StatePattern, Variations),
%   member(Key-Pattern, Variations),
   vary(StatePattern, [], Variations),
   member(Pattern, Variations),
   checkConstrains(Pattern, ConstraintsPattern),
%   allHeightsSmaller(Key-Pattern, MaxHeight).
   allHeightsSmaller(Pattern, MaxHeight).


checkConstrains([],[]).
checkConstrains([_Throw|Pattern], [Constraint|ConstraintsPattern]) :-
   var(Constraint),!,
   checkConstrains(Pattern, ConstraintsPattern).
checkConstrains([Throw|Pattern], [Constraint|ConstraintsPattern]) :-
   nonvar(Constraint),!,
   Throw = Constraint,
   checkConstrains(Pattern, ConstraintsPattern).
	

state(Objects, Length, Pattern) :- 
   length(TempPattern, Length),
   distribute(TempPattern, Objects),
   multiply(TempPattern, Length, Pattern).


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
   allHeightsHeigher(NewPattern, 0),
   not(member(NewPattern, Old)),!,
   vary(NewPattern, [NewPattern | Old], New).

vary(_, All, All).


variations(Key-Pattern, Variations) :-
  sumlist(Pattern, Max),
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
  allHeightsHeigher(NewKey-NewPattern, 0),
  withinLimits(Key-Pattern, Limit, NewKey-NewPattern).

withinLimits(Pattern, Limit, NewPattern) :-
  compare_heights(Order, Limit, Pattern),!,
  compare_heights(Order, Limit, NewPattern),
  compare_heights(Order, NewPattern, Pattern).



swap(OldPattern, FirstIndex, SecondIndex, NewPattern) :-
   length(OldPattern, Length),
   length(NewPattern, Length),

   Delta is SecondIndex - FirstIndex,

   nth1(FirstIndex,  OldPattern, OldFirstThrow),
   nth1(SecondIndex, OldPattern, OldSecondThrow),

   (var(OldSecondThrow) ->
      NewFirstThrow = _OldSecondThrow;
      NewFirstThrow  is OldSecondThrow + Delta
   ),
   (var(OldFirstThrow) ->
      NewSecondThrow = _OldFirstThrow;
      NewSecondThrow is OldFirstThrow - Delta
   ),

   nth1(FirstIndex,  NewPattern, NewFirstThrow),
   nth1(SecondIndex, NewPattern, NewSecondThrow),

   fillIn(OldPattern, NewPattern, 1, [FirstIndex, SecondIndex]),!.


fillIn([],[], _, _).

fillIn( [_Orig_Head | Orig_Rest], [_Copy_Head | Copy_Rest], Position, DontChange) :-
   member(Position, DontChange),!,
   NextPosition is Position + 1,
   fillIn(Orig_Rest, Copy_Rest, NextPosition, DontChange).

fillIn( [Orig_Head | Orig_Rest], [Copy_Head | Copy_Rest], Position, DontChange) :-
   nonvar(Orig_Head),
   Copy_Head = Orig_Head,
   NextPosition is Position + 1,
   fillIn(Orig_Rest, Copy_Rest, NextPosition, DontChange).

fillIn( [Orig_Head | Orig_Rest], [_ | Copy_Rest], Position, DontChange) :-
   var(Orig_Head),
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
	