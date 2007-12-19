siteswap(Objects, Pattern, MaxHeight) :-
   length(Pattern, Length),
   state(Objects, Length, StatePattern),
   MaxStateHeight is Length + MaxHeight - 1,
   allHeightsSmaller(StatePattern, MaxStateHeight),
   variations(StatePattern, Variations),
   member(Pattern, Variations),
   allHeightsSmaller(Pattern, MaxHeight).


state(Objects, Length, Pattern) :- 
   length(TempPattern, Length),
   distribute(TempPattern, Objects),
   multiply(TempPattern, Length, Pattern).

multiply([], _Factor, []).
multiply([HeadIn | TailIn], Factor, [HeadOut | TailOut]) :-
   HeadOut is HeadIn * Factor,
   multiply(TailIn, Factor, TailOut).



distribute([], 0).
distribute([Head | Tail],Amount) :-
   between(0, Amount, Head),
   RestAmount is Amount - Head,
   distribute(Tail, RestAmount).



variations(Pattern, Variations) :-
  sumlist(Pattern, Max),
  length(Pattern, Length),
  OneShorter is Length-1,
  zeros(OneShorter, Zeros),
  append([Max], Zeros, LimitHi),
  append(Zeros, [Max], LimitLo),
  variations(Pattern, LimitLo, LimitHi, Variations).

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
  length(Pattern, Length),
  LengthMinus1 is Length - 1,
  between(1, LengthMinus1, Offset),
  First is Length - Offset,
  findall(P, varySecond(Pattern, LimitLo, First, P), BagOfLowerPatterns),
  not(length(BagOfLowerPatterns, 0)),!, %if a lower pattern is found, dont look for more
  is_biggest(NextLowerPattern, BagOfLowerPatterns).

nextUpper(Pattern, LimitHi, NextHigherPattern) :-
  length(Pattern, Length),
  LengthMinus1 is Length - 1,
  between(1, LengthMinus1, Offset),
  First is Length - Offset,
  findall(P, varySecond(Pattern, LimitHi, First, P), BagOfHigherPatterns),
  not(length(BagOfHigherPatterns, 0)),!, %if a lower pattern is found, dont look for more
  is_smallest(NextHigherPattern, BagOfHigherPatterns).

varySecond(Pattern, Limit, First, NewPattern) :-
  FirstPlus1 is First + 1,
  length(Pattern, Length),
  between(FirstPlus1, Length, Second),
  swap(Pattern, First, Second, NewPattern),
  withinLimits(Pattern, Limit, NewPattern).

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

   NewFirstThrow  is OldSecondThrow + Delta,
   NewSecondThrow is OldFirstThrow - Delta,

   NewSecondThrow >= 0,
   NewFirstThrow >= 0,

   nth1(FirstIndex,  NewPattern, NewFirstThrow),
   nth1(SecondIndex, NewPattern, NewSecondThrow),

   fillIn(OldPattern, NewPattern).

fillIn([],[]).

fillIn( [_Orig_Head | Orig_Rest], [Copy_Head | Copy_Rest]) :-
   not(var(Copy_Head)),
   fillIn(Orig_Rest, Copy_Rest).

fillIn( [Orig_Head | Orig_Rest], [Copy_Head | Copy_Rest]) :-
   var(Copy_Head),
   Copy_Head = Orig_Head,
   fillIn(Orig_Rest, Copy_Rest).


height( [],  0).
height( Throw, Throw) :- number(Throw),!.
height( Throw, Height) :- rational_to_number(Throw,Height).
height(p(Throw,_,_), Height) :- rational_to_number(Throw,Height).
height([MultiplexHead|MultiplexRest], X) :-
	height(MultiplexRest, X),
	height(MultiplexHead, HeightHead),
	X >= HeightHead.
height([MultiplexHead|MultiplexRest], X) :-
	height(MultiplexRest, HeightRest),
	height(MultiplexHead, X),
	X > HeightRest.



maxHeight([PatternHead|PatternRest], X) :-
	height(PatternRest, X),
	height(PatternHead, HeightHead),
	X >= HeightHead.
maxHeight([PatternHead|PatternRest], X) :-
	height(PatternRest, HeightRest),
	height(PatternHead, X),
	X > HeightRest.


allHeightsSmaller([], _Max).
allHeightsSmaller([Throw| Siteswap], Max) :- 
   height(Throw, Height),
   Height =< Max,
   allHeightsSmaller(Siteswap, Max).


listOfHeights([],[]).
listOfHeights([Throw|Siteswap],[Height|List]) :-
	height(Throw,Height),
	listOfHeights(Siteswap,List).

compare_heights(Order,P1,P2) :-
	listOfHeights(P1,H1),
	listOfHeights(P2,H2),
	compare(Order,H1,H2).

is_biggest(Siteswap,List) :-
	member(Siteswap,List),
	is_bigger_than_list(Siteswap,List).

is_bigger_than_list(_Siteswap,[]).
is_bigger_than_list(Siteswap,[Head|Tail]) :-
	compare_heights(Order,Siteswap,Head),
	Order \= <,
	is_bigger_than_list(Siteswap,Tail).


is_smallest(Siteswap,List) :-
	member(Siteswap,List),
	is_smaller_than_list(Siteswap,List).

is_smaller_than_list(_Siteswap,[]).
is_smaller_than_list(Siteswap,[Head|Tail]) :-
	compare_heights(Order,Siteswap,Head),
	Order \= >,
	is_smaller_than_list(Siteswap,Tail).

rotateHighestFirst(Siteswap,Rotated) :-
	findall(R,rotate(Siteswap,R),ListOfRotations),
	is_biggest(Rotated,ListOfRotations).

