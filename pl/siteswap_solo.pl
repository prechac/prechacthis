siteswap(Objects, Pattern) :-
   length(Pattern, Length),
   state(Objects, Length, StatePattern), 
   vary(StatePattern, [], Variations),
   member(Pattern, Variations).


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


vary(Pattern, Old, New) :-
   length(Pattern, Length),
   First_Upper is Length - 1,
   between(1, First_Upper, First),
   Second_Lower is First + 1,
   between(Second_Lower, Length, Second),
   swap(Pattern, First, Second, NewPattern),
   not(member(NewPattern, Old)),!,
   vary(NewPattern, [NewPattern | Old], New).

vary(_, All, All).

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

