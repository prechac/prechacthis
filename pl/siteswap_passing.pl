passing_siteswap(Throws, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, ContainShort, DontContainShort, ClubDoesShort, ReactShort) :-
   length(Throws, Length),  %create list with one of the specified period-lengths
   passing_siteswap(Throws, Persons, Objects, Max, NumberOfMultiplexes),
   passesMin(Throws, PassesMin),
   passesMax(Throws, PassesMax),
   convertShortPasses(ContainShort,Length,Persons,Max,Contain), %1.3p -> 1.33333333p
   convertShortPasses(DontContainShort,Length,Persons,Max,DontContain),
   convertShortPasses(ClubDoesShort,Length,Persons,Max,ClubDoes),
   convertShortPasses(ReactShort,Length,Persons,Max,React),
   containsAny(Throws, Contain),
   dontcontainSome(Throws, DontContain),
   clubDoesAny(Throws, ClubDoes),
   reactAny(Throws, React).

passing_siteswap(PassingSiteswap, Persons, Objects, Max, NumberOfMultiplexes) :-
   length(PassingSiteswap, Length),
   length(SoloSiteswap, Length),
   between(1, Length, Passes),
   length(IndexList, Passes),
   isIndexList(Persons, IndexList),
   sumlist(IndexList, IndexSum),
   ObjectsSolo is (Objects + IndexSum)/Persons, %% Objects = ObjectsSolo*Persons - IndexSum
   integer(ObjectsSolo),
   MaxSolo is Max + Length,
   %floor(MaxSoloFloat, MaxSolo),
   siteswap(ObjectsSolo, SoloSiteswap),
   multiplex(SoloSiteswap, SoloSiteswapWithM, NumberOfMultiplexes),
   allHightsSmaller(SoloSiteswapWithM, MaxSolo),
   transform_start(SoloSiteswapWithM, Persons, IndexList, PassingSiteswap),
   allHightsSmaller(PassingSiteswap, Max).

isIndexList(_, []).
isIndexList(Persons, [Head|Tail]) :-
	IndexMax is Persons - 1,
	between(1, IndexMax, Head),
	isIndexList(Persons, Tail).


transform_start(SoloSiteswap, Persons, IndexList, PassingSiteswap) :-
   length(SoloSiteswap, Length),
   Minuend is Length / Persons,
   transform(SoloSiteswap, IndexList, Minuend, PassingSiteswap).
transform([], [], _, []).
transform([Orig_Head| Orig_Rest], IndexList, Minuend, [Trans_Head| Trans_Rest]) :-
   prech(Orig_Head, Trans_Head, Minuend, IndexList, NewIndexList),
   transform(Orig_Rest, NewIndexList, Minuend, Trans_Rest).

prech(Origen, Origen, _, IndexList, IndexList) :- number(Origen).
prech(Origen, p(Transformed, Index, Origen), Minuend, [Index|IndexRest], IndexRest) :- 
   number(Origen),
   Transformed is Origen - Index*Minuend,
   Transformed >= 1.
prech([], [], _, IndexList, IndexList).
prech([MultiplexHead| MultiplexRest], [TransformedHead| TransformedRest], Minuend, IndexList, NewIndexList) :-
	prech(MultiplexHead, TransformedHead, Minuend, IndexList, IndexListAfterHead),
	prech(MultiplexRest, TransformedRest, Minuend, IndexListAfterHead, NewIndexList).
	

allHightsSmaller([], _Max).
allHightsSmaller([Throw| Siteswap], Max) :- 
   hight(Throw, Hight),
   Hight =< Max,
   allHightsSmaller(Siteswap, Max).

passesMin(Throws, PassesMin) :-
   number(PassesMin),
   amountOfPasses(Throws, Passes),
   PassesMin =< Passes.
passesMin(Throws, PassesMin) :- 
   var(PassesMin),
   passesMin(Throws, 1).          %if minimum of passes not specified require _one_ pass.

passesMax(Throws, PassesMax) :-
   number(PassesMax),
   amountOfPasses(Throws, Passes),
   Passes =< PassesMax.
passesMax(_Throws, PassesMax) :- 
   var(PassesMax).                %succeed if maximum of passes not specified

amountOfPasses([], 0).
amountOfPasses([FirstThrow|RestThrows], Passes) :-
   amountOfPasses(RestThrows, RestPasses),
   isPass(FirstThrow, ThisThrowIsPass),
   Passes is ThisThrowIsPass + RestPasses.

isPass(p(_,_,_),     1).
isPass(  Throw,  0) :- number(Throw).
isPass(Multiplex, NumberOfPasses) :- 
	length(Multiplex, Length),
	Length > 1,
	amountOfPasses(Multiplex, NumberOfPasses).

hight(p(X,_,_), X) :- number(X).
hight(  X,  X) :- number(X).
hight( [],  0).
hight([MultiplexHead|MultiplexRest], X) :-
	hight(MultiplexRest, X),
	hight(MultiplexHead, HightHead),
	X >= HightHead.
hight([MultiplexHead|MultiplexRest], X) :-
	hight(MultiplexRest, HightRest),
	hight(MultiplexHead, X),
	X > HightRest.


containsAny(Pattern, [FirstSegment|RestSegments]) :-
   containsAll(Pattern, FirstSegment);
   containsAny(Pattern, RestSegments).

containsAll(_Pattern, []).
containsAll(_Pattern, Seq) :- var(Seq).
containsAll(Pattern, [FirstSegment|RestSegments]) :-
   rotate(Pattern, PatternRotated),
   contains(PatternRotated, FirstSegment),
   containsAll(Pattern, RestSegments).

contains(Pattern, Segment) :-
   append(Segment, _, Pattern).

dontcontainSome(Pattern, [FirstSegment|RestSegments]) :-
   dontcontainAny(Pattern, FirstSegment);
   dontcontainSome(Pattern, RestSegments).

dontcontainAny(_Pattern, []).
dontcontainAny(_Pattern, Seq) :- var(Seq).
dontcontainAny(Pattern, [FirstSegment|RestSegments]) :- 
   not(
   (rotate(Pattern, PatternRotated),
   contains(PatternRotated, FirstSegment))),
   dontcontainAny(Pattern, RestSegments).


clubDoesAny(Pattern, [FirstSegment|RestSegments]) :-
   clubDoesAll(Pattern, FirstSegment);
   clubDoesAny(Pattern, RestSegments).

clubDoesAll(_Pattern, []).
clubDoesAll(Pattern, [FirstSegment | RestSegments]) :-
   rotate(Pattern, PatternRotated),
   clubDoes(PatternRotated, FirstSegment),
   clubDoesAll(Pattern, RestSegments).

clubDoes(Pattern, Seq) :-
   insertThrows(Seq, 0, Pattern).


reactAny(Pattern, [FirstSegment|RestSegments]) :-
   reactAll(Pattern, FirstSegment);
   reactAny(Pattern, RestSegments).


reactAll(_Pattern, []).
reactAll(Pattern, [FirstSegment | RestSegments]) :-
   rotate(Pattern, PatternRotated),
   react(PatternRotated, FirstSegment),
   reactAll(Pattern, RestSegments).


react(Pattern, Seq) :-
	insertThrows(Seq, 0, Pattern, -2).
   


insertThrows(Pattern, Site, Seq) :-
	insertThrows(Pattern, Site, Seq, 0).
insertThrows([],_,_,_).
insertThrows([Throw | Rest], Site, Pattern, Delta) :-
   length(Pattern, Length),
   nth0(Site, Pattern, Throw),
   SitePlusDelta is Site + Delta,
   %Test ob H�he OK!?! (react: 1 2 nicht sinnvoll)
   landingSite(SitePlusDelta, Throw, Length, NextSite),
   insertThrows(Rest, NextSite, Pattern, Delta).


landingSite(Site, Throw, Length, LandingSite) :- %self
   number(Throw),
   LandingSite is (Site + Throw) mod Length.

landingSite(Site, p(_,_,Origen), Length, LandingSite) :- %pass
	landingSite(Site, Origen, Length, LandingSite).

landingSite(Site, Multiplex, Length, LandingSite) :- %multiplex
	is_list(Multiplex),
	member(Throw,Multiplex),
	landingSite(Site, Throw, Length, LandingSite).


float_to_shortpass(Throw,ShortPass) :-
	ThrowTen is Throw * 10,
	ShortPass is floor(ThrowTen)/10.

shortpass_to_float(ShortPass,_,_,_,ShortPass) :- var(ShortPass).
shortpass_to_float(ShortPass,Length,Persons,MaxHight,Throw) :-
	number(ShortPass),
	Minuend is Length / Persons,
	IndexMax is Persons - 1,
	between(1, IndexMax, Index),
	MaxHightSolo is MaxHight + Length,
	between(1, MaxHightSolo, Origen),
    Throw is Origen - Index * Minuend,
	float_to_shortpass(ShortPass, ShortThrow),
	float_to_shortpass(Throw, ShortThrow),!. 

convertShortPasses(ShortPasses,Length,Persons,Max,FloatPasses) :-
	convertShortPassesAny(ShortPasses,Length,Persons,Max,FloatPasses),
	length(FloatPasses, FloatLength),
	FloatLength > 0.

convertShortPassesAny([],_,_,_,[]).
convertShortPassesAny([HeadShort|TailShort],Length,Persons,Max,[Head|Tail]) :-
	convertShortPassesAll(HeadShort,Length,Persons,Max,Head),!,
	convertShortPassesAny(TailShort,Length,Persons,Max,Tail).
convertShortPassesAny([_HeadShort|TailShort],Length,Persons,Max,Tail) :-
	convertShortPassesAny(TailShort,Length,Persons,Max,Tail).

convertShortPassesAll([],_,_,_,[]).
convertShortPassesAll([HeadShort|TailShort],Length,Persons,Max,[Head|Tail]) :-
	convertShortPassesAll(HeadShort,Length,Persons,Max,Head),
	convertShortPassesAll(TailShort,Length,Persons,Max,Tail).
convertShortPassesAll(p(ShortPass,_,_),Length,Persons,Max,p(Throw,_,_)) :-
	shortpass_to_float(ShortPass,Length,Persons,Max,Throw).
convertShortPassesAll(Self,_,_,_,Self) :- number(Self).

pStyle(Length, Origen, classic) :- 
	even(Length),
	odd(Origen).
pStyle(Length, Origen, equi) :-
	even(Length),
	even(Origen).
pStyle(Length, Origen, bi) :-
	odd(Length),
	odd(Origen).
pStyle(Length, Origen, instantbi) :-
	odd(Length),
	even(Origen).
