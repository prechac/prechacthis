%:- ensure_loaded([helpers, siteswap_helpers, siteswap_tree, siteswap_multiplex]).



siteswap(OutputPattern, NumberOfJugglers, Objects, Length, MaxHeight, _NumberOfMultiplexes, PassesMin, PassesMax, ContainShort, DontContainShort, ClubDoesShort, ReactShort) :-
	listOfConstraints(ListOfConstraints, Length, NumberOfJugglers, MaxHeight, ContainShort, DontContainShort, ClubDoesShort, ReactShort),
	member(Pattern, ListOfConstraints),
	%% check constraints!
	siteswap(NumberOfJugglers, Objects, MaxHeight, Pattern),
	(passesMin(Pattern, PassesMin); NumberOfJugglers=1),
	passesMax(Pattern, PassesMax),            
	convertShortPasses(DontContainShort,Length,NumberOfJugglers,MaxHeight,DontContain),
	dontcontainSome(Pattern, DontContain), % call in siteswap predicate allready 
	rotateHighestFirst(Pattern, OutputPattern).

listOfConstraints(SetOfConstraints, Length, Persons, Max, Contain, DontContain, ClubDoes, React) :-
	findall(Throws, mergeConstraints(Throws, Length, Persons, Max, Contain, DontContain, ClubDoes, React), ListOfConstraints),
	cleanEquals(ListOfConstraints, SetOfConstraints).
	
mergeConstraints(Throws, Length, Persons, Max, ContainShort, DontContainShort, ClubDoesShort, ReactShort) :-
	convertShortPasses(ContainShort,Length,Persons,Max,Contain), %  p(1.3,_,_) -> p(4 rdiv 3, _, _)
	convertShortPasses(DontContainShort,Length,Persons,Max,DontContain),
	convertShortPasses(ClubDoesShort,Length,Persons,Max,ClubDoes),
	convertShortPasses(ReactShort,Length,Persons,Max,React),
    length(Throws, Length),
	containsAny(Throws, Contain),
	clubDoesAny(Throws, ClubDoes),
	reactAny(Throws, React),
	dontcontainSome(Throws, DontContain).

passesMin(Throws, PassesMin) :-
   number(PassesMin),
   amountOfPasses(Throws, Passes),
   PassesMin =< Passes.
passesMin(Throws, PassesMin) :- 
   var(PassesMin),
   passesMin(Throws, 0).          %if minimum of passes not specified require _one_ pass.

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

isPass(p(_,Index,_), 1) :- Index > 0.
isPass(p(_,Index,_), 0) :- Index = 0.
isPass(Multiplex, NumberOfPasses) :- 
	is_list(Multiplex),
	amountOfPasses(Multiplex, NumberOfPasses).


containsAny(Pattern, [FirstSegment|RestSegments]) :-
   containsAll(Pattern, FirstSegment);
   containsAny(Pattern, RestSegments).

containsAll(_Pattern, []) :- !.
containsAll(_Pattern, Seq) :- var(Seq).
containsAll(Pattern, [FirstSegment|RestSegments]) :-
   rotate(Pattern, PatternRotated),
   contains(PatternRotated, FirstSegment),
   containsAll(Pattern, RestSegments).

contains(Pattern, Segment) :-
   append(Segment, _, Pattern).

dontcontainSome(Pattern, [FirstSegment|RestSegments]) :-
   dontcontainAny(Pattern, FirstSegment),!;
   dontcontainSome(Pattern, RestSegments),!.

dontcontainAny(_Pattern, []).
dontcontainAny(_Pattern, Seq) :- var(Seq).
dontcontainAny(Pattern, [FirstSegment|RestSegments]) :- 
   forall(rotate(Pattern, Rotation),
      dontcontain(Rotation, FirstSegment)
   ),
   dontcontainAny(Pattern, RestSegments),!.


dontcontain(_, []) :- fail,!.
dontcontain([PatternHead|Pattern], [SegmentHead|Segment]) :-
   ((var(PatternHead); var(SegmentHead));
   dontcontain(Pattern, Segment)),!.
dontcontain([PatternHead|Pattern], [SegmentHead|Segment]) :-
   (PatternHead \= SegmentHead;
   dontcontain(Pattern, Segment)),!.

clubDoesAny(Pattern, [FirstSegment|RestSegments]) :-
   clubDoesAll(Pattern, FirstSegment);
   clubDoesAny(Pattern, RestSegments).

clubDoesAll(_Pattern, []).
clubDoesAll(Pattern, [FirstSegment | RestSegments]) :-
   rotate(Pattern, PatternRotated),
   clubDoes(PatternRotated, FirstSegment),
   clubDoesAll(Pattern, RestSegments).

clubDoes(Pattern, Seq) :-
   insertThrows(Pattern, 0, Seq).


reactAny(Pattern, [FirstSegment|RestSegments]) :-
   reactAll(Pattern, FirstSegment);
   reactAny(Pattern, RestSegments).


reactAll(_Pattern, []).
reactAll(Pattern, [FirstSegment | RestSegments]) :-
   rotate(Pattern, PatternRotated),
   react(PatternRotated, FirstSegment),
   reactAll(Pattern, RestSegments).


react(Pattern, Seq) :-
	insertThrows(Pattern, 0, Seq, -2).


insertThrows(Pattern, Site, Seq) :-
	insertThrows(Pattern, Site, Seq, 0).
insertThrows(_,_,[],_).
insertThrows(Pattern, Site, [Throw | Rest], Delta) :-
   length(Pattern, Length),
   nth0(Site, Pattern, Throw),
   SitePlusDelta is Site + Delta,
   %Test ob Hoehe OK!?! (react: 1 2 nicht sinnvoll)
   landingSite(SitePlusDelta, Throw, Length, NextSite),
   insertThrows(Pattern, NextSite, Rest, Delta).


float_to_shortpass(Throw,ShortPass) :-
	(number(Throw);rational(Throw)),!,
	ThrowTen is Throw * 10,
	ShortPass is truncate(ThrowTen)/10.
float_to_shortpass(p(Throw,Index,Original), p(ShortPass,Index,Original)) :-
	float_to_shortpass(Throw, ShortPass).

float_to_shortpass([],[]).
float_to_shortpass([Throw|Rest],[ShortPass|RestShort]) :-
	float_to_shortpass(Throw,ShortPass),
	float_to_shortpass(Rest, RestShort).

shortpass_to_pass(ShortPass,_,_,_,ShortPass) :- var(ShortPass),!.
shortpass_to_pass(Self, _, _, _, p(Self, 0, Self)) :- integer(Self).
shortpass_to_pass(p(Self, Zero, _), _, _, _, p(Self, Zero, Self)) :- 
	%nonvar(Zero),
	Zero = 0,
	integer(Self).
shortpass_to_pass(p(ShortThrow), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	shortpass_to_pass(p(ShortThrow, _, _), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)),
	Index > 0.
shortpass_to_pass(p(ShortThrow, Index), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	shortpass_to_pass(p(ShortThrow, Index, _), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)).
shortpass_to_pass(p(ShortThrow, Index, Origen), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	number(ShortThrow),
	Prechator is Length rdiv Jugglers,
	IndexMax is Jugglers - 1,
	MaxHeightSolo is MaxHeight + Length,
	float_to_shortpass(ShortThrow, ShortThrowShortend),
	between(1, IndexMax, Index),
	between(1, MaxHeightSolo, Origen),
    Throw is Origen - (Jugglers - Index) * Prechator,
	float_to_shortpass(Throw, ShortThrowShortend). 

convertShortPasses(ShortPasses,Length,Persons,Max,FloatPasses) :-
	convertShortPassesAny(ShortPasses,Length,Persons,Max,FloatPasses),
	length(FloatPasses, FloatLength),
	FloatLength > 0.

convertShortPassesAny([],_,_,_,[]).
convertShortPassesAny([HeadShort|TailShort],Length,Persons,Max,[Head|LongList]) :-
	convertShortPassesAll(HeadShort,Length,Persons,Max,Head),
	convertShortPassesAny(TailShort,Length,Persons,Max,LongList).
convertShortPassesAny([HeadShort|TailShort],Length,Persons,Max,LongList) :-	
	not(convertShortPassesAll(HeadShort,Length,Persons,Max,_Head)),
	convertShortPassesAny(TailShort,Length,Persons,Max,LongList).

convertShortPassesAll([],_,_,_,[]).
convertShortPassesAll([HeadShort|TailShort],Length,Persons,Max,[Head|Tail]) :-
	convertShortPassesAll(HeadShort,Length,Persons,Max,Head),
	convertShortPassesAll(TailShort,Length,Persons,Max,Tail).
convertShortPassesAll(ShortPass,Length,Persons,Max,Pass) :-
	not(is_list(ShortPass)),
	shortpass_to_pass(ShortPass,Length,Persons,Max,Pass).




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
