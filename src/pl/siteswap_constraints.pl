

siteswap(OutputPattern, NumberOfJugglers, Objects, Length, MaxHeight, _NumberOfMultiplexes, PassesMin, PassesMax, ContainShortBag, DontContainShortBag, ClubDoesShortBag, ReactShortBag) :-
	listOfConstraints(ListOfConstraints, Length, NumberOfJugglers, MaxHeight, ContainShortBag, ClubDoesShortBag, ReactShortBag),
	(ListOfConstraints = [] -> 
		length(Pattern, Length);
		member(Pattern, ListOfConstraints)
	),
	siteswap(NumberOfJugglers, Objects, MaxHeight, Pattern),
	(passesMin(Pattern, PassesMin); NumberOfJugglers=1),
	passesMax(Pattern, PassesMax),            
	member(DontContainShort, DontContainShortBag),
	convertShortPassesDont(DontContainShort,Length,NumberOfJugglers,MaxHeight,DontContain),  %% !!! doesn't convert Index and Original !!!!
	forall(member(DontContainPattern, DontContain), dontContainRotation(Pattern, DontContainPattern)),
	rotateHighestFirst(Pattern, OutputPattern).

listOfConstraints(SetOfConstraints, Length, Persons, Max, Contain, ClubDoes, React) :-
	findall(Throws, mergeConstraints(Throws, Length, Persons, Max, Contain, ClubDoes, React), ListOfConstraints),
	(ListOfConstraints = [] ->
		(
			Contain = [[]], 
			ClubDoes = [[]],
			React = [[]]
		);true
	),
	cleanEqualConstraints(ListOfConstraints, SetOfConstraints).
	
mergeConstraints(ConstraintRotated, Length, Persons, Max, ContainShortBag, ClubDoesShortBag, ReactShortBag) :-
	member(ContainShort, ContainShortBag),
	member(ClubDoesShort, ClubDoesShortBag),
	member(ReactShort, ReactShortBag),
	convertShortPasses(ContainShort,Length,Persons,Max,Contain), %  p(1.3,_,_) -> p(4 rdiv 3, I, O)
	convertShortPasses(ClubDoesShort,Length,Persons,Max,ClubDoes),
	convertShortPasses(ReactShort,Length,Persons,Max,React),
	findall(Pattern, (length(Pattern, Length), member(ContainThrows,  Contain ), contains(Pattern, ContainThrows )), BagContains),
	findall(Pattern, (length(Pattern, Length), member(ClubDoesThrows, ClubDoes), clubDoes(Pattern, ClubDoesThrows)), BagClubDoes),
	findall(Pattern, (length(Pattern, Length), member(ReactThrows,    React   ), react(   Pattern, ReactThrows   )), BagReact   ),
	append(BagContains, BagClubDoes, BagTmp),
	append(BagTmp, BagReact, BagOfCostraints),
	mergeN(BagOfCostraints, Constraint),
	rotateHighestFirst(Constraint, ConstraintRotated).


cleanEqualConstraints(BagOfConstraints, CleanBagOfConstraints) :-
	cleanEqualConstraintsForward(BagOfConstraints, HalfCleanedBag),
	reverse(HalfCleanedBag, HalfCleanedBagInverted),
	cleanEqualConstraintsForward(HalfCleanedBagInverted, CleanBagOfConstraints).

cleanEqualConstraintsForward([], []) :- !.
cleanEqualConstraintsForward([SubConstraint|BagOfConstraints], CleanBagOfConstraints) :-
	member(Constraint, BagOfConstraints),
	isRotatedSubConstraint(SubConstraint, Constraint),!,
	cleanEqualConstraintsForward(BagOfConstraints, CleanBagOfConstraints).
cleanEqualConstraintsForward([Constraint|BagOfConstrains], [Constraint|CleanBagOfConstrains]) :-
	cleanEqualConstraintsForward(BagOfConstrains, CleanBagOfConstrains).
	
isRotatedSubConstraint(SubConstraint, Constraint) :-
	rotate(Constraint, ConstraintRotated),
	isSubConstaint(SubConstraint, ConstraintRotated).

isSubConstaint([], []) :- !.
isSubConstaint([SubThrow|SubConstraint], [Throw|Constraint]) :-
	nonvar(SubThrow), nonvar(Throw), !,
	SubThrow = Throw,
	isSubConstaint(SubConstraint, Constraint).
isSubConstaint([_SubThrow|SubConstraint], [Throw|Constraint]) :-
	var(Throw), !,
	isSubConstaint(SubConstraint, Constraint).


%% --- Constraints Passes ---

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


%%% --- Constraints Pattern ---

contains(Pattern, Segment) :-
   append(Segment, _, Pattern).

dontcontain(_, []) :- fail,!.
dontcontain([PatternHead|Pattern], [SegmentHead|Segment]) :-
   ((var(PatternHead); var(SegmentHead));
   dontcontain(Pattern, Segment)),!.
dontcontain([PatternHead|Pattern], [SegmentHead|Segment]) :-
   (PatternHead \= SegmentHead;
   dontcontain(Pattern, Segment)),!.

dontContainRotation(Pattern, Segment) :-
	forall(rotate(Pattern, Rotation), dontcontain(Rotation, Segment)).

clubDoes(Pattern, Seq) :-
   insertThrows(Pattern, 0, Seq).

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



%%% --- short passes ---

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


shortpass_to_pass_dont(ShortPass,_,_,_,ShortPass) :- var(ShortPass), !.
shortpass_to_pass_dont(Self, _, _, _, p(Self, 0, Self)) :- integer(Self), !.
shortpass_to_pass_dont(p(Self, Zero, Self), _, _, _, p(Self, Zero, Self)) :- 
	nonvar(Zero),
	Zero = 0,
	integer(Self),!.
shortpass_to_pass_dont(p(ShortThrow), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	shortpass_to_pass_dont(p(ShortThrow, _, _), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)),
	(nonvar(Index) -> Index > 0; true). %?????!!!!!!!!!!!!!!!!!!!!!!!
shortpass_to_pass_dont(p(ShortThrow, Index), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)) :-
	shortpass_to_pass_dont(p(ShortThrow, Index, _), Length, Jugglers, MaxHeight, p(Throw, Index, Origen)).
shortpass_to_pass_dont(p(ShortThrow, Index, Origen), Length, Jugglers, MaxHeight, p(Throw, NewIndex, NewOrigen)) :-
	number(ShortThrow),
	Prechator is Length rdiv Jugglers,
	IndexMax is Jugglers - 1,
	MaxHeightSolo is MaxHeight + Length,
	float_to_shortpass(ShortThrow, ShortThrowShortend),
	(nonvar(Index) -> NewIndex = Index; true),
	(nonvar(Origen) -> NewOrigen = Origen; true),
	between(1, IndexMax, Index),
	between(1, MaxHeightSolo, Origen),
    Throw is Origen - (Jugglers - Index) * Prechator,
	float_to_shortpass(Throw, ShortThrowShortend), !.

convertShortPasses(Var,_,_,_,Var) :- var(Var), !.
convertShortPasses([],_,_,_,[]) :- !.
convertShortPasses([HeadShort|TailShort],Length,Persons,Max,[Head|Tail]) :-
	convertShortPasses(HeadShort,Length,Persons,Max,Head),
	convertShortPasses(TailShort,Length,Persons,Max,Tail).
convertShortPasses(ShortPass,Length,Persons,Max,Pass) :-
	not(is_list(ShortPass)),
	shortpass_to_pass(ShortPass,Length,Persons,Max,Pass).


convertShortPassesDont([],_,_,_,[]) :- !.
convertShortPassesDont([HeadShort|TailShort],Length,Persons,Max,[Head|Tail]) :-
	convertShortPassesDont(HeadShort,Length,Persons,Max,Head),
	convertShortPassesDont(TailShort,Length,Persons,Max,Tail).
convertShortPassesDont(ShortPass,Length,Persons,Max,Pass) :-
	not(is_list(ShortPass)),!,
	shortpass_to_pass_dont(ShortPass,Length,Persons,Max,Pass).


