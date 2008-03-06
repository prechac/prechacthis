

siteswap(OutputPattern, NumberOfJugglers, Objects, Length, MaxHeight, _NumberOfMultiplexes, PassesMin, PassesMax, ContainShortBag, DontContainShortBag, ClubDoesShortBag, ReactShortBag) :-
	initConstraintCheck,
	constraint(Pattern, Length, NumberOfJugglers, MaxHeight, ContainShortBag, ClubDoesShortBag, ReactShortBag),
	siteswap(NumberOfJugglers, Objects, MaxHeight, Pattern),
	(passesMin(Pattern, PassesMin); NumberOfJugglers=1),
	passesMax(Pattern, PassesMax),            
	member(DontContainShort, DontContainShortBag),
	convertShortPassesDont(DontContainShort,Length,NumberOfJugglers,MaxHeight,DontContain),  %% !!! doesn't convert Index and Original !!!!
	forall(member(DontContainPattern, DontContain), dontContainRotation(Pattern, DontContainPattern)),
	rotateHighestFirst(Pattern, OutputPattern).

initConstraintCheck :- 
	retractall(constraintChecked(_)),!.

constraint(Constraint, Length, _Persons, _Max, [[]], [[]], [[]]) :-
	length(Constraint, Length),!.
constraint(Constraint, Length, Persons, Max, Contain, ClubDoes, React) :-
	mergeConstraints(Constraint, Length, Persons, Max, Contain, ClubDoes, React),
	not(supConstraintChecked(Constraint)),
	asserta(constraintChecked(Constraint)).
	
supConstraintChecked(Constraint) :-
	constraintChecked(SupConstraint),
	isRotatedSubConstraint(Constraint, SupConstraint).
	
%	cleanEqualConstraints(ListOfConstraints, SetOfConstraints).
	
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
	isSubConstraint(SubConstraint, ConstraintRotated),!.

isSubConstraint([], []) :- !.
isSubConstraint([SubThrow|SubConstraint], [Throw|Constraint]) :-
	nonvar(SubThrow), nonvar(Throw), !,
	SubThrow = Throw,
	isSubConstraint(SubConstraint, Constraint).
isSubConstraint([_SubThrow|SubConstraint], [Throw|Constraint]) :-
	var(Throw), !,
	isSubConstraint(SubConstraint, Constraint).


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

