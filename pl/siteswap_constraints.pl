
siteswap(OutputPattern, NumberOfJugglersPre, ObjectsPre, LengthPre, MaxHeightPre, PassesMinPre, PassesMaxPre, ContainString, DontContainString, ClubDoesString, ReactString, ContainMagicPre) :-
	preprocess_number(NumberOfJugglersPre, NumberOfJugglers),
	preprocess_number(ObjectsPre, Objects),
	preprocess_number(LengthPre, Length),
	preprocess_number(MaxHeightPre, MaxHeight),
	preprocess_number(PassesMinPre, PassesMin),
	preprocess_number(PassesMaxPre, PassesMax),
	preprocess_number(ContainMagicPre, ContainMagic),
	initConstraintCheck,
	constraint(Pattern, Length, NumberOfJugglers, MaxHeight, ContainString, ClubDoesString, ReactString, ContainMagic),
	preprocessMultiplexes(Pattern),
	siteswap(NumberOfJugglers, Objects, MaxHeight, PassesMin, PassesMax, Pattern),
	catch(
		preprocessConstraint(DontContainString, negativ, Length, NumberOfJugglers, MaxHeight, DontContain),
		constraint_unclear,
		throw(constraint_unclear('"Exclude"'))
	),
	forall(member(DontContainPattern, DontContain), dontContainRotation(Pattern, DontContainPattern)),
	orderMultiplexes(Pattern, PatternM),
	rotateHighestFirst(PatternM, OutputPattern).

initConstraintCheck :- 
	retractall(constraintChecked(_)),!.

constraint(Constraint, Length, _Persons, _Max, "", "", "", 0) :-
	length(Constraint, Length),!.
constraint(Constraint, Length, Persons, Max, Contain, ClubDoes, React, ContainMagic) :-
	mergeConstraints(Constraint, Length, Persons, Max, Contain, ClubDoes, React, ContainMagic),
	not(supConstraintChecked(Constraint)),
	asserta(constraintChecked(Constraint)).
	
supConstraintChecked(Constraint) :-
	constraintChecked(SupConstraint),
	isRotatedSubConstraint(Constraint, SupConstraint).
	
%	cleanEqualConstraints(ListOfConstraints, SetOfConstraints).
	
mergeConstraints(ConstraintRotated, Length, Persons, Max, ContainString, ClubDoesString, ReactString, ContainMagic) :-
	length(MagicPattern, Length),
	(ContainMagic = 1 ->	
		(
			containsMagicOrbit(MagicPattern, Persons, Max)
		);
		true
	),
	catch(
		preprocessConstraint(ContainString, positiv, Length, Persons, Max, ContainConstraints),
		constraint_unclear,
		throw(constraint_unclear('"Contain"'))
	),
	catch(
		preprocessConstraint(ClubDoesString, positiv, Length, Persons, Max, ClubDoesConstraints),
		constraint_unclear,
		throw(constraint_unclear('"Club does"'))
	),
	catch(
		preprocessConstraint(ReactString, positiv, Length, Persons, Max, ReactConstraints),
		constraint_unclear,
		throw(constraint_unclear('"React"'))
	),
	findall(Pattern, (length(Pattern, Length), member(Contain,  ContainConstraints ), contains(Pattern, Contain )), BagContains),
	(BagContains = [] -> ContainConstraints = []; true),
	findall(Pattern, (length(Pattern, Length), member(ClubDoes, ClubDoesConstraints), clubDoes(Pattern, ClubDoes)), BagClubDoes),
	(BagClubDoes = [] -> ClubDoesConstraints = []; true),
	findall(Pattern, (length(Pattern, Length), member(React,    ReactConstraints   ), react(   Pattern, React   )), BagReact   ),
	(BagReact = [] -> ReactConstraints = []; true),
	% !!!!!! ?????????????????
	append([MagicPattern], BagContains, BagTmp1),
	append(BagTmp1, BagClubDoes, BagTmp2),
	append(BagTmp2, BagReact, BagOfConstraints),
	(BagOfConstraints = [] ->
			length(ConstraintRotated, Length);
			(
				mergeN(BagOfConstraints, Constraint),
				rotateHighestFirst(Constraint, ConstraintRotated)
			)
	).

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

isPass(Var, 0) :- var(Var), !.
isPass(p(_,Index,_), 1) :- Index > 0, !.
isPass(p(_,Index,_), 0) :- Index = 0, !.
isPass(Multiplex, NumberOfPasses) :- 
	is_list(Multiplex),!,
	amountOfPasses(Multiplex, NumberOfPasses).


%%% --- Constraints Pattern ---

contains(Pattern, Segment) :-
   append(Segment, _, Pattern).


% Pattern doesn't contain Segment
dontcontain(_, []) :- fail,!.
dontcontain([PatternHead|_Pattern], _) :-
   var(PatternHead),!. % one is var
dontcontain(_, [SegmentHead|_Segment]) :-
   var(SegmentHead),!. % one is var
dontcontain([PatternMultiplex|_Pattern], [SegmentThrow|_Segment]) :-
	is_list(PatternMultiplex), 
	not(is_list(SegmentThrow)),!, % PatternHead is Multiplex the other not
	multiplexDoesntContain(PatternMultiplex, SegmentThrow).
dontcontain([PatternThrow|_Pattern], [SegmentMultipex|_Segment]) :-
	not(is_list(PatternThrow)), 
	is_list(SegmentMultipex),!. % SegHead is Multiplex the other not
dontcontain([PatternMultiplex|_Pattern], [SegmentMultiplex|_Segment]) :-
	is_list(PatternMultiplex),
	is_list(SegmentMultiplex),!,  % both Multiplex
	multiplexDoesntContain(PatternMultiplex, SegmentMultiplex).
dontcontain([PatternHead|_Pattern], [SegmentHead|_Segment]) :-
	not_this_throw(PatternHead, SegmentHead), !. % not same head
dontcontain([_PatternHead|Pattern], [_SegmentHead|Segment]) :-
	dontcontain(Pattern, Segment),!. % not same tail

multiplexDoesntContain([], _) :- !.
multiplexDoesntContain([Head|Multiplex], p(T,I,O)) :-
	not_this_throw(Head, p(T,I,O)),
	multiplexDoesntContain(Multiplex, p(T,I,O)),!.
multiplexDoesntContain(_, []) :- fail,!.
multiplexDoesntContain(Multiplex, [Head|Tail]) :-
	multiplexDoesntContain(Multiplex, Head);
	multiplexDoesntContain(Multiplex, Tail).
	


not_this_throw(p(_Throw, Index, _Origen), p(_SegThrow, SegIndex, _SegOrigen)) :-
	var(SegIndex),
	Index = 0,!.
not_this_throw(p(Throw, Index, Origen), p(SegThrow, SegIndex, SegOrigen)) :-
	nonvar(SegIndex),
	(
		Throw \= SegThrow;
		Index \= SegIndex;
		Origen \= SegOrigen
	),!.
not_this_throw(p(Throw, Index, Origen), p(SegThrow, SegIndex, SegOrigen)) :-
	var(SegIndex),
	(		
			Throw \= SegThrow;
			Origen \= SegOrigen;
			Index \= SegIndex
	),!.


dontContainRotation(Pattern, Segment) :-
	forall(rotate(Pattern, Rotation), dontcontain(Rotation, Segment)).

clubDoes(Pattern, Seq) :-
   insertThrows(Pattern, 0, Seq).

react(Pattern, Seq) :-
	insertThrows(Pattern, 0, Seq, -2).

insertThrows(Pattern, Site, Seq) :-
	insertThrows(Pattern, Site, Seq, 0).
insertThrows(_,_,[],_) :- !.
insertThrows(Pattern, Site, [Throw | Rest], Delta) :-
   length(Pattern, Length),
   nth0(Site, Pattern, Throw),
   SitePlusDelta is Site + Delta,
   %Test ob Hoehe OK!?! (react: 1 2 nicht sinnvoll) !!!!!!!!!!!!!!!!!!!!!!!
   landingSite(SitePlusDelta, Throw, Length, NextSiteList),
   (is_list(NextSiteList) ->
      member(NextSite, NextSiteList);
      NextSite = NextSiteList
   ),!,   % doesn't work with [_ _] 1p   not all are found!!!!!!!!!!!!!!!!!!!!!!!!!!!
   insertThrows(Pattern, NextSite, Rest, Delta).


containsMagicOrbit(Pattern, NumberOfJugglers, MaxHeight) :-
	length(Pattern, Length),
	Prechator is Length rdiv NumberOfJugglers,
	MagicMaxHeight is min(MaxHeight, Prechator),
	possibleThrows(NumberOfJugglers, Length, MagicMaxHeight, [p(0,0,0)|PossibleThrows]),
	searchMagicThrows(PossibleThrows, MagicThrows, Prechator, Length, Length),
	clubDoes(Pattern, MagicThrows).
	
	
%% 1 = Clubs = SumThrows * Jugglers / Length ==> SumThrows = Prechator
%%
%% Orbit ==> SumOrig = N * Length
%% Orig = Throw + IndexDown * Prechator 
%% ==>
%% N = SumOrig / Length
%%   = Sum(Throw + IndexDown * Prechator) / Length
%%   = (Prechator + Sum(IndexDown * Prechator))/ Length
%%   = (Prechator + Prechator * Sum(IndexDown)) / Length
%%   = (1 + Sum(IndexDown)) / Jugglers
searchMagicThrows(_PossibleThrows, [], 0, 0, _) :- !.
searchMagicThrows(PossibleThrows, MagicThrows, Prechator, Length, OrigLength) :-
	Length =< 0,
	NextLength is OrigLength + Length,
	searchMagicThrows(PossibleThrows, MagicThrows, Prechator, NextLength, OrigLength).
searchMagicThrows(PossibleThrows, [MagicThrow|MagicThrows], Prechator, Length, OrigLength) :-
	member(MagicThrow, PossibleThrows),
	MagicThrow = p(Throw, _Index, Origen),
	Throw > 0,
	Throw =< Prechator,
	%Origen =< Length,
	PrechatorMinus is Prechator - Throw,
	LengthMinus is Length - Origen,
	searchMagicThrows(PossibleThrows, MagicThrows, PrechatorMinus, LengthMinus, OrigLength).
	
	
	
possibleThrows(NumberOfJugglers, Length, MaxHeight, PossibleThrows) :-
	findall(
		Throw,
		possibleThrow(NumberOfJugglers, Length, MaxHeight, Throw),
		PossibleThrows
	).
possibleThrow(_NumberOfJugglers, _Length, MaxHeight, p(Throw, 0, Throw)) :-
	MaxHeightI is truncate(MaxHeight),
	between(0, MaxHeightI, Throw).
possibleThrow(NumberOfJugglers, Length, MaxHeight, p(Throw, Index, Origen)) :-
	Prechator is Length rdiv NumberOfJugglers,
	IndexMax is NumberOfJugglers - 1,
	between(1, IndexMax, Index),
	MaxHeightSolo is truncate(MaxHeight + (NumberOfJugglers - Index) * Prechator),
	between(1, MaxHeightSolo, Origen),
    Throw is Origen - (NumberOfJugglers - Index) * Prechator,
	Throw >= 1,
	Throw =< MaxHeight.