
shift_by_minuend(OldPosition, Times, NewPosition, NumberOfJugglers, Period) :-
	Minuend is Period rdiv NumberOfJugglers,
	NewPosition is OldPosition + Times * Period - truncate(Times * Minuend).
	

siteswap_position(Juggler, Position, SiteswapPosition, NumberOfJugglers, Period) :-	 
	shift_by_minuend(Position, Juggler, SiteswapPosition, NumberOfJugglers, Period).

siteswap_position_general(Position, SiteswapPosition, NumberOfJugglers, Period) :-
	siteswap_position_general(Position, SiteswapPosition, _Juggler, NumberOfJugglers, Period).
	
siteswap_position_general(Position, SiteswapPosition, Juggler, NumberOfJugglers, Period) :-
	throwing_juggler(Position, LocalPosition, Juggler, NumberOfJugglers, Period),
	siteswap_position(Juggler, LocalPosition, SiteswapPosition, NumberOfJugglers, Period).


	
zerosTillPos_general([p(0,0,0)|_Pattern], 0, _NumberOfJugglers, 1) :- !.
zerosTillPos_general(_Pattern, 0, _NumberOfJugglers, 0) :- !.
zerosTillPos_general(Pattern, Pos, NumberOfJugglers, NumberOf0) :-
	length(Pattern, Period),
	siteswap_position_general(Pos, SiteswapPosition, NumberOfJugglers, Period),
	ModPos is SiteswapPosition mod Period,
	nth0(ModPos, Pattern, p(0,0,0)),!,
	NextPos is Pos - 1,
	zerosTillPos_general(Pattern, NextPos, NumberOfJugglers, NumberOf0Befor),
	NumberOf0 is NumberOf0Befor + 1.
zerosTillPos_general(Pattern, Pos, NumberOfJugglers, NumberOf0) :-
	NextPos is Pos - 1,
	zerosTillPos_general(Pattern, NextPos, NumberOfJugglers, NumberOf0).

	

throwing_juggler(Position, LocalPosition, Juggler, NumberOfJugglers, Period) :-
	throwing_order_of_jugglers(NumberOfJugglers, Period, ThrowingOrder),
	Pos is Position mod NumberOfJugglers,
	nth0(Pos, ThrowingOrder, Juggler),
	LocalPosition is Position // NumberOfJugglers. 
	
throwing_order_of_jugglers(NumberOfJugglers, Period, ThrowingOrder) :-
	findall(
		DelayF,
		(
			between(1, NumberOfJugglers, Juggler),	
			RealDelay is (Period rdiv NumberOfJugglers) * (Juggler - 1),
			Delay is float_fractional_part(RealDelay),
			DelayF is float(Delay)
		),
		Delays
	), 
	JugglerMax is NumberOfJugglers - 1,
	numlist(0, JugglerMax, Jugglers),
	keysort(Jugglers, Delays, ThrowingOrder).

throw_time(ThrowingJuggler, Position, Time, NumberOfJugglers, Period) :-
	JugglerMax is NumberOfJugglers - 1,
	between(0, JugglerMax, ThrowingJuggler),
	Minuend is Period rdiv NumberOfJugglers,
	TimeTmp is float_fractional_part(ThrowingJuggler * Minuend) + Position, %% Bug in Prolog: ?- A is float(1 rdiv 5), A is 0.2. --> No !?!? 
	abs(TimeTmp - Time) < 10^(-12).
	
pass_to_juggler(PassingJuggler, Index, CatchingJuggler, NumberOfJugglers) :-  %% Juggler = 0,1,2,...,NumberOfJugglers-1
	CatchingJuggler is (PassingJuggler + Index) mod NumberOfJugglers.
	

pass_to(PassingJuggler, Position, Pass, LandingSiteswapPosition, NumberOfJugglers, Period) :-
	pass_to(PassingJuggler, Position, Pass, _, _, _, LandingSiteswapPosition, NumberOfJugglers, Period).

pass_to(PassingJuggler, Position, p(Pass,Index,_), ThrowingTime, LandingTime, CatchingJuggler, LandingSiteswapPosition, NumberOfJugglers, Period) :-
	%% PassingJuggler = 0,1,2,3,...
	throw_time(PassingJuggler, Position, ThrowingTime, NumberOfJugglers, Period),
	LandingTime is ThrowingTime + Pass,
	LandingPosition is truncate(LandingTime),
	pass_to_juggler(PassingJuggler, Index, CatchingJuggler, NumberOfJugglers),
	siteswap_position(CatchingJuggler, LandingPosition, LandingSiteswapPosition, NumberOfJugglers, Period).
pass_to(_PassingJuggler, _Position, [], _ThrowingTime, [], [], [], _NumberOfJugglers, _Period) :- !.
pass_to(PassingJuggler, Position, [MultiplexHead|Multiplex], ThrowingTime, [LandingTimeHead|LandingTime], [CatchingJugglerHead|CatchingJuggler], [LandingSiteswapPositionHead|LandingSiteswapPosition], NumberOfJugglers, Period) :-
	pass_to(PassingJuggler, Position, Multiplex, ThrowingTime, LandingTime, CatchingJuggler, LandingSiteswapPosition, NumberOfJugglers, Period),
	pass_to(PassingJuggler, Position, MultiplexHead, ThrowingTime, LandingTimeHead, CatchingJugglerHead, LandingSiteswapPositionHead, NumberOfJugglers, Period).



pass_info(PassingJuggler, Position, Pass, ThrowingTime, ThrowingSiteswapPosition, LandingTime, CatchingJuggler, LandingSiteswapPosition, NumberOfJugglers, Period) :-
	pass_to(PassingJuggler, Position, Pass, ThrowingTime, LandingTime, CatchingJuggler, LandingSiteswapPosition, NumberOfJugglers, Period),
	siteswap_position(PassingJuggler, Position, ThrowingSiteswapPosition, NumberOfJugglers, Period).
	


what_happens_at_point_in_time(PointInTime, Pattern, NumberOfJugglers, Action) :-
	length(Pattern, Period),
	Position is truncate(PointInTime),
	throw_time(ThrowingJuggler, Position, PointInTime, NumberOfJugglers, Period),
	siteswap_position(ThrowingJuggler, Position, ThrowingSiteswapPosition, NumberOfJugglers, Period),
	ThrowingPositionInPattern is (ThrowingSiteswapPosition mod Period),
	nth0(ThrowingPositionInPattern, Pattern, Throw),
	pass_info(ThrowingJuggler, Position, Throw, PointInTime, ThrowingSiteswapPosition, LandingTime, CatchingJuggler, LandingSiteswapPosition, NumberOfJugglers, Period),
	Action = [PointInTime, ThrowingJuggler, ThrowingSiteswapPosition, Throw, LandingTime, CatchingJuggler, LandingSiteswapPosition].


possible_point_in_time(PointInTime, NumberOfJugglers, Period) :-
	Minuend is Period rdiv NumberOfJugglers,
	JugglerMax is NumberOfJugglers - 1,
	PositionMax is Period - 1,
	between(0, JugglerMax, Juggler),
	between(0, PositionMax, Position),
	PointInTime is Position + float_fractional_part(Juggler * Minuend).

all_points_in_time(PointsInTime, NumberOfJugglers, Period) :-
	setof(Point, possible_point_in_time(Point, NumberOfJugglers, Period), PointsInTimeR),
	sort_list_of_expr(PointsInTimeR, PointsInTime).
	
time_between_throws(NumberOfJugglers, Period, Time) :-
	all_points_in_time(PointsInTime, NumberOfJugglers, Period),
	nth0(1, PointsInTime, Time).

	
what_happens([], _, _, []).
what_happens([Point|PointsInTime], Pattern, NumberOfJugglers, Action) :-
	findall(ThisAction, what_happens_at_point_in_time(Point, Pattern, NumberOfJugglers, ThisAction), ActionBag),
	what_happens(PointsInTime, Pattern, NumberOfJugglers, RestAction),
	append(ActionBag,RestAction,Action).



shortPointInTime(PointInTime, ShortPointInTime) :-
	ShortPointInTime is truncate(PointInTime*10)/10.

hand([], []) :- !.
hand([Head|List], [Hand|Hands]) :-
	!,
	hand(Head, Hand),
	hand(List, Hands).
hand(Position, a) :- even(Position),!.
hand(Position, b) :- odd(Position),!.

	
nextPeriodActionList([], _Period, []) :- !.
nextPeriodActionList([FirstAction|FirstPeriod], Period, [SecondAction|SecondPeriod]) :-
	FirstAction = [PointInTime, ThrowingJuggler, ThrowingSiteswapPosition, Throw, LandingTime, CatchingJuggler, LandingSiteswapPosition],
	SecondPointInTime is PointInTime + Period,
	SecondThrowingSiteswapPosition is ThrowingSiteswapPosition + Period,
	add(LandingTime, Period, SecondLandingTime),
	add(LandingSiteswapPosition, Period, SecondLandingSiteswapPosition),
	SecondAction = [SecondPointInTime, ThrowingJuggler, SecondThrowingSiteswapPosition, Throw, SecondLandingTime, CatchingJuggler, SecondLandingSiteswapPosition],
	nextPeriodActionList(FirstPeriod, Period, SecondPeriod).
	


clubsThrown(Multiplex, Clubs) :-
	is_list(Multiplex), !,
	length(Multiplex, Clubs).
clubsThrown(Var, 0) :-
	var(Var), !.
clubsThrown(p(Zero,_,_), 0) :-
	number(Zero), 
	Zero = 0, !.
clubsThrown(Zero, 0) :-
	number(Zero), 
	Zero = 0, !.
clubsThrown(_, 1).

	
earlierCatch(Catch1, Catch2, Catch1) :-
	nonvar(Catch1),
	nonvar(Catch2),
	Catch1 < Catch2,!.
earlierCatch(Catch1, Catch2, Catch2) :-
	nonvar(Catch1),
	nonvar(Catch2),
	Catch1 > Catch2,!.
earlierCatch(Catch1, Catch2, Catch2) :-
	var(Catch1),
	nonvar(Catch2),!.
earlierCatch(Catch1, Catch2, Catch1) :-
	nonvar(Catch1),
	var(Catch2),!.
	
	
applyNewSwaps(OldSwapList, NewSwaps, SwapList) :-
	intersection(OldSwapList, NewSwaps, Intersection),
	subtract(OldSwapList, Intersection, RemainingOld),
	subtract(NewSwaps, Intersection, RemainingNew),
	union(RemainingOld, RemainingNew, SwapList).
	

club_distribution(ActionList, OrbitPattern, NumberOfClubs, NumberOfJugglers, Period, ClubDistribution) :-
	club_siteswap_positions(ActionList, OrbitPattern, NumberOfClubs, Period, SiteswapPositions),
	siteswapPosition2ClubDistribution(SiteswapPositions, NumberOfJugglers, ClubDistribution).

siteswapPosition2ClubDistribution([], NumberOfJugglers, ClubDistribution) :-
	listOf([0,0], NumberOfJugglers, ClubDistribution), !.
siteswapPosition2ClubDistribution([[Juggler, Position, _Orbit]|SiteswapPositions], NumberOfJugglers, ClubDistribution) :-	
	siteswapPosition2ClubDistribution(SiteswapPositions, NumberOfJugglers, OldClubDistribution),
	Hand is Position mod 2,
	nth0(Juggler, OldClubDistribution, OldHands),
	nth0(Hand, OldHands, OldClubs),
	Clubs is OldClubs + 1,
	changeOnePosition(OldHands, Hand, Clubs, NewHands),
	changeOnePosition(OldClubDistribution, Juggler, NewHands, ClubDistribution).
	



%%%                 1              2                     3               4         5              6                    7
%%%  Action = [PointInTime, ThrowingJuggler, ThrowingSiteswapPosition, Throw, LandingTime, CatchingJuggler, LandingSiteswapPosition].

club_siteswap_positions(ActionList, OrbitPattern, NumberOfClubs, Period, SiteswapPositions) :-
	club_siteswap_positions(ActionList, ActionList, NumberOfClubs, SiteswapPositions, OrbitPattern, Period, []).

club_siteswap_positions(_ActionList, _OldActionList, 0, [], _OrbitPattern, _Period, _ThrownTo) :- !.
club_siteswap_positions([], OldActionList, ClubCount, SiteswapPositions, OrbitPattern, Period, ThrownTo) :- 
	nextPeriodActionList(OldActionList, Period, NewActionList),
	club_siteswap_positions(NewActionList, NewActionList, ClubCount, SiteswapPositions, OrbitPattern, Period, ThrownTo),
	!.
club_siteswap_positions([Action|ActionList], OldActionList, ClubCount, SiteswapPositions, OrbitPattern, Period, ThrownTo) :-
	nth1(4, Action, p(0,0,0)),!,
	club_siteswap_positions(ActionList, OldActionList, ClubCount, SiteswapPositions, OrbitPattern, Period, ThrownTo).
club_siteswap_positions([Action|ActionList], OldActionList, ClubCount, SiteswapPositions, OrbitPattern, Period, ThrownTo) :-	
	nth1(6, Action, CatchingJuggler),
	number(CatchingJuggler), % no Multiplex
	nth1(1, Action, PointInTime),
	nth1(2, Action, ThrowingJuggler),
	nth1(3, Action, ThrowingSiteswapPosition),
	ThrowingSiteswapPositionMod is ThrowingSiteswapPosition mod Period,
	nth0(ThrowingSiteswapPositionMod, OrbitPattern, Orbit),
	member([ThrowingJuggler, PointInTime, Orbit], ThrownTo),!,
	nth1(5, Action, LandingTime),
	club_siteswap_positions(ActionList, OldActionList, ClubCount, SiteswapPositions, OrbitPattern, Period, [[CatchingJuggler, LandingTime, Orbit]|ThrownTo]).
club_siteswap_positions([Action|ActionList], OldActionList, ClubCount, [[ThrowingJuggler, ClubSiteswapPosition, Orbit]|SiteswapPositions], OrbitPattern, Period, ThrownTo) :-	
	nth1(6, Action, CatchingJuggler), 
	number(CatchingJuggler), !, % no Multiplex
	nth1(3, Action, ClubSiteswapPosition), 
	nth1(2, Action, ThrowingJuggler),
	nth1(5, Action, LandingTime),
	ClubSiteswapPositionMod is ClubSiteswapPosition mod Period,
	nth0(ClubSiteswapPositionMod, OrbitPattern, Orbit),
	NextClubCount is ClubCount - 1,
	club_siteswap_positions(ActionList, OldActionList, NextClubCount, SiteswapPositions, OrbitPattern, Period, [[CatchingJuggler, LandingTime, Orbit]|ThrownTo]).
club_siteswap_positions([Action|ActionList], OldActionList, ClubCount, NextSiteswapPositions, OrbitPattern, Period, ThrownTo) :-	
	nth1(6, Action, CatchingJugglers),
	is_list(CatchingJugglers), !, % Multiplex
	nth1(1, Action, PointInTime),
	nth1(2, Action, ThrowingJuggler),
	nth1(3, Action, ThrowingSiteswapPosition),
	nth1(4, Action, Throw),
	ThrowingSiteswapPositionMod is ThrowingSiteswapPosition mod Period,
	nth0(ThrowingSiteswapPositionMod, OrbitPattern, Orbits),
	nth1(5, Action, LandingTimes),
	club_siteswap_positions_MultiplexHelper(PointInTime, ThrowingJuggler, ThrowingSiteswapPosition, Throw, LandingTimes, CatchingJugglers, Orbits, ThrownTo, NewThrownTo, NewClubCount, NewSiteswapPositions),
	append(NewSiteswapPositions, SiteswapPositions, NextSiteswapPositions),
	append(ThrownTo, NewThrownTo, NextThrownTo),
	NextClubCount is ClubCount - NewClubCount,
	club_siteswap_positions(ActionList, OldActionList, NextClubCount, SiteswapPositions, OrbitPattern, Period, NextThrownTo).
	

club_siteswap_positions_MultiplexHelper(_PointInTime, _ThrowingJuggler, _ThrowingSiteswapPosition, [], [], [], [], _ThrownTo, [], 0, []) :- !. % ??????
club_siteswap_positions_MultiplexHelper(PointInTime, Juggler, SiteswapPosition, [p(0,0,0)|Multiplex], [_LandingTime|LandingTimes], [_CJuggler|CatchingJugglers], [_Orbit|Orbits], ThrownTo, NewThrownTo, NewClubCount, NewSiteswapPositions) :-
	!,
	club_siteswap_positions_MultiplexHelper(PointInTime, Juggler, SiteswapPosition, Multiplex, LandingTimes, CatchingJugglers, Orbits, ThrownTo, NewThrownTo, NewClubCount, NewSiteswapPositions).
club_siteswap_positions_MultiplexHelper(PointInTime, Juggler, SiteswapPosition, [_Throw|Multiplex], [LandingTime|LandingTimes], [CJuggler|CatchingJugglers], [Orbit|Orbits], ThrownTo, [[CJuggler, LandingTime, Orbit]|NewThrownTo], NewClubCount, NewSiteswapPositions) :-
	member([Juggler, PointInTime, Orbit], ThrownTo), !,
	club_siteswap_positions_MultiplexHelper(PointInTime, Juggler, SiteswapPosition, Multiplex, LandingTimes, CatchingJugglers, Orbits, ThrownTo, NewThrownTo, NewClubCount, NewSiteswapPositions).
club_siteswap_positions_MultiplexHelper(PointInTime, Juggler, SiteswapPosition, [_Throw|Multiplex], [LandingTime|LandingTimes], [CJuggler|CatchingJugglers], [Orbit|Orbits], ThrownTo, [[CJuggler, LandingTime, Orbit]|NewThrownTo], NewClubCount, [[Juggler, SiteswapPosition, Orbit]|NewSiteswapPositions]) :-
	club_siteswap_positions_MultiplexHelper(PointInTime, Juggler, SiteswapPosition, Multiplex, LandingTimes, CatchingJugglers, Orbits, ThrownTo, NewThrownTo, NextNewClubCount, NewSiteswapPositions), 
	NewClubCount is NextNewClubCount + 1.
	
	