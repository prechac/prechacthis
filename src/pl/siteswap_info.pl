
shift_by_minuend(OldPosition, Times, NewPosition, NumberOfJugglers, Period) :-
	Minuend is Period rdiv NumberOfJugglers,
	NewPosition is OldPosition + Times * Period - truncate(Times * Minuend).
	

siteswap_position(Juggler, Position, SiteswapPosition, NumberOfJugglers, Period) :-	 %% Pos. = 1 is what Juggler does first ...
	shift_by_minuend(Position, Juggler, SiteswapPosition, NumberOfJugglers, Period).

throw_time(ThrowingJuggler, Position, Time, NumberOfJugglers, Period) :-
	JugglerMax is NumberOfJugglers - 1,
	between(0, JugglerMax, ThrowingJuggler),
	Minuend is Period rdiv NumberOfJugglers,
	TimePlusOne is Time + 1,
	PositionPlusOne is Position + 1,
	TimePlusOne is float_fractional_part(ThrowingJuggler * Minuend) + PositionPlusOne. %% Bug in Prolog: 0.2 is float_fractional_part(2.2). --> No !?!? 
	
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


pass_info(PassingJuggler, Position, Pass, ThrowingTime, ThrowingSiteswapPosition, LandingTime, CatchingJuggler, LandingSiteswapPosition, NumberOfJugglers, Period) :-
	pass_to(PassingJuggler, Position, Pass, ThrowingTime, LandingTime, CatchingJuggler, LandingSiteswapPosition, NumberOfJugglers, Period),
	siteswap_position(PassingJuggler, Position, ThrowingSiteswapPosition, NumberOfJugglers, Period).
	
%% Multiplex


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
	
what_happens([], _, _, []).
what_happens([Point|PointsInTime], Pattern, NumberOfJugglers, Action) :-
	findall(ThisAction, what_happens_at_point_in_time(Point, Pattern, NumberOfJugglers, ThisAction), ActionBag),
	what_happens(PointsInTime, Pattern, NumberOfJugglers, RestAction),
	append(ActionBag,RestAction,Action).



shortPointInTime(PointInTime, ShortPointInTime) :-
	ShortPointInTime is truncate(PointInTime*10)/10.

hand(Position, a) :- even(Position),!.
hand(Position, b) :- odd(Position),!.

	
nextPeriodActionList([], _Period, []) :- !.
nextPeriodActionList([FirstAction|FirstPeriod], Period, [SecondAction|SecondPeriod]) :-
	FirstAction = [PointInTime, ThrowingJuggler, ThrowingSiteswapPosition, Throw, LandingTime, CatchingJuggler, LandingSiteswapPosition],
	SecondPointInTime is PointInTime + Period,
	SecondThrowingSiteswapPosition is ThrowingSiteswapPosition + Period,
	SecondLandingTime is LandingTime + Period,
	SecondLandingSiteswapPosition is LandingSiteswapPosition + Period,
	SecondAction = [SecondPointInTime, ThrowingJuggler, SecondThrowingSiteswapPosition, Throw, SecondLandingTime, CatchingJuggler, SecondLandingSiteswapPosition],
	nextPeriodActionList(FirstPeriod, Period, SecondPeriod).
	

clubsInHand(Juggler, Hand, Period, ActionList, ClubsInHand) :-
	member(Hand, [a,b]),
	numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, ActionList, ActionList, NumberOfThrows, _FirstCatch),
	ClubsInHand = NumberOfThrows.


listOfCatches(_,_,_,[],[]).
listOfCatches(CatchingJuggler, Hand, Period, [Action|ActionList], [Catch|ListOfCatches]) :-	
	not(nth1(4, Action, p(0,_,_))), % Throw not 0
	nth1(6, Action, CatchingJuggler),
	((		
		nth1(7, Action, CatchingSiteswapPosition),
		hand(CatchingSiteswapPosition, Hand),
		nth1(5, Action, Catch)
	);(
		odd(Period),
		nth1(5, Action, FirstCatch),
		Catch is FirstCatch + Period		
	)),
	!,
	listOfCatches(CatchingJuggler, Hand, Period, ActionList, ListOfCatches).
listOfCatches(CatchingJuggler, Hand, Period, [_|ActionList], ListOfCatches) :-	
	listOfCatches(CatchingJuggler, Hand, Period, ActionList, ListOfCatches).


firstCatch(Juggler, Hand, Period, ActionList, FirstCatch) :-
	member(Hand, [a,b]),
	listOfCatches(Juggler, Hand, Period, ActionList, ListOfCatches),
	min_of_list(FirstCatch, ListOfCatches).

%%%  Action = [PointInTime, ThrowingJuggler, ThrowingSiteswapPosition, Throw, LandingTime, CatchingJuggler, LandingSiteswapPosition].

numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, [], OldActionList, NumberOfThrows, FirstCatch) :- 
	nextPeriodActionList(OldActionList, Period, NewActionList),
	numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, NewActionList, NewActionList, NumberOfThrows, FirstCatch),
	!.
numberOfThrowsUntilFirstCatch(Juggler, Hand, _, [Action|_ActionList], _OriginalAction, 0, FirstCatch) :- 
	nonvar(FirstCatch),
	nth1(1, Action, FirstCatch), % point of time is time of first catch
	nth1(2, Action, Juggler), % Juggler is throwing
	nth1(3, Action, ThrowingSiteswapPosition),
	hand(ThrowingSiteswapPosition, Hand), % Juggler is throwing with this hand
	!. 
numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, [Action|ActionList], OriginalAction, NumberOfThrows, FirstCatch) :-	
	nth1(4, Action, p(0,_,_)), % throw is a 0
	!,
	numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, ActionList, OriginalAction, NumberOfThrows, FirstCatch).
numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, [Action|ActionList], OriginalAction, NewNumberOfThrows, OldFirstCatch) :-	
	nth1(2, Action, Juggler),   % Juggler is throwing
	nth1(3, Action, ThrowingSiteswapPosition),
	hand(ThrowingSiteswapPosition, Hand), % Juggler is throwing with this hand
	nth1(6, Action, Juggler),   % Juggler is catching
	not(nth1(4, Action, p(0,_,_))),
	nth1(7, Action, CatchingSiteswapPosition),
	hand(CatchingSiteswapPosition, Hand),
	nth1(5, Action, Catch), % Juggler is catching with this hand
	!,
	earlierCatch(OldFirstCatch, Catch, NewFirstCatch),
	numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, ActionList, OriginalAction, OldNumberOfThrows, NewFirstCatch),	
	NewNumberOfThrows is OldNumberOfThrows + 1.
numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, [Action|ActionList], OriginalAction, NewNumberOfThrows, FirstCatch) :-	
	nth1(2, Action, Juggler),   % Juggler is throwing
	nth1(3, Action, ThrowingSiteswapPosition),
	hand(ThrowingSiteswapPosition, Hand), % Juggler is throwing with this hand
	!,
	numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, ActionList, OriginalAction, OldNumberOfThrows, FirstCatch),	
	NewNumberOfThrows is OldNumberOfThrows + 1.
numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, [Action|ActionList], OriginalAction, NumberOfThrows, OldFirstCatch) :-	
	nth1(6, Action, Juggler),   % Juggler is catching
	not(nth1(4, Action, p(0,_,_))),
	nth1(7, Action, CatchingSiteswapPosition),
	hand(CatchingSiteswapPosition, Hand),
	nth1(5, Action, Catch), % Juggler is catching with this hand
	!,
	earlierCatch(OldFirstCatch, Catch, NewFirstCatch),
	numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, ActionList, OriginalAction, NumberOfThrows, NewFirstCatch).
numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, [_Action|ActionList], OriginalAction, NumberOfThrows, FirstCatch) :-	
	numberOfThrowsUntilFirstCatch(Juggler, Hand, Period, ActionList, OriginalAction, NumberOfThrows, FirstCatch).
	

	
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
	
	
testClubDistribution(ActionList, NumberOfJugglers, Period, ClubsInPattern) :-
	JugglerMax is NumberOfJugglers - 1,
	findall(
		ClubsInHand, 
		(
			between(0, JugglerMax, Juggler),
			member(Hand, [a,b]),
			clubsInHand(Juggler, Hand, Period, ActionList, ClubsInHand)
		),
		ListOfClubs
	),
	sumlist(ListOfClubs, ClubsInPattern).
	


%%% --- print ---

print_pattern_info(Pattern, NumberOfJugglers) :-
	print_pattern_info(Pattern, NumberOfJugglers, '').
print_pattern_info(PatternWithShortPasses, NumberOfJugglers, BackURL) :-
	length(PatternWithShortPasses, Period),
	maxHeight(PatternWithShortPasses, ShortMaxHeight),
	MaxHeight is truncate(ShortMaxHeight) + 1,
	convertShortPasses(PatternWithShortPasses,Period,NumberOfJugglers,MaxHeight,Pattern),
	all_points_in_time(PointsInTime, NumberOfJugglers, Period),
	what_happens(PointsInTime, Pattern, NumberOfJugglers, ActionList),
	writeBigSwapAndRotations(Pattern, PatternWithShortPasses, NumberOfJugglers, BackURL),
	writePatternInfo(PointsInTime, ActionList, NumberOfJugglers, Period),
	averageNumberOfClubs(Pattern, AverageNumberOfClubs),
	NumberOfClubs is AverageNumberOfClubs * NumberOfJugglers,
	(testClubDistribution(ActionList, NumberOfJugglers, Period, NumberOfClubs) ->
		true;
		format("<p class='info_clubdistri'>Not a possible starting point without extra throws ahead.<br>Try to turn pattern.</p>\n\n")
	),
	JugglerMax is NumberOfJugglers - 1,
	forall(between(0, JugglerMax, Juggler), writeJugglerInfo(Juggler, ActionList, NumberOfJugglers, Period)).
	
	
writePatternInfo(PointsInTime, ActionList, NumberOfJugglers, Period) :-
	format("<table class='info_pattern_table' align='center'>\n"),
	format("<tr>\n"),
	format("<td class='info_lable_swap'>point in time:</td>\n"),
	forall(member(Point, PointsInTime), (shortPointInTime(Point, ShortPoint), format("<td class='info_pointintime'>~w</td>\n", [ShortPoint]))),
	JugglerMax is NumberOfJugglers - 1,
	format("</tr>\n"),
	forall(between(0, JugglerMax, Juggler), print_jugglers_throws(Juggler, ActionList, PointsInTime, NumberOfJugglers, Period)),
	format("</table>\n\n").

writeJugglerInfo(Juggler, ActionList, NumberOfJugglers, Period) :-
	ColspanLong is Period + 1,
	ColspanShort is Period - 1,
	jugglerShown(Juggler, JugglerShown),
	clubsInHand(Juggler, a, Period, ActionList, ClubsHandA),
	clubsInHand(Juggler, b, Period, ActionList, ClubsHandB),
	format("<table class='info_juggler_table'>"),
	format("<tr>\n"),
	format("<th class='info_title' colspan=~w>juggler ~s</th>\n", [ColspanLong, JugglerShown]),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>clubs in hand a:</td>\n"),	
	format("<td class='info_clubs'>~w</td>\n", [ClubsHandA]),
	format("<td class='info_clubs' colspan=~w>&nbsp;</th>\n", [ColspanShort]),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>clubs in hand b:</td>\n"),	
	format("<td class='info_clubs'>~w</td>\n", [ClubsHandB]),
	format("<td class='info_clubs' colspan=~w>&nbsp;</th>\n", [ColspanShort]),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>throw:</td>\n"),
	forall(member(Action, ActionList), print_throw(Juggler, Action, NumberOfJugglers, Period)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>throwing time:</td>\n"),
	forall(member(Action, ActionList), print_throwing_time(Juggler, Action)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>throwing hand:</td>\n"),
	forall(member(Action, ActionList), print_throwing_hand(Juggler, Action)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>landing time:</td>\n"),
	forall(member(Action, ActionList), print_landing_time(Juggler, Action)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>catching juggler:</td>\n"),
	forall(member(Action, ActionList), print_catching_juggler(Juggler, Action)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>catching hand:</td>\n"),
	forall(member(Action, ActionList), print_catching_hand(Juggler, Action)),
	format("</tr>\n"),
	format("</table>\n\n").


writeBigSwapAndRotations(Pattern, PatternWithShortPasses, NumberOfJugglers, BackURL) :-
	format("<table align='center'>\n"),
	format("<tr><td colspan=2>"),
	writeBigSwap(Pattern, NumberOfJugglers),
	format("</td></tr>\n"),
	writeRotatedLinks(PatternWithShortPasses, NumberOfJugglers, BackURL),
	format("</table>\n\n").
	
writeBigSwap(Throws) :-
   concat_atom(Throws, ' ', Swap),
   format("<h1 class='big_swap'>~w</h1>\n", [Swap]),!.

writeBigSwap(Throws, Persons) :-
	length(Throws, Length),
    convertP(Throws, ThrowsP, Length, Persons),
	convertMultiplex(ThrowsP,ThrowsPM),
    writeBigSwap(ThrowsPM).
	
writeRotatedLinks(Pattern, NumberOfJugglers, BackURL) :-
	rotate_left(Pattern, PatternRotatedLeft),
	rotate_right(Pattern, PatternRotatedRight),
	sformat(ArrowLeft, "<img src='./images/left_arrow.png' alt='rotate left' border=0>", []),
	sformat(ArrowRight, "<img src='./images/right_arrow.png' alt='rotate right' border=0>", []),
	format("<tr>\n"),
	format("<td class='info_left'><a href='./info.php?pattern=~w&persons=~w&back=~w'>~w</a></td>\n", [PatternRotatedLeft,NumberOfJugglers,BackURL,ArrowLeft]),
	format("<td class='info_right'><a href='./info.php?pattern=~w&persons=~w&back=~w'>~w</a></td>\n", [PatternRotatedRight,NumberOfJugglers,BackURL,ArrowRight]),
	format("</tr>\n").	
	
print_jugglers_throws(Juggler, ActionList, PointsInTime, NumberOfJugglers, Period) :-
	format("<tr>\n"),
	jugglerShown(Juggler, JugglerShown),
	format("<td class='info_lable_swap'>juggler ~s:</td>\n", [JugglerShown]),
	forall(member(Point, PointsInTime), print_jugglers_point_in_time(Juggler, Point, ActionList, NumberOfJugglers, Period)),
	format("</tr>\n").

print_jugglers_point_in_time(Juggler, PointInTime, ActionList, NumberOfJugglers, Period) :-
	member(Action, ActionList),
	nth1(2, Action, Juggler),
	nth1(1, Action, PointInTime),!,
	nth1(4, Action, Throw),
	convertP(Throw, ThrowP, Period, NumberOfJugglers),
	%%convertMultiplex_singleThrow(ThrowsP,ThrowsPM),
	format("<td class='info_throw'>"),
	format(ThrowP),
	format("</td>\n").
print_jugglers_point_in_time(_, _, _, _, _) :-
	format("<td class='info_throw'>&nbsp;</td>\n").




print_throw(ThrowingJuggler, Action, NumberOfJugglers, Period) :-	
	nth1(2, Action, ThrowingJuggler),!,
	nth1(4, Action, Throw),
	convertP(Throw, ThrowP, Period, NumberOfJugglers),
	%%convertMultiplex_singleThrow(ThrowsP,ThrowsPM),
	format("<td class='info_throw'>"),
	format(ThrowP),
	format("</td>\n").
print_throw(_, _, _, _).
	
print_throwing_time(ThrowingJuggler, Action) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(1, Action, Time),
	shortPointInTime(Time, ShortTime),
	format("<td class='info_pointintime'>~w</td>\n", [ShortTime]).
print_throwing_time(_, _).


print_throwing_hand(ThrowingJuggler, Action) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(3, Action, ThrowingSiteswapPosition),
	hand(ThrowingSiteswapPosition, Hand),
	format("<td class='info_hand'>~w</td>\n", [Hand]).
print_throwing_hand(_, _).

print_catching_juggler(ThrowingJuggler, Action) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(6, Action, CatchingJuggler),
	jugglerShown(CatchingJuggler, JugglerShown),
	format("<td class='info_juggler'>~s</td>\n", [JugglerShown]).
print_catching_juggler(_, _).
	
print_catching_hand(ThrowingJuggler, Action) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(7, Action, CatchingSiteswapPosition),
	hand(CatchingSiteswapPosition, Hand),
	format("<td class='info_hand'>~w</td>\n", [Hand]).
print_catching_hand(_, _).


print_landing_time(ThrowingJuggler, Action) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(5, Action, Time),
	shortPointInTime(Time, ShortTime),
	format("<td class='info_pointintime'>~w</td>\n", [ShortTime]).
print_landing_time(_, _).
	

jugglerShown(Juggler, JugglerShown) :-
	JugglerList = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"],
	nth0(Juggler, JugglerList, JugglerShown).
	