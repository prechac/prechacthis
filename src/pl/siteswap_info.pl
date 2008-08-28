
shift_by_minuend(OldPosition, Times, NewPosition, NumberOfJugglers, Period) :-
	Minuend is Period rdiv NumberOfJugglers,
	NewPosition is OldPosition + Times * Period - truncate(Times * Minuend).
	

siteswap_position(Juggler, Position, SiteswapPosition, NumberOfJugglers, Period) :-	 %% Pos. = 1 is what Juggler does first ...
	shift_by_minuend(Position, Juggler, SiteswapPosition, NumberOfJugglers, Period).

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
	
	
applyNewSwaps(OldSwapList, NewSwaps, SwapList) :-
	intersection(OldSwapList, NewSwaps, Intersection),
	subtract(OldSwapList, Intersection, RemainingOld),
	subtract(NewSwaps, Intersection, RemainingNew),
	union(RemainingOld, RemainingNew, SwapList).
	
	

	
	
	
%%% --- print ---

print_pattern_info(Pattern, NumberOfJugglers) :-
	print_pattern_info(Pattern, NumberOfJugglers, [], [], '').
print_pattern_info(PatternWithShortPasses, NumberOfJugglers, OldSwapList, NewSwaps, BackURL) :-
	applyNewSwaps(OldSwapList, NewSwaps, SwapList),
	length(PatternWithShortPasses, Period),
	maxHeight(PatternWithShortPasses, ShortMaxHeight),
	MaxHeight is truncate(ShortMaxHeight) + 1,
	convertShortPasses(PatternWithShortPasses,Period,NumberOfJugglers,MaxHeight,Pattern),
	all_points_in_time(PointsInTime, NumberOfJugglers, Period),
	what_happens(PointsInTime, Pattern, NumberOfJugglers, ActionList),
	writePattern(Pattern, PatternWithShortPasses, NumberOfJugglers, BackURL),
	writePatternInfo(PointsInTime, ActionList, NumberOfJugglers, Period),
	writeOrbitInfo(Pattern, PatternWithShortPasses, NumberOfJugglers),
	averageNumberOfClubs(Pattern, AverageNumberOfClubs),
	NumberOfClubs is AverageNumberOfClubs * NumberOfJugglers,
	(testClubDistribution(ActionList, NumberOfJugglers, Period, NumberOfClubs) ->
		true;
		format("<p class='info_clubdistri'>Not a possible starting point without extra throws ahead.<br>Number of clubs not correct!<br>Try to turn pattern.</p>\n\n")
	),
	JugglerMax is NumberOfJugglers - 1,
	forall(between(0, JugglerMax, Juggler), writeJugglerInfo(Juggler, ActionList, SwapList, NumberOfJugglers, Period, PatternWithShortPasses, BackURL)),
	writeJoepassLink(PatternWithShortPasses, NumberOfJugglers, SwapList).
	
writePatternInfo(PointsInTime, ActionList, NumberOfJugglers, Period) :-
	format("<table class='info_pattern_table' align='center'>\n"),
/*	
	format("<td class='info_lable_swap'>point in time:</td>\n"),
	forall(member(Point, PointsInTime), (shortPointInTime(Point, ShortPoint), format("<td class='info_pointintime'>~w</td>\n", [ShortPoint]))),
	format("</tr>\n"),
*/
	JugglerMax is NumberOfJugglers - 1,
	forall(between(0, JugglerMax, Juggler), print_jugglers_throws(Juggler, ActionList, PointsInTime, NumberOfJugglers, Period)),
	format("</table>\n\n").

writeJugglerInfo(Juggler, ActionList, SwapList, NumberOfJugglers, Period, Pattern, BackURL) :-
	ColspanLong is Period,
	ColspanShort is Period - 1,
	jugglerShown(Juggler, JugglerShown),
	clubsInHand(Juggler, a, Period, ActionList, ClubsHandA),
	clubsInHand(Juggler, b, Period, ActionList, ClubsHandB),
	handShown(Juggler, a, SwapList, HandShownA),
	handShownLong(HandShownA, HandShownALong),
	handShown(Juggler, b, SwapList, HandShownB),
	handShownLong(HandShownB, HandShownBLong),
	format("<table class='info_juggler_table'>"),
	format("<tr>\n"),
	writeSwapLink(Juggler, SwapList, NumberOfJugglers, Pattern, BackURL),
	format("<th class='info_title' colspan=~w>juggler ~s</th>\n", [ColspanLong, JugglerShown]),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>clubs in ~w hand:</td>\n", [HandShownALong]),	
	format("<td class='info_clubs'>~w</td>\n", [ClubsHandA]),
	format("<td class='info_clubs' colspan=~w>&nbsp;</th>\n", [ColspanShort]),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>clubs in ~w hand:</td>\n", [HandShownBLong]),	
	format("<td class='info_clubs'>~w</td>\n", [ClubsHandB]),
	format("<td class='info_clubs' colspan=~w>&nbsp;</th>\n", [ColspanShort]),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>throwing hand:</td>\n"),
	forall(member(Action, ActionList), print_throwing_hand(Juggler, Action, SwapList)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>throw:</td>\n"),
	forall(member(Action, ActionList), print_throw(Juggler, Action, NumberOfJugglers, Period)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>cross/tramline:</td>\n"),
	forall(member(Action, ActionList), print_cross_tramline(Juggler, Action, SwapList)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>catching juggler:</td>\n"),
	forall(member(Action, ActionList), print_catching_juggler(Juggler, Action)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>catching hand:</td>\n"),
	forall(member(Action, ActionList), print_catching_hand(Juggler, Action, SwapList)),
	format("</tr>\n"),
/*
	format("<tr>\n"),
	format("<td class='info_lable'>throwing time:</td>\n"),
	forall(member(Action, ActionList), print_throwing_time(Juggler, Action)),
	format("</tr>\n"),
	format("<tr>\n"),
	format("<td class='info_lable'>landing time:</td>\n"),
	forall(member(Action, ActionList), print_landing_time(Juggler, Action)),
	format("</tr>\n"),
*/
	format("</table>\n\n").
	
writePattern(Pattern, PatternWithShortPasses, NumberOfJugglers, BackURL) :-
	format("<table class='info_bigSwap_table' align='center'>\n"),
	writePrechacThisLinks(Pattern, up, NumberOfJugglers, BackURL),
	writeBigSwapAndRotations(Pattern, PatternWithShortPasses, NumberOfJugglers, BackURL),
	writePrechacThisLinks(Pattern, down, NumberOfJugglers, BackURL),
	format("</table>\n\n").
	
writeSwapLink(Juggler, SwapList, NumberOfJugglers, Pattern, BackURL) :-
	NewSwaps = [Juggler],
	pattern_to_string(Pattern, PatternStr),
	format("<td class='info_swaplink'><a href='info.php?pattern=~s&persons=~w&swap=~w&newswap=~w&back=~w'>swap hands</a></td>\n", [PatternStr, NumberOfJugglers, SwapList, NewSwaps, BackURL]).

writeBigSwapAndRotations(Pattern, PatternWithShortPasses, NumberOfJugglers, BackURL) :-
	rotate_left(PatternWithShortPasses, PatternRotatedLeft),
	pattern_to_string(PatternRotatedLeft, PatternRotatedLeftStr),
	rotate_right(PatternWithShortPasses, PatternRotatedRight),
	pattern_to_string(PatternRotatedRight, PatternRotatedRightStr),
	sformat(ArrowLeft, "<img src='./images/left_arrow.png' alt='rotate left' border=0>", []),
	sformat(ArrowRight, "<img src='./images/right_arrow.png' alt='rotate right' border=0>", []),
	format("<tr>\n"),
	format("<td class='info_left_arrow'><a href='./info.php?pattern=~s&persons=~w&back=~w' title='rotate left'>~w</a></td>\n", [PatternRotatedLeftStr,NumberOfJugglers,BackURL,ArrowLeft]),
	writeBigSwap(Pattern, NumberOfJugglers),
	format("<td class='info_right_arrow'><a href='./info.php?pattern=~s&persons=~w&back=~w' title='rotate right'>~w</a></td>\n", [PatternRotatedRightStr,NumberOfJugglers,BackURL,ArrowRight]),
	format("</tr>\n").
	
writeBigSwap(Throws) :-
	concat_atom(Throws, '</h1></td><td class="big_swap"><h1 class="big_swap">', Swap),
	format("<td class='big_swap'><h1 class='big_swap'>"),
	format(Swap),
	format("</h1></td>\n").

writeBigSwap(Throws, Persons) :-
	length(Throws, Length),
    convertP(Throws, ThrowsP, Length, Persons),
	magicPositions(Throws, Persons, MagicPositions),
	convertMagic(ThrowsP, MagicPositions, ThrowsPM),
	convertMultiplex(ThrowsPM,ThrowsPMM),
    writeBigSwap(ThrowsPMM).
	

writePrechacThisLinks(Pattern, UpDown, NumberOfJugglers, BackURL) :-
	length(Pattern, Period),
	PosMax is Period - 1,
	format("<tr><td>&nbsp;</td>\n"),
	forall(between(0, PosMax, Pos), 
		(
			prechacThis(Pattern, Pos, UpDown, NumberOfJugglers, NewPattern),
			writePrechacThisLink(NewPattern, UpDown, NumberOfJugglers, BackURL)
		)
	),
	format("<td>&nbsp;</td></tr>\n").
	
writePrechacThisLink(false, _UpDown, _NumberOfJugglers, _BackURL) :-
	!,
	format("<td class='prechacthis_link'>"),
	format("&nbsp;"),
	format("</td>\n").
writePrechacThisLink(Pattern, UpDown, NumberOfJugglers, BackURL) :-
	float_to_shortpass(Pattern, PatternShort),
	pattern_to_string(PatternShort, PatternString),
	arrowUpDown(UpDown, ArrowUpDown),
	format("<td class='prechacthis_link'>"),
	format("<a href='./info.php?pattern=~s&persons=~w&back=~w' title='PrechacThis ~w'>~s</a>", [PatternString, NumberOfJugglers, BackURL, UpDown, ArrowUpDown]),
	format("</td>\n").

arrowUpDown(up, String) :-
	format(string(String), "<img src='./images/up.png' alt='up' border=0>", []).
arrowUpDown(down, String) :-
	format(string(String), "<img src='./images/down.png' alt='down' border=0>", []).
	

writeOrbitInfo(Pattern, PatternWithShortPasses, NumberOfJugglers) :-
	orbits(Pattern, OrbitPattern),
	magicPositions(Pattern, NumberOfJugglers, MagicPositions),
	length(Pattern, Period),
	convertP(PatternWithShortPasses, PatternP, Period, NumberOfJugglers),
	convertMagic(PatternP, MagicPositions, PatternPM),
	convertMultiplex(PatternPM, PatternPMM),
	flatten(OrbitPattern, OrbitsFlat),
	list_to_set(OrbitsFlat, OrbitsSet),
	sort(OrbitsSet, Orbits),
	format("<table class='info_pattern_table' align='center'>\n"),
	Colspan is Period,
	averageNumberOfClubs(Pattern, AVClubs),
	Clubs is AVClubs * NumberOfJugglers,
	format("<td class='info_title' colspan=~w>orbits</td><td class='info_right_info'>~w clubs</td>\n", [Colspan, Clubs]),
	forall(member(Orbit, Orbits), writeThisOrbitInfo(OrbitPattern, Orbit, Pattern, NumberOfJugglers, PatternPMM)),
	format("</table>\n\n").
	
writeThisOrbitInfo(OrbitPattern, Orbit, Pattern, NumberOfJugglers, PatternPMM) :-	
	clubsInOrbit(Pattern, OrbitPattern, Orbit, ClubsAV),
	Clubs is ClubsAV * NumberOfJugglers,
	justThisOrbit(PatternPMM, OrbitPattern, Orbit, PatternPrint, print),
	concat_atom(PatternPrint, '</td><td class="info_throw">', Swap),
	format("<tr>\n"),
	format("<td class='info_throw'>"),
	format(Swap),
	format("</td><td class='info_right_info'>~w</td>\n", [Clubs]),
	format("</tr>\n").

writeOrbits(Pattern, NumberOfJugglers) :-
	orbits(Pattern, Orbits),
	orbitShown(Orbits, OrbitsShown),
	clubsInOrbits(Pattern, Orbits, AvClubs),
	multiply(AvClubs, NumberOfJugglers, Clubs),	
	concat_atom(OrbitsShown, '</td><td class="info_orbits">', OrbitsTDs),
	format("<tr>\n<td class='info_lable'>Orbits:</td>\n<td class='info_orbits'>"),
	format(OrbitsTDs),
	format("</td>\n<td>&nbsp;</td>\n</tr>\n"),
	length(Pattern, Length),
	Colspan is Length + 2,
	format("<tr><td colspan='~w'>", [Colspan]),
	print(Clubs),
	format("</td></tr>\n").
	
writeJoepassLink(Pattern, NumberOfJugglers, SwapList) :-
	pattern_to_string(Pattern, PatternStr),
	jp_filename(Pattern, FileName),
	format("<div class='jp_link'>\n"),
	format("<form action='./joepass.php' method='post'>\n"),
	format("<input type='hidden' name='pattern' value='~s'>\n", [PatternStr]),
	format("<input type='hidden' name='persons' value='~w'>\n", [NumberOfJugglers]),
	format("<input type='hidden' name='file' value='~w'>\n", [FileName]),
	format("<input type='hidden' name='swap' value='~w'>\n", [SwapList]),
	format("JoePass! file:&nbsp;\n"),
	format("<select name='download' size='1'>"),
	format("<option value='on'>download</option>"),
	format("<option value='off'>show</option>"),
	format("</select>\n"),
	(NumberOfJugglers = 2 ->
		(
			format("&nbsp;"),
			format("<select name='style' size='1'>"),
			format("<option value='normal'>face to face</option>"),
			format("<option value='sidebyside'>side by side</option>"),
			format("</select>\n")
		); true
	),
	format("&nbsp;"),
	format("<input type='submit' value='go'>\n"),
	format("</form>\n"),
	%format("<a href='joepass.php?pattern=~s&persons=~w&file=~w&swap=~w'>show</a>", [PatternStr, NumberOfJugglers, FileName, SwapList]),
	%format("/"),
	%format("<a href='joepass.php?pattern=~s&persons=~w&file=~w&swap=~w&download=on'>download</a>", [PatternStr, NumberOfJugglers, FileName, SwapList]),
	%format(" JoePass! file\n"),
	format("</div>\n").
	
	
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



%%%  Action = [PointInTime, ThrowingJuggler, ThrowingSiteswapPosition, Throw, LandingTime, CatchingJuggler, LandingSiteswapPosition].



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

print_throwing_hand(ThrowingJuggler, Action, SwapList) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(3, Action, ThrowingSiteswapPosition),
	hand(ThrowingSiteswapPosition, Hand),
	handShown(ThrowingJuggler, Hand, SwapList, HandShown),
	format("<td class='info_hand'>~w</td>\n", [HandShown]).
print_throwing_hand(_, _, _).

print_cross_tramline(ThrowingJuggler, Action, _SwapList) :-
	nth1(2, Action, ThrowingJuggler),
	nth1(6, Action, ThrowingJuggler),!, % self
	format("<td class='info_cross'>&nbsp;</td>\n").
print_cross_tramline(ThrowingJuggler, Action, SwapList) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(3, Action, ThrowingSiteswapPosition),
	nth1(6, Action, CatchingJuggler),
	nth1(7, Action, CatchingSiteswapPosition),
	hand(ThrowingSiteswapPosition, ThrowingHand),
	hand(CatchingSiteswapPosition, CatchingHand),
	handShown(ThrowingJuggler, ThrowingHand, SwapList, ThrowingHandShown),
	handShown(CatchingJuggler, CatchingHand, SwapList, CatchingHandShown),
	cross_or_tramline(ThrowingHandShown, CatchingHandShown, CrossOrTram),
	format("<td class='info_cross'>~s</td>\n", [CrossOrTram]).
print_cross_tramline(_, _, _).

cross_or_tramline(Hand, Hand, "X") :- !.
cross_or_tramline(_HandA, _HandB, "|&nbsp;|") :- !.

print_catching_juggler(ThrowingJuggler, Action) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(6, Action, CatchingJuggler),
	jugglerShown(CatchingJuggler, JugglerShown),
	format("<td class='info_juggler'>~s</td>\n", [JugglerShown]).
print_catching_juggler(_, _).
	
print_catching_hand(ThrowingJuggler, Action, SwapList) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(6, Action, CatchingJuggler),
	nth1(7, Action, CatchingSiteswapPosition),
	hand(CatchingSiteswapPosition, Hand),
	handShown(CatchingJuggler, Hand, SwapList, HandShown),
	format("<td class='info_hand'>~w</td>\n", [HandShown]).
print_catching_hand(_, _, _).


print_landing_time(ThrowingJuggler, Action) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(5, Action, Time),
	shortPointInTime(Time, ShortTime),
	format("<td class='info_pointintime'>~w</td>\n", [ShortTime]).
print_landing_time(_, _).

orbitShown([], []) :- !.
orbitShown([Orbit|ListOrbit], [Shown|ListShown]) :-
	!,
	orbitShown(Orbit, Shown),
	orbitShown(ListOrbit, ListShown).
orbitShown(Orbit, OrbitShown) :-
	OrbitList = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'],
	nth0(Orbit, OrbitList, OrbitShown).	


jugglerShown(Juggler, JugglerShown) :-
	JugglerList = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"],
	nth0(Juggler, JugglerList, JugglerShown).
	
handShown(Juggler, a, SwapList, l) :-
	member(Juggler, SwapList), !.
handShown(Juggler, a, SwapList, r) :-	
	not(member(Juggler, SwapList)), !.
handShown(Juggler, b, SwapList, r) :-
	member(Juggler, SwapList), !.
handShown(Juggler, b, SwapList, l) :- 
	not(member(Juggler, SwapList)), !.

handShownLong(r, right) :- !.
handShownLong(l, left) :- !.
	 
	
