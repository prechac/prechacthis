jp_pattern_def(PatternShort, NumberOfJugglers, SwapList, Style) :-
	length(PatternShort, Period),
	maxHeight(PatternShort, ShortMaxHeight),
	MaxHeight is truncate(ShortMaxHeight) + 1,
	convertShortPasses(PatternShort, Period, NumberOfJugglers, MaxHeight, Pattern),
	all_points_in_time(PointsInTime, NumberOfJugglers, Period),
	what_happens(PointsInTime, Pattern, NumberOfJugglers, ActionList),
	jp_listOfJugglerStartLeft(Period, NumberOfJugglers, SwapList, LeftList),
	jp_header,
	jp_positions(NumberOfJugglers, Style),
	(noMultiplex(Pattern) -> 
		jp_colors(Pattern, NumberOfJugglers);
		true
	),
	jp_jugglerStartLeft(LeftList),
	jp_delay(NumberOfJugglers, Period, Delay),
	jp_pattern(ActionList, SwapList, LeftList, PointsInTime, Delay, NumberOfJugglers).

jp_header :- 
	format("#sx\n\n").

jp_positions(_, sidebyside) :-
	format("#sidetoside\n"), !.
jp_positions(NumberOfJugglers, shortdistance) :-
	D = 150,
	forall(between(1, NumberOfJugglers, Juggler), jp_printJugglerPosition(Juggler, NumberOfJugglers, circle, D)), !.
jp_positions(_NumberOfJugglers, _Style) :- !.

jp_printJugglerPosition(Juggler, NumberOfJugglers, circle, D) :-
	X is 2 * pi * (Juggler - 1)/NumberOfJugglers,
	Radius is D / (2 * sin(pi/NumberOfJugglers)),
	U is round(Radius * cos(X)),
	V is round(Radius * sin(X)),
	format("#j ~w (~w,0,~w)(0,0,0)\n", [Juggler,U,V]).

jp_colors(Pattern, NumberOfJugglers) :- 
	orbits(Pattern, OrbitPattern),
	averageNumberOfClubs(Pattern, AVClubs),
	NumberOfClubs is AVClubs * NumberOfJugglers,
	forall(between(1, NumberOfClubs, Club), jp_printClubColor(Club, Pattern, OrbitPattern, NumberOfJugglers)).

% !!! Multiplex ToDo
jp_printClubColor(Club, Pattern, ListOfColors, NumberOfJugglers) :-
	Club0 is Club - 1,
	club_siteswap_position(Club0, Pattern, SiteswapPosition, NumberOfJugglers),
	length(ListOfColors, Length),
	ColorPos is SiteswapPosition mod Length,
	nth0(ColorPos, ListOfColors, ColorNo),
	jp_Color(ColorNo, Color),
	format("#O ~w ~w\n", [Club, Color]).
	
jp_Color(Number, Color) :-
	ColorList = ['(1,0,0)','(0,1,0)','(0,0,1)','(0,1,1)','(1,0,1)','(1,1,0)','(1,1,1)','(0,0,0)','(0.5,0,0)','(0,0.5,0)','(0,0,0.5)','(0,0.5,0.5)','(0.5,0,0.5)','(0.5,0.5,0)','(0.5,0.5,0.5)'],
	length(ColorList, NumberOfColors),
	Pos is Number mod NumberOfColors,
	nth0(Pos, ColorList, Color).

jp_jugglerStartLeft([]) :- !.
jp_jugglerStartLeft([Juggler|LeftList]) :-
	JugglerShown is Juggler + 1,
	format("#jugglerStartLeft ~w\n", [JugglerShown]),
	jp_jugglerStartLeft(LeftList).

jp_delay(NumberOfJugglers, Period, nodelay) :-
	Prechator is (Period rdiv NumberOfJugglers),
	0 is float_fractional_part(Prechator), !.
jp_delay(NumberOfJugglers, Period, delay) :-
	format("#D -\n"),
	JugglerMax is NumberOfJugglers - 1,
	forall(
		between(0, JugglerMax, Juggler),
		(	
			RealDelay is (Period rdiv NumberOfJugglers) * Juggler,
			Delay is float_fractional_part(RealDelay),
			DelayF is float(Delay),
			JugglerShown is Juggler + 1,
			(Delay = 0 -> true; format("#jugglerDelay ~w ~w\n", [JugglerShown, DelayF]))
		)
	).


jp_pattern(ActionList, SwapList, LeftList, PointsInTime, Delay, NumberOfJugglers) :-
	JugglerMax is NumberOfJugglers - 1,
	format("\n< "),
	forall(
		between(0, JugglerMax, Juggler), 
		jp_print_jugglers_throws(Juggler, ActionList, SwapList, LeftList, PointsInTime, Delay)
	),
	format(">").	

	
jp_print_jugglers_throws(Juggler, ActionList, SwapList, LeftList, PointsInTime, Delay) :-
	(Juggler = 0 -> true; format("|\n  ")),
	forall(member(Point, PointsInTime), jp_print_jugglers_point_in_time(Juggler, Point, SwapList, LeftList, ActionList, Delay)).

jp_print_jugglers_point_in_time(Juggler, PointInTime, SwapList, LeftList, ActionList, Delay) :-
	member(Action, ActionList),
	nth1(2, Action, Juggler),
	nth1(1, Action, PointInTime),!,
	nth1(4, Action, Throw),
	%%jp_throwing_hand(Juggler, Action, SwapList, RightOrLeft),
	jp_cross_tramline(Juggler, Action, SwapList, LeftList, Delay, CrossOrTram),
	jp_convertP(Throw, CrossOrTram, ThrowP),
	jp_formatThrow(ThrowP),
	format(" ").
jp_print_jugglers_point_in_time(_, _, _, _, _, _) :- !.
	
jp_formatThrow(Multiplex) :-
	is_list(Multiplex), !,
	format('[ '),
	jp_formatThrow(Multiplex, m).
jp_formatThrow(Throw) :-
	not(is_list(Throw)), !,
	format(Throw),
	format(' ').
jp_formatThrow([], m) :-
	format(']'), !.
jp_formatThrow([Throw|Multiplex], m) :-
	jp_formatThrow(Throw),
	jp_formatThrow(Multiplex, m).

	
jp_convertP(p(Self, 0, _), _, Self) :- !.
jp_convertP(p(Throw, Index, _), Cross, ThrowP) :-
	float_to_shortpass(Throw,ShortPass),
	ThrowPList = [ShortPass, r, Index, Cross],
	concat_atom(ThrowPList, ThrowP).
	%%jp_convertPRightLeft(ThrowP, RightLeft, ThrowPrint).
jp_convertP([], [], []) :- !.
jp_convertP([Throw|Multiplex], [C|Cross], [ThrowP|MultiplexP]) :-
	jp_convertP(Throw, C, ThrowP),
	jp_convertP(Multiplex, Cross, MultiplexP).
jp_convertP([], '', []) :- !.
jp_convertP([Throw|Multiplex], '', [ThrowP|MultiplexP]) :-
	jp_convertP(Throw, '', ThrowP),
	jp_convertP(Multiplex, '', MultiplexP).
	

jp_convertPRightLeft(Throw, l, ThrowPrint) :-
	format(atom(ThrowPrint), "( ~w , - )", [Throw]).
jp_convertPRightLeft(Throw, r, ThrowPrint) :-
	format(atom(ThrowPrint), "( - , ~w )", [Throw]).


jp_throwing_hand(ThrowingJuggler, Action, SwapList, HandShown) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(3, Action, ThrowingSiteswapPosition),
	hand(ThrowingSiteswapPosition, Hand),
	handShown(ThrowingJuggler, Hand, SwapList, HandShown).
jp_throwing_hand(_, _, _, _).

jp_cross_tramline(_ThrowingJuggler, _Action, _SwapList, _LeftList, nodelay, '') :- !.   % no delay
jp_cross_tramline(ThrowingJuggler, Action, _SwapList, _LeftList, _Delay, '') :-         % self
	nth1(2, Action, ThrowingJuggler),
	nth1(6, Action, ThrowingJuggler),!.
jp_cross_tramline(ThrowingJuggler, Action, SwapList, LeftList, _Delay, CrossOrTram) :- % pass
	nth1(2, Action, ThrowingJuggler),!,
	nth1(3, Action, ThrowingSiteswapPosition),
	nth1(4, Action, Throw),
	nth1(6, Action, CatchingJuggler),
	nth1(7, Action, CatchingSiteswapPosition),
	hand(ThrowingSiteswapPosition, ThrowingHand),
	hand(CatchingSiteswapPosition, CatchingHand),
	applyNewSwaps(SwapList, LeftList, SwapListLeftList),
	handShown(ThrowingJuggler, ThrowingHand, SwapListLeftList, ThrowingHandShown),
	handShown(CatchingJuggler, CatchingHand, SwapListLeftList, CatchingHandShown),
	jp_cross_or_tramline(ThrowingHandShown, CatchingHandShown, Throw, CrossOrTram).
jp_cross_tramline(_, _, _, _, _, '').

jp_cross_or_tramline([], [], [], []) :- !.
jp_cross_or_tramline([A|HandsA], [B|HandsB], [Throw|Multiplex], [CT|CrossOrTram]) :-
	!,
	jp_cross_or_tramline(A, B, Throw, CT),
	jp_cross_or_tramline(HandsA, HandsB, Multiplex, CrossOrTram).
jp_cross_or_tramline(Hand, Hand, p(Throw, _, _), ' ') :- 
	ThrowInt is float_integer_part(Throw),
	even(ThrowInt), !.
jp_cross_or_tramline(Hand, Hand, _Throw, x) :- !.
jp_cross_or_tramline(_HandA, _HandB, p(Throw, _, _), ' ') :- 
	ThrowInt is float_integer_part(Throw),
	odd(ThrowInt), !.
jp_cross_or_tramline(_HandA, _HandB, _Throw, x) :- !.


jp_filename([], []) :- !.
jp_filename(PatternShort, FileName) :- 
	jp_listOfThrows(PatternShort, Throws),
	concat_atom(Throws, FileName).


jp_listOfThrows([], []) :- !.
jp_listOfThrows([p(Throw, 0, Throw)|Pattern], [Throw|Throws]) :-
	!,
	jp_listOfThrows(Pattern, Throws).
jp_listOfThrows([p(Throw, _Index, _Origen)|Pattern], [ThrowP|Throws]) :-
	atom_concat(Throw, p, ThrowP),
	jp_listOfThrows(Pattern, Throws).
jp_listOfThrows([Multiplex|Pattern], Throws) :-
	is_list(Multiplex),!,
	jp_listOfThrows(Multiplex, MultiplexThrows0),
	append(['['], MultiplexThrows0, MultiplexThrows1),
	append(MultiplexThrows1, [']'], MultiplexThrows),
	jp_listOfThrows(Pattern, RestThrows),
	append(MultiplexThrows, RestThrows, Throws).


jp_jugglerStartWithHand(Juggler, Period, NumberOfJugglers, SwapList, Hand) :-
	siteswap_position(Juggler, 0, SiteswapPosition, NumberOfJugglers, Period),
	hand(SiteswapPosition, HandAB),
	handShown(Juggler, HandAB, SwapList, Hand).

jp_listOfJugglerStartLeft(Period, NumberOfJugglers, SwapList, LeftList) :- 
	JugglerMax is NumberOfJugglers - 1,
	findall(
		Juggler,
		(
			between(0,JugglerMax, Juggler),
			jp_jugglerStartWithHand(Juggler, Period, NumberOfJugglers, SwapList, l)
		), 
		LeftList
	).
	
	
	
	