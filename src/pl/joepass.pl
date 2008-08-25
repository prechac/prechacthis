jp_pattern_def(PatternShort, NumberOfJugglers, SwapList) :-
	length(PatternShort, Period),
	maxHeight(PatternShort, ShortMaxHeight),
	MaxHeight is truncate(ShortMaxHeight) + 1,
	convertShortPasses(PatternShort, Period, NumberOfJugglers, MaxHeight, Pattern),
	all_points_in_time(PointsInTime, NumberOfJugglers, Period),
	what_happens(PointsInTime, Pattern, NumberOfJugglers, ActionList),
	jp_header,
	jp_positions,
	jp_colors,
	%%jp_jugglerStartLeft(SwapList),
	jp_delay(NumberOfJugglers, Period, Delay),
	jp_pattern(ActionList, SwapList, PointsInTime, Delay, NumberOfJugglers).

jp_header :- 
	format("#sx\n").
	%%format("#mhn*\n").

jp_positions :- !.

jp_colors :- !.

jp_jugglerStartLeft([]) :- !.
jp_jugglerStartLeft([Juggler|SwapList]) :-
	JugglerShown is Juggler + 1,
	format("#jugglerStartLeft ~w\n", [JugglerShown]),
	jp_jugglerStartLeft(SwapList).

jp_delay(NumberOfJugglers, Period, 0) :-
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

jp_pattern(ActionList, SwapList, PointsInTime, Delay, NumberOfJugglers) :-
	JugglerMax is NumberOfJugglers - 1,
	format("< "),
	forall(
		between(0, JugglerMax, Juggler), 
		jp_print_jugglers_throws(Juggler, ActionList, SwapList, PointsInTime, Delay)
	),
	format(">").	

	
jp_print_jugglers_throws(Juggler, ActionList, SwapList, PointsInTime, Delay) :-
	(Juggler = 0 -> true; format("| ")),
	forall(member(Point, PointsInTime), jp_print_jugglers_point_in_time(Juggler, Point, SwapList, ActionList, Delay)).


jp_print_jugglers_point_in_time(Juggler, PointInTime, SwapList, ActionList, Delay) :-
	member(Action, ActionList),
	nth1(2, Action, Juggler),
	nth1(1, Action, PointInTime),!,
	nth1(4, Action, Throw),
	%%jp_throwing_hand(Juggler, Action, SwapList, RightOrLeft),
	jp_cross_tramline(Juggler, Action, SwapList, Delay, CrossOrTram),
	jp_convertP(Throw, CrossOrTram, ThrowP),
	%%convertMultiplex_singleThrow(ThrowsP,ThrowsPM),
	format(ThrowP),
	format(" ").
jp_print_jugglers_point_in_time(_, _, _, _, _) :- !.
	

jp_convertP(p(Throw, Index, _), Cross, ThrowP) :-
	float_to_shortpass(Throw,ShortPass),
	ThrowPList = [ShortPass, r, Index, Cross],
	concat_atom(ThrowPList, ThrowP).
	%%jp_convertPRightLeft(ThrowP, RightLeft, ThrowPrint).

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

jp_cross_tramline(_ThrowingJuggler, _Action, _SwapList, 0, '') :- !.
jp_cross_tramline(ThrowingJuggler, Action, _SwapList, _Delay, '') :-
	nth1(2, Action, ThrowingJuggler),
	nth1(6, Action, ThrowingJuggler),!.
jp_cross_tramline(ThrowingJuggler, Action, SwapList, _Delay, CrossOrTram) :-
	nth1(2, Action, ThrowingJuggler),!,
	nth1(3, Action, ThrowingSiteswapPosition),
	nth1(6, Action, CatchingJuggler),
	nth1(7, Action, CatchingSiteswapPosition),
	hand(ThrowingSiteswapPosition, ThrowingHand),
	hand(CatchingSiteswapPosition, CatchingHand),
	handShown(ThrowingJuggler, ThrowingHand, SwapList, ThrowingHandShown),
	handShown(CatchingJuggler, CatchingHand, SwapList, CatchingHandShown),
	jp_cross_or_tramline(ThrowingHandShown, CatchingHandShown, CrossOrTram).
jp_cross_tramline(_, _, _, _, '').

jp_cross_or_tramline(Hand, Hand, x) :- !.
jp_cross_or_tramline(_HandA, _HandB, '') :- !.


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


