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
	jp_jugglerStartLeft(SwapList),
	jp_delay,
	jp_pattern(ActionList, PointsInTime, NumberOfJugglers).

jp_header :- 
	format("#sx\n\n").

jp_positions :- !.

jp_colors :- !.

jp_jugglerStartLeft([]) :- !.
jp_jugglerStartLeft([Juggler|SwapList]) :-
	JugglerShown is Juggler + 1,
	format("#jugglerStartLeft ~w\n", [JugglerShown]),
	jp_jugglerStartLeft(SwapList).

jp_delay :- !.

jp_pattern(ActionList, PointsInTime, NumberOfJugglers) :-
	JugglerMax is NumberOfJugglers - 1,
	format("\n\n< "),
	forall(
		between(0, JugglerMax, Juggler), 
		jp_print_jugglers_throws(Juggler, ActionList, PointsInTime)
	),
	format(">\n").	

	
jp_print_jugglers_throws(Juggler, ActionList, PointsInTime) :-
	(Juggler = 0 -> true; format("| ")),
	forall(member(Point, PointsInTime), jp_print_jugglers_point_in_time(Juggler, Point, ActionList)).


jp_print_jugglers_point_in_time(Juggler, PointInTime, ActionList) :-
	member(Action, ActionList),
	nth1(2, Action, Juggler),
	nth1(1, Action, PointInTime),!,
	nth1(4, Action, Throw),
	jp_convertP(Throw, ThrowP),
	%%convertMultiplex_singleThrow(ThrowsP,ThrowsPM),
	format(ThrowP),
	format(" ").
jp_print_jugglers_point_in_time(_, _, _) :- !.
	

jp_convertP(p(Throw, Index, _), ThrowP) :-
	float_to_shortpass(Throw,ShortPass),
	ThrowPList = [ShortPass, r, Index],
	concat_atom(ThrowPList, ThrowP).
	
	
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



