:- module(http_joepass, 
    [
        joepass_page/1,
        jp_filename/2,
        jp_Color/1,
        jp_Color/2,
        jp_Color_Info/2
    ]
).

:- use_module(library('http/http_parameters')).

:- use_module(helpers).
:- use_module(siteswap_helpers).
:- use_module(siteswap_multiplex).
:- use_module(siteswap_info_page).
:- use_module(http_info_page).

:- use_module(http_helpers).
:- use_module(http_common).
:- use_module(http_server).
:- use_module(siteswap_preprocessing).

joepass_page(Request) :-
    %http_joepass_page_path(JoePassPagePath),
    http_parameters(
        Request,
        [ 
            pattern( ReqPattern,  [optional(false)]         ),
            persons( ReqPersons,  [optional(false), integer]),
            filename(ReqFilename, [default('joe')]          ),
            swap(    ReqSwap,     [default('[]')]           ),
            download(ReqDownload, [default('on')]           ),
            style(   ReqStyle,    [default('normal')]       ),
            nametype(ReqNameType, [default('joe')]          ),
            distance(ReqDistance, [default(1), integer]     ),
            allways( ReqAllways,  [default('off')]          ),
            color0(Color0,        [default(-1), integer]    ),
            color1(Color1,        [default(-1), integer]    ),
            color2(Color2,        [default(-1), integer]    ),
            color3(Color3,        [default(-1), integer]    ),
            color4(Color4,        [default(-1), integer]    ),
            color5(Color5,        [default(-1), integer]    ),
            color6(Color6,        [default(-1), integer]    ),
            color7(Color7,        [default(-1), integer]    ),
            color8(Color8,        [default(-1), integer]    ),
            color9(Color9,        [default(-1), integer]    ),
            color10(Color10,      [default(-1), integer]    ),
            color11(Color11,      [default(-1), integer]    ),
            color12(Color12,      [default(-1), integer]    ),
            color13(Color13,      [default(-1), integer]    ),
            color14(Color14,      [default(-1), integer]    ),
            color15(Color15,      [default(-1), integer]    ),
            color16(Color16,      [default(-1), integer]    ),
            color17(Color17,      [default(-1), integer]    ),
            color18(Color18,      [default(-1), integer]    ),
            color19(Color19,      [default(-1), integer]    ),
            color20(Color20,      [default(-1), integer]    ),
            color21(Color21,      [default(-1), integer]    ),
            color22(Color22,      [default(-1), integer]    ),
            color23(Color23,      [default(-1), integer]    )
        ]
    ),
    
    OrbitColors = [
        [0, Color0],
        [1, Color1],
        [2, Color2],
        [3, Color3],
        [4, Color4],
        [5, Color5],
        [6, Color6],
        [7, Color7],
        [8, Color8],
        [9, Color9],
        [10, Color10],
        [11, Color11],
        [12, Color12],
        [13, Color13],
        [14, Color14],
        [15, Color15],
        [16, Color16],
        [17, Color17],
        [18, Color18],
        [19, Color19],
        [20, Color20],
        [21, Color21],
        [22, Color22],
        [23, Color23]
    ],
    
    jp_clean_orbit_colors(OrbitColors, OrbitColorsCleaned),
%   OrbitColorsCleanedz = [[0,0], [1,1]],
    
    www_form_encode(PatternAtom, ReqPattern),
    Persons = ReqPersons,
    www_form_encode(FilenameAtom, ReqFilename),
    www_form_encode(SwapListAtom, ReqSwap),
    www_form_encode(DownloadAtom, ReqDownload),
    www_form_encode(StyleAtom, ReqStyle),
    www_form_encode(NameTypeAtom, ReqNameType),
    Distance = ReqDistance,
    
    atom2Pattern(PatternAtom, Pattern),
    atom2SwapList(SwapListAtom, SwapList),
    
    set_cookie('joepass_download', DownloadAtom),
    set_cookie('joepass_style', StyleAtom),
    set_cookie('joepass_file', NameTypeAtom),
    set_cookie('joepass_allways_download', ReqAllways),
    
    jp_file_header(DownloadAtom, NameTypeAtom, FilenameAtom),
    jp_pattern_def(Pattern, Persons, SwapList, StyleAtom, OrbitColorsCleaned, Distance).


    
jp_file_header(on, numbers, Filename) :-
    format('Content-type: application/force-download~n'),
    format('Content-Transfer-Encoding: Binary~n'),
    format('Content-disposition: attachment; filename="~w.pass"~n~n', [Filename]).
jp_file_header(on, joe, _Filename) :-
    format('Content-type: application/force-download~n'),
    format('Content-Transfer-Encoding: Binary~n'),
    format('Content-disposition: attachment; filename="joe.pass"~n~n').
jp_file_header(off, _, _) :-
    format('Content-type: text/plain~n~n').
    

jp_pattern_def(PatternShort, NumberOfJugglers, SwapList, Style, OrbitColors, Distance) :-
    length(PatternShort, Period),
    maxHeight(PatternShort, ShortMaxHeight),
    MaxHeight is truncate(ShortMaxHeight) + 1,
    convertShortPasses(PatternShort, Period, NumberOfJugglers, MaxHeight, Pattern),
    all_points_in_time(PointsInTime, NumberOfJugglers, Period),
    what_happens(PointsInTime, Pattern, NumberOfJugglers, ActionList),
    jp_listOfJugglerStartLeft(Period, NumberOfJugglers, SwapList, LeftList),
    jp_header(PatternShort, NumberOfJugglers),
    jp_positions(NumberOfJugglers, Style, Distance),
    %jp_colors(ActionList, Pattern, NumberOfJugglers),
    (noMultiplex(Pattern) -> 
        jp_colors(ActionList, Pattern, NumberOfJugglers, OrbitColors);
        true
    ),
    jp_jugglerStartLeft(LeftList),
    jp_delay(NumberOfJugglers, Period, Delay),
    jp_pattern(ActionList, SwapList, LeftList, PointsInTime, Delay, NumberOfJugglers),
    format('\n\n\n'),
    format('!-------------------------------------------\n'),
    format('! generated by PrechacThis (prechacthis.org)\n'),
    format('!-------------------------------------------\n').

jp_header(PatternShort, NumberOfJugglers) :- 
    ((NumberOfJugglers > 2) ->
        jp_listOfThrows(PatternShort, Throws, index);
        jp_listOfThrows(PatternShort, Throws, noindex)
    ),
    concat_atom(Throws, ' ', PatternString),
    format('!###########################################\n'),
    format('! ~w  (~w Jugglers)\n', [PatternString, NumberOfJugglers]),
    format('!###########################################\n'),
    format('#sx\n\n').

jp_positions(NumberOfJugglers, Style, Distance) :-
    NumberOfJugglers > 1,
    format('!+++++++++++++++++++++++++++++++++++++++++++\n'),
    format('! juggler positions\n'),
    format('!+++++++++++++++++++++++++++++++++++++++++++\n'),
    forall(between(1, NumberOfJugglers, Juggler), jp_printJugglerPosition(Juggler, NumberOfJugglers, Style, Distance)), !,
    format('\n'), !.
jp_positions(_NumberOfJugglers, _Niente, _Distance) :- !.

jp_printJugglerPosition(Juggler, NumberOfJugglers, normal, Distance) :-
    PositionInCircle is ((Juggler - 1) * Distance) mod NumberOfJugglers,
    X is 2 * pi * PositionInCircle/NumberOfJugglers - (NumberOfJugglers mod 2) * pi/2,
    Radius1 is 130 / (2 * sin(pi/NumberOfJugglers)),
    Radius2 is 200,
    Radius is max(Radius1, Radius2),
    U is round(Radius * cos(X)),
    V is round(Radius * sin(X)),
    format('#jugglerPosition ~w (~w,0,~w)(0,0,0)\n', [Juggler, U, V, PositionInCircle]).

jp_printJugglerPosition(Juggler, NumberOfJugglers, shortdistance, Distance) :-
    PositionInCircle is ((Juggler - 1) * Distance) mod NumberOfJugglers,
    X is 2 * pi * PositionInCircle/NumberOfJugglers - (NumberOfJugglers mod 2) * pi/2,
    Radius is 150 / (2 * sin(pi/NumberOfJugglers)),
    U is round(Radius * cos(X)),
    V is round(Radius * sin(X)),
    format('#jugglerPosition ~w (~w,0,~w)(0,0,0)\n', [Juggler,U,V]).

jp_printJugglerPosition(Juggler, NumberOfJugglers, sidebyside, Distance) :-
    PositionInCircle is ((Juggler - 1) * Distance) mod NumberOfJugglers,
    X is round(110 * ( PositionInCircle - (NumberOfJugglers-1)/2 )),
    format('#jugglerPosition ~w (~w,0,0)(~w,0,1)\n', [Juggler, X, X]).

jp_printJugglerPosition(Juggler, NumberOfJugglers, backtoback, Distance) :-
    PositionInCircle is ((Juggler - 1) * Distance) mod NumberOfJugglers,
    X is 2 * pi * PositionInCircle/NumberOfJugglers - (NumberOfJugglers mod 2) * pi/2,
    Radius is 50 / (2 * sin(pi/NumberOfJugglers)),
    U is round(Radius * cos(X)),
    V is round(Radius * sin(X)),
    RU is 2*U,
    RV is 2*V,
    format('#jugglerPosition ~w (~w,0,~w)(~w,0,~w)\n', [Juggler,U,V,RU,RV]).


jp_printJugglerPosition(Juggler, NumberOfJugglers, dropbackline, _Distance) :-
    Juggler is 1,
    PositionInCircle is 0,
    X is round(150 * ( PositionInCircle - (NumberOfJugglers-1)/2 ) - 50),
    RX is X+1,
    format('#jugglerPosition ~w (~w,0,0)(~w,0,0)\n', [Juggler, X, RX]).
jp_printJugglerPosition(Juggler, NumberOfJugglers, dropbackline, Distance) :-
    PositionInCircle is ((Juggler - 1) * Distance) mod NumberOfJugglers,
    X is round(150 * ( PositionInCircle - (NumberOfJugglers-1)/2 ) + 50),
    RX is X-1,
    format('#jugglerPosition ~w (~w,0,0)(~w,0,0)\n', [Juggler, X, RX]).


jp_printJugglerPosition(Juggler, NumberOfJugglers, 'wye', Distance) :-
    NumberOfJugglers is 4,
    PositionInCircle is ((Juggler - 1) * Distance) mod NumberOfJugglers,
    Positions = ['(0,0,0)(1,0,0)', '(250,0,-150)(-250,0,0)', '(-250,0,0)(0,0,0)', '(250,0,150)(-250,0,0)'],
    nth0(PositionInCircle, Positions, P),
    format('#jugglerPosition ~w ~w\n', [Juggler,P]).




jp_colors(ActionList, Pattern, NumberOfJugglers, OrbitColors) :-
    format('!+++++++++++++++++++++++++++++++++++++++++++\n'),
    format('! colors\n'),
    format('!+++++++++++++++++++++++++++++++++++++++++++\n\n'), 
    orbits(Pattern, OrbitPattern),
    averageNumberOfClubs(Pattern, AVClubs),
    NumberOfClubs is AVClubs * NumberOfJugglers,
    jp_printOrbitDefs(OrbitColors),
    length(Pattern, Period),
    club_siteswap_positions(ActionList, OrbitPattern, NumberOfClubs, Period, SiteswapPositions),
    format('!-------------------------------------------\n'),
    format('! object colors, colors match orbits\n'),
    format('!-------------------------------------------\n'),
    jp_printClubColor(SiteswapPositions, NumberOfClubs),
    format('\n').

    
jp_clean_orbit_colors([], []) :- !.
jp_clean_orbit_colors([[_, -1]|OrbitColors], OrbitColorsCleaned) :-
    !, jp_clean_orbit_colors(OrbitColors, OrbitColorsCleaned).
jp_clean_orbit_colors([Color| OrbitColors], [Color| OrbitColorsCleaned]) :-
    jp_clean_orbit_colors(OrbitColors, OrbitColorsCleaned).

jp_printOrbitDefs(OrbitColors) :-
    format('!-------------------------------------------\n'),
    format('! color definition for orbits\n'),
    format('!-------------------------------------------\n'),
    forall(member([Orbit, Number], OrbitColors),
        (
            Orbit1 is Orbit + 1,
            jp_Color([number(Number), code(Color), name(Name)]),
            format('#replace orbit~w ~w ! ~w\n', [Orbit1, Color, Name])
        )
    ),
    format('\n').




jp_printClubColor([], _) :- !.
jp_printClubColor([[_Juggler, _SiteswapPosition, Orbit]|ClubSiteswapPositions], NumberOfClubs) :-
    length(ClubSiteswapPositions, NumberOfRestClubs),
    Club is NumberOfClubs - NumberOfRestClubs,
    Orbit1 is Orbit + 1,
    format('#objectColor ~w orbit~w\n', [Club, Orbit1]),
    jp_printClubColor(ClubSiteswapPositions, NumberOfClubs).
    
    
jp_Color(Info) :-
    select(number(Number), Info, InfoShort),!,
    jp_Color(Number, InfoShort).
jp_Color(Info) :-
    jp_Color(_, Info).
    
jp_Color(Number, Info) :-
    not(is_list(Info)), !,
    jp_Color(Number, [Info]).
jp_Color(Number, InfoList) :-
    is_list(InfoList),
    jp_Color_Info(Number, InfoList).
    
jp_Color_Info(_, []) :- !.
jp_Color_Info(Number, [code(Code)|InfoList]) :- 
    jp_ListOfColorCodes(ColorList),
    (
        (
            var(Number),
            Pos = Number
        );
        (
            number(Number),
            length(ColorList, NumberOfColors),
            Pos is Number mod NumberOfColors
        )
    ),
    nth0(Pos, ColorList, Code),
    jp_Color_Info(Number, InfoList).
jp_Color_Info(Number, [name(Name)|InfoList]) :-
    jp_ListOfColorNames(NameList),
    (
        (
            var(Number),
            Pos = Number
        );
        (
            number(Number),
            length(NameList, NumberOfNames),
            Pos is Number mod NumberOfNames
        )
    ),
    nth0(Pos, NameList, Name),
    jp_Color_Info(Number, InfoList).

jp_ListOfColorCodes(['(1,1,1)', '(1,0,0)', '(0,1,0)', '(0,0,1)', '(0,1,1)',   '(1,0,1)', '(1,1,0)', '(0.5,0,0)', '(0,0.5,0)', '(0,0,0.5)', '(0,0.5,0.5)', '(0.5,0,0.5)', '(0.5,0.5,0)', '(0.5,0.5,0.5)', '(0,0,0)']).
jp_ListOfColorNames(['white',   'red',     'green',   'blue',    'turquoise', 'magenta', 'lemon',   'cayenne',   'clover',    'midnight',  'teal',        'plum',        'asparagus',   'nickel',        'black'  ]).



jp_jugglerStartLeft([]) :- !.
jp_jugglerStartLeft(LeftList) :-
    format('!+++++++++++++++++++++++++++++++++++++++++++\n'),
    format('! jugglers starting with left hand\n'),
    format('!+++++++++++++++++++++++++++++++++++++++++++\n'),
    jp_jugglerStartLeft(LeftList, go),
    format('\n').
jp_jugglerStartLeft([], go) :- !.
jp_jugglerStartLeft([Juggler|LeftList], go) :-
    JugglerShown is Juggler + 1,
    format('#jugglerStartLeft ~w\n', [JugglerShown]),
    jp_jugglerStartLeft(LeftList, go).

jp_delay(NumberOfJugglers, Period, nodelay) :-
    Prechator is (Period rdiv NumberOfJugglers),
    0 is float_fractional_part(Prechator), !.
jp_delay(NumberOfJugglers, Period, delay) :-
    format('!+++++++++++++++++++++++++++++++++++++++++++\n'),
    format('! juggler delays\n'),
    format('!+++++++++++++++++++++++++++++++++++++++++++\n'),
    format('#D -\n'),
    JugglerMax is NumberOfJugglers - 1,
    forall(
        between(0, JugglerMax, Juggler),
        (   
            RealDelay is (Period rdiv NumberOfJugglers) * Juggler,
            Delay is float_fractional_part(RealDelay),
            DelayF is float(Delay),
            JugglerShown is Juggler + 1,
            (Delay = 0 -> true; format('#jugglerDelay ~w ~w\n', [JugglerShown, DelayF]))
        )
    ),
    format('\n').


jp_pattern(ActionList, SwapList, LeftList, PointsInTime, Delay, NumberOfJugglers) :-
    format('!+++++++++++++++++++++++++++++++++++++++++++\n'),
    format('! pattern\n'),
    format('!+++++++++++++++++++++++++++++++++++++++++++\n'),
    JugglerMax is NumberOfJugglers - 1,
    format('\n< '),
    forall(
        between(0, JugglerMax, Juggler), 
        jp_print_jugglers_throws(Juggler, ActionList, SwapList, LeftList, PointsInTime, Delay)
    ),
    format('>').    

    
jp_print_jugglers_throws(Juggler, ActionList, SwapList, LeftList, PointsInTime, Delay) :-
    (Juggler = 0 -> true; format('|\n  ')),
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
    format(' ').
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
    format(atom(ThrowPrint), '( ~w , - )', [Throw]).
jp_convertPRightLeft(Throw, r, ThrowPrint) :-
    format(atom(ThrowPrint), '( - , ~w )', [Throw]).


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

jp_cross_or_tramline(_HandA, [], [], []) :- !.
jp_cross_or_tramline(HandA, [B|HandsB], [Throw|Multiplex], [CT|CrossOrTram]) :-
    !,
    jp_cross_or_tramline(HandA, B, Throw, CT),
    jp_cross_or_tramline(HandA, HandsB, Multiplex, CrossOrTram).
jp_cross_or_tramline(Hand, Hand, p(Throw, _, _), ' ') :- 
    ThrowInt is float_integer_part(Throw),
    even(ThrowInt), !.
jp_cross_or_tramline(Hand, Hand, Throw, x) :- 
    not(is_list(Throw)), !.
jp_cross_or_tramline(_HandA, _HandB, p(Throw, _, _), ' ') :- 
    ThrowInt is float_integer_part(Throw),
    odd(ThrowInt), !.
jp_cross_or_tramline(_HandA, _HandB, Throw, x) :- 
    not(is_list(Throw)), !.


jp_filename([], []) :- !.
jp_filename(PatternShort, FileName) :- 
    jp_listOfThrows(PatternShort, Throws),
    concat_atom(Throws, FileName).

jp_listOfThrows(Pattern, Throws) :-
    jp_listOfThrows(Pattern, Throws, noindex).
    
jp_listOfThrows([], [], _) :- !.
jp_listOfThrows([p(Throw, 0, Throw)|Pattern], [Throw|Throws], Style) :-
    !,
    jp_listOfThrows(Pattern, Throws, Style).
jp_listOfThrows([p(Throw, Index, _Origen)|Pattern], [ThrowP|Throws], Style) :-
    (Style = index ->
        ThrowList = [Throw, p, Index];
        ThrowList = [Throw, p]
    ),
    concat_atom(ThrowList, ThrowP),
    jp_listOfThrows(Pattern, Throws, Style).
jp_listOfThrows([Multiplex|Pattern], Throws, Style) :-
    is_list(Multiplex),!,
    jp_listOfThrows(Multiplex, MultiplexThrows0, Style),
    append(['['], MultiplexThrows0, MultiplexThrows1),
    append(MultiplexThrows1, [']'], MultiplexThrows),
    jp_listOfThrows(Pattern, RestThrows, Style),
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
            jp_jugglerStartWithHand(Juggler, Period, NumberOfJugglers, SwapList, 'L')
        ), 
        LeftList
    ).
    
    
    
    
