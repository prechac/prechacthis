:- module(http_swap_page, 
	[
		swap_page/1
	]
).

:- use_module(http_common).

:- use_module(library('http/http_parameters')).

:- use_module(helpers).
:- use_module(siteswap_helpers).

:- use_module(http_helpers).

swap_page(Request) :-
	http_parameters(
		Request,
		[ 
			pattern( ReqPattern,  [optional(false)]         ),
			pos1( ReqPos1,        [optional(false), integer]),
			pos2( ReqPos2,        [optional(false), integer])
		]
	),
	
	%www_form_encode(ReqPattern, PatternAtom),
	atom2Pattern(ReqPattern, Pattern),

	swapPage_file_header,
	%format(ReqPattern),
	swapPage_swaped_pattern(Pattern, ReqPos1, ReqPos2).

swapPage_file_header :-
	format('Content-type: text/plain~n~n').
	
swapPage_swaped_pattern(Pattern, Pos1, Pos2) :-
	swapThrows(Pattern, Pos1, Pos2, NewPattern),!,
	atom2Pattern(PatternAtom, NewPattern),
	www_form_encode(PatternAtom, PatternEnc),
	format(PatternEnc).
swapPage_swaped_pattern(_,_,_) :-
	format("-2").
