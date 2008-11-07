swap_page(Request) :-
	http_parameters(
		Request,
		[ 
			pattern( ReqPattern,  [optional(false)]         ),
			pos1( ReqPos1,        [optional(false), integer]),
			pos2( ReqPos2,        [optional(false), integer])
		]
	),

	www_form_encode(PatternAtom, ReqPattern),
	atom2Pattern(PatternAtom, Pattern),

	swapPage_file_header,
	swapPage_swaped_pattern(Pattern, ReqPos1, ReqPos2).

swapPage_file_header :-
	format('Content-type: text/plain~n~n').
	
swapPage_swaped_pattern(Pattern, Pos1, Pos2) :-
	swapThrows(Pattern, Pos1, Pos2, NewPattern),!,
	float_to_shortpass(NewPattern, PatternShort),
	www_form_encode_all(PatternShort, PatternEnc),
	format(PatternEnc).
swapPage_swaped_pattern(_,_,_) :-
	format("-2").
