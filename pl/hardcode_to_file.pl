:- [siteswap].
	
hardcodeSiteswaps(ObjectMax,PeriodMax,MaxHeight) :-
	(exists_directory('siteswap_hc'); make_directory('siteswap_hc')),
	working_directory(Old,'siteswap_hc'),
	forall(
		(
			between(1,ObjectMax,Objects), 
			between(2,PeriodMax,Period)
		),
		(
			sformat(FileName, 'siteswap_hc_~w-~w.pl', [Objects,Period]),
			open(FileName,write,Stream),
			format(Stream, '%% Siteswaps with ~w objects, length ~w and max. height ~w\n', [Objects, PeriodMax, MaxHeight]),
			close(Stream),
			writeSiteswaps(Objects,Period,MaxHeight,FileName)
		)
	),
	working_directory(_,Old).


writeSiteswaps(Objects,Period,MaxHeight,FileName) :-
	open(FileName,append,Stream),
	format('objects: ~w, period: ~w\n', [Objects,Period]), 
	length(Siteswap,Period),
	forall(siteswap(Objects,MaxHeight,Siteswap), formatSwap(Stream,Objects,Siteswap)),
	close(Stream).


formatSwap(Stream,Objects,Swap) :-
	format(Stream,'siteswap_hc(~w, ~w).\n', [Objects,Swap]).


siteswap(Objects,MaxHeight, Siteswap) :-
	siteswap(Objects,Siteswap),
	allHeightsSmaller(Siteswap,MaxHeight).
