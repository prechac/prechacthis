:- [siteswap].
	
hardcodeSiteswaps(ObjectMax,PeriodMax,MaxHight) :-
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
			format(Stream, '%% Siteswaps with ~w objects, length ~w and max. hight ~w\n', [Objects, PeriodMax, MaxHight]),
			close(Stream),
			writeSiteswaps(Objects,Period,MaxHight,FileName)
		)
	),
	working_directory(_,Old).


writeSiteswaps(Objects,Period,MaxHight,FileName) :-
	open(FileName,append,Stream),
	format('objects: ~w, period: ~w\n', [Objects,Period]), 
	length(Siteswap,Period),
	forall(siteswap(Objects,MaxHight,Siteswap), formatSwap(Stream,Objects,Siteswap)),
	close(Stream).


formatSwap(Stream,Objects,Swap) :-
	format(Stream,'siteswap_hc(~w, ~w).\n', [Objects,Swap]).


siteswap(Objects,MaxHight, Siteswap) :-
	siteswap(Objects,Siteswap),
	allHightsSmaller(Siteswap,MaxHight).
