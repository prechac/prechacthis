:- module(siteswap_main_page,
	[
		find_siteswap_lists/16,
		find_siteswaps/15
	]
).

:- use_module(helpers).
:- use_module(siteswap_helpers).
:- use_module(siteswap_constraints).
:- use_module(siteswap_preprocessing).



find_siteswap_lists(SearchResults, PersonsInt, ObjectsAtom, LengthAtom, MaxInt, PassesMinInt, PassesMaxInt, ContainAtom, DontContainAtom, ClubDoesAtom, ReactAtom, SyncAtom, JustAtom, MagicInt, ResultsInt, Request) :-
	http_main_page_path(MainPagePath),
	memberchk(path(MainPagePath), Request),
	
	(a2Number(PassesMaxInt, -1) -> PassesMaxVar = _Var; PassesMaxVar = PassesMaxInt),
	name(ContainAtom, ContainList),
	name(DontContainAtom, DontContainList),
	name(ClubDoesAtom, ClubDoesList),
	name(ReactAtom, ReactList),
	name(SyncAtom, SyncList),
	name(JustAtom, JustList),
	
	forall(recorded(period_searched, _, R), erase(R)),
	forall(recorded(objects_searched, _, R), erase(R)),
	get_time(Start),
	findall_restricted(
		Siteswaps,
		(
			preprocess_number(LengthAtom, LengthInt, [to_come(_PeriodsToCome), default('1-5')]),
			recordz(period_searched, LengthInt),
			preprocess_number(ObjectsAtom, ObjectsInt, [to_come(_ObjectsToCome), default('>0'), stop_if(test_constraint_not_fillable)]),
			recordz(objects_searched, ObjectsInt),
			siteswap_main_page:find_siteswaps(Siteswaps, PersonsInt, ObjectsInt, LengthInt, MaxInt, PassesMinInt, PassesMaxVar, ContainList, DontContainList, ClubDoesList, ReactList, SyncList, JustList, MagicInt, ResultsInt)
		),
		SiteswapLists,
		[time(15), flag(Flag)]
	),
	get_time(End),
	Time is End - Start,
	findall_restricted(Period, (recorded(period_searched, Period, R), erase(R)), PeriodsSearched, [unique]),
	findall_restricted(Objects, (recorded(objects_searched, Objects, R), erase(R)), ObjectsSearched, [unique]),
	SearchResults = [
		flag(Flag),
		time(Time),
		period_searched(PeriodsSearched),
		objects_searched(ObjectsSearched),
		lists(SiteswapLists)
	], !.
find_siteswap_lists(false, _PersonsInt, _ObjectsAtom, _LengthAtom, _MaxInt, _PassesMinInt, _PassesMaxInt, _ContainAtom, _DontContainAtom, _ClubDoesAtom, _ReactAtom, _SyncAtom, _JustAtom, _MagicInt, _ResultsInt, _Request) :- !.


find_siteswaps(SiteswapList, Persons, Objects, Length, Max, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Sync, Just, Magic, Results) :-
	get_time(Start),
	findall_restricted(
		Throws, 
		siteswap(Throws, Persons, Objects, Length, Max, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Sync, Just, Magic),
		Bag,
		[unique, results(Results), flag(Flag)]
	),
	length(Bag, NumberOfSiteswaps),
	NumberOfSiteswaps > 0, !,
	sortListOfSiteswaps(Bag, Siteswaps),
	get_time(End),
	Time is End - Start,
	SiteswapList = [
		flag(Flag),
		time(Time),
		objects(Objects), 
		length(Length),
		siteswaps(Siteswaps)
	].

