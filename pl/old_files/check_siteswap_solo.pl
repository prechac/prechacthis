siteswap(Throws) :-
   siteswap(Throws, _).

siteswap(Throws, Objects) :-
   allThrows(Throws),
   mean(Throws, Objects),
   integer(Objects),
   uniqueLandingSites(Throws).
%    isSorted(Throws).

siteswap(Objects, Max, Throws) :-
   allThrows(Throws, Max),
   mean(Throws, Objects),
   integer(Objects),
   uniqueLandingSites(Throws).


allThrows([], _).
allThrows([Head|Tail], Max):-
   between(0, Max, Head),
   allThrows(Tail).

allThrows([]).
allThrows([Head|Tail]):-
   throwable(Head),
   allThrows(Tail).


throwable(0).
throwable(1).
throwable(2).
throwable(3).
throwable(4).
throwable(5).
throwable(6).
throwable(7).
throwable(8).
throwable(9).
throwable(10).

mean(Data, Mean) :-
   sumlist(Data,Sum),
   length(Data,Length),
   Mean is Sum/Length.


uniqueLandingSites(Throws):-
   landingSites(Throws, LandingSites),
   allMembersUnique(LandingSites).

landingSites(Throws, LandingSites) :-
   length(Throws,  Periodlength),

   findLandingSites(Throws,          LandingSitesBig, Periodlength),
    modLandingSites(LandingSitesBig, LandingSites,    Periodlength).

findLandingSites([], [], _PeriodLength).
findLandingSites([Throw      | RestThrows],
                 [LandingSite| RestSites], PeriodLength) :-
   length(RestThrows, RestLength),
   OriginalIndex is PeriodLength - RestLength + 2,
   LandingSite is Throw + OriginalIndex,
   findLandingSites(RestThrows, RestSites, PeriodLength).

modLandingSites([], [], _PeriodLength).
modLandingSites([   FirstSite|    RestSites],
                [ModFirstSite| ModRestSites],    PeriodLength) :-
   ModFirstSite is FirstSite mod PeriodLength,
   modLandingSites(RestSites, ModRestSites, PeriodLength).


allMembersUnique([]).
allMembersUnique([Head | Tail]) :-
   forall(member(X, Tail), Head\=X),
   allMembersUnique(Tail).


isSorted([_]).
isSorted([First, Second | Tail]) :-
   First >= Second,
   isSorted([Second | Tail]).
