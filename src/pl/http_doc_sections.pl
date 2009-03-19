%%% ---------- Documentation - Sections ----------- %%%

doc_section(main, []).

doc_section(simple, [ 
    name('simple mode'),
    url('./list?persons=2&objects=4&period=4&max=4&passesmin=1&passesmax=-1&contain=1+1p&mode=simple'),
    content(
        'All patterns with 4 objects and the length of 4 containing the sequence "1 1p"'
    )
]).

doc_section(advanced, [ 
    name('advanced mode'),
    url('./list?persons=2&objects=4+-+6&period=<+7&max=4&passesmin=2&passesmax=4&contain=1+(1p+or+1.5p)+and+(4+or+4p+or+4.5p)&exclude=0&react=4+(1p+or+1.5p)&results=42&mode=advanced'),
    content(
        'some text is going to come soonish ;-)'
    )
]).

doc_section(noobjects, [ 
    name('number of objects not given'),
    url('./list?persons=2&objects=&period=4&max=4&passesmin=1&passesmax=-1&contain=1+1p&results=42&mode=advanced'),
    content(
        'some text is going to come soonish ;-)'
    )
]).

doc_section(wildcards, [ 
    name('wildcards'),
    url('./list?persons=3&objects=9&period=4&max=4&passesmin=2&passesmax=4&contain=_p+_+3+and+_s+_p1&results=42&mode=advanced'),
    content(
        'some text is going to come soonish ;-)'
    )
]).

doc_section(multiplexes, [ 
    name('multiplexes'),
    url('./list?persons=2&objects=4&period=4&max=4&passesmin=1&passesmax=-1&contain=[3+2p]&results=42&mode=advanced'),
    content(
        'some text is going to come soonish ;-)'
    )
]).

doc_section(multiplexes2, [ 
    name('multiplexes with wildcards'),
    url('./list?persons=2&objects=4&period=4&max=4&passesmin=1&passesmax=-1&contain=2+_+[_+_p]&results=42&mode=advanced'),
    content(
        'some text is going to come soonish ;-)'
    )
]).

doc_section(multiplexes3, [
    name('3 person multiplexes'),
    url('./list?persons=3&objects=10&period=6&max=4&passesmin=2&passesmax=3&contain=2+3+[_p2+_p1]+and+3p&results=42&mode=advanced'),
    content(
        'some text is going to come soonish ;-)'
    )
]).

doc_section(multiplexes4, [
    name('multiplexes length 3'),
    url('./list?persons=3&objects=10&period=6&max=3&passesmin=2&passesmax=3&contain=2+_+[2+2]+_+[_s+_p2+_p1]&exclude=0&results=42&mode=advanced'),
    content(
        'some text is going to come soonish ;-)'
    )
]).
