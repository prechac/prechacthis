:- module(http_doc_page,
	[
		doc_page/5,
        remove_doc_in_URL/2
	]
).


:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_header')).

:- use_module(helpers).

:- use_module(http_helpers).
:- use_module(http_server).
:- use_module(http_common).
:- use_module(http_info_page).
:- use_module(http_main_page).


doc_page(false, _Request, 'mainPage') --> [], !.
doc_page(DocPage, Request, 'mainPage_doc') -->
    {
        doc_section(DocPage, _)
    },
    html([
        div([id(docPage)],[
            div([id(doc_container)],[
                \doc_main(DocPage, Request)
            ])
        ])
    ]).
doc_page(_DocPage, _Request, 'mainPage') --> [], !.

doc_main('main', Request) -->
    {
        remove_doc_in_URL(Request, Request_clean),
        parse_url(URL, Request_clean)
    },
    html([
        div([class(back)],[
            a([href(URL)],[
                'close'
            ])
        ]),
        h1([],[
            'Examples'
        ]),
        div([id(doc_main)],[
            ul([],[
                \doc_index
            ])
        ])
    ]).
doc_main(DocPage, Request) --> 
    {
        doc_section(DocPage, Section),
        memberchk(name(Name), Section),
        memberchk(content(Content), Section),
        remove_doc_in_URL(Request, Request_clean),
        parse_url(URL, Request_clean)
    },
    html([
        div([class(back)],[
            a([href(URL)],[
                'close'
            ]),
            &(nbsp),
            '|',
            &(nbsp),
            a([href(URL+'&doc=main')],[
                'index'
            ])
        ]),
        h1([],[
            Name
        ]),
        div([id(doc_main)],[
            Content
        ])
    ]).

doc_index -->
    {
        findall([page(PageName)| Section], 
            (
                doc_section(PageName, Section),
                memberchk(name(_), Section)
            ),
            Index
        )
    },
    doc_index(Index).

doc_index([]) --> [], !.
doc_index([Section| Index]) -->
    doc_index_li(Section),
    doc_index(Index).

doc_index_li(Section) -->
    {
        memberchk(page(Page), Section),
        memberchk(name(Name), Section),
        memberchk(url(URL), Section)
    },
    html([
        li([],[a([href(URL+'&doc='+Page)],[Name])])
    ]).


remove_doc_in_URL(Request, [search(NewSearch)|Request_clean]) :-
    select(search(Search), Request, Request_clean),
    removeAll(Search, doc=_, NewSearch). 


%%% ---------- Sections ----------- %%%

doc_section(main, []).

doc_section(simple, [ 
    name('Simple Mode'),
    url('./list?persons=2&objects=4&period=4&max=4&passesmin=1&passesmax=-1&contain=1+1p&mode=simple'),
    content(
        'All patterns with 4 objects and the length of 4 containing the sequence "1 1p"'
    )
]).

