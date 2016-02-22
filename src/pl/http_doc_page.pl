:- module(http_doc_page,
	[
		doc_page/5,
        docFreeRequest/2,
        newDocRequest/3
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

:- [http_doc_sections].

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
        docFreeRequest(Request, DocFreeRequest),
        request2URL(DocFreeRequest, URL)
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
        docFreeRequest(Request, DocFreeRequest),
        newDocRequest(DocFreeRequest, 'main', MainRequest),
        request2URL(MainRequest, MainURL),
        request2URL(DocFreeRequest, URL)
    },
    html([
        div([class(back)],[
            a([href(URL)],[
                'close'
            ]),
            &(nbsp),
            '|',
            &(nbsp),
            a([href(MainURL)],[
                'index'
            ])
        ]),
        h1([],[
            Name
        ]),
        div([id(doc_main)],[
            Content,
            hr([]),
            ul([],[
                \doc_index
            ])
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


request2DocFreeURL(Request, URL) :-
    memberchk(search(Search), Request),
    memberchk(path(Path), Request),
    removeAll(Search, doc=_, DocFreeSearch),
    %concat_atom([_|ShortPath], '/', Path),
    parse_url(URL, [path(Path), search(DocFreeSearch)]).

request2DocURL(Request, DocPage, URL) :-
    memberchk(search(Search), Request),
    memberchk(path(Path), Request),
    removeAll(Search, doc=_, DocFreeSearch),
    DocSearch = [doc=DocPage|DocFreeSearch],
    %concat_atom([_|ShortPath], '/', Path),
    parse_url(URL, [path(Path), search(DocSearch)]).

docFreeRequest(Request, [search(NewSearch)|Request_clean]) :-
    select(search(Search), Request, Request_clean),
    removeAll(Search, doc=_, NewSearch). 
docFreeRequest(Request, Request) :- !.


newDocRequest(Request, DocPage, [search([doc=DocPage|NewSearch])|Request_clean]) :-
    select(search(Search), Request, Request_clean),
    removeAll(Search, doc=_, NewSearch).
newDocRequest(Request, DocPage, [search([doc=DocPage])|Request]) :- !.
