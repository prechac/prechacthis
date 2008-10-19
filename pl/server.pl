:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).

:- [html_helpers].
:- [html_main_page].
:- [html_info_page].


server(Port) :-
        http_server(http_dispatch, [port(Port)]).



:- http_handler('/', main_page, []).
:- http_handler('/list', main_page, []).
:- http_handler('/index.php', main_page, []).
:- http_handler('/info', info_page, []).
:- http_handler('/info.php', info_page, []).
%%:- http_handler('/js/', http_reply_file('../js/', []), []).

:- http_handler('/css/prechacthis.css', http_reply_file('../css/prechacthis.css', []), []).
:- http_handler('/images/favicon.png', http_reply_file('../images/favicon.png', []), []).


serve_css(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/css/', Local, Path),
	absolute_file_name(css(Local), FilePath, [access(read)]),
	absolute_file_name(css(,), Dir, [file_type(directory)]),
	sub_atom(FilePath, 0, _, _, Dir),
	http_reply_file(FilePath, [], Request).
