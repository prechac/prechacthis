:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).

:- [html_helpers].
:- [html_main_page].
:- [html_info_page].


server(Port) :-
	http_server(http_dispatch, [port(Port)]).


server_reload(Port) :-
	http_stop_server(Port, []), consult('pl/server'), server(Port).
	

% ------ pages ------ %
:- http_handler('/', main_page, []).
:- http_handler('/list', main_page, []).
:- http_handler('/index.php', main_page, []).
:- http_handler('/info', info_page, []).
:- http_handler('/info.php', info_page, []).


% ------ files ------ %

server_location(CWD) :-
	working_directory(CWD, CWD).

file_search_path(css, Path) :-
	server_location(ServerPath),
	atom_concat(ServerPath, '/css', Path).
	
:- http_handler(prefix('/css/'), serve_css, []).

serve_css(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/css/', Local, Path),
	absolute_file_name(css(Local), FilePath, [access(read)]),
	absolute_file_name(css(''), Dir, [file_type(directory)]),
	sub_atom(FilePath, 0, _, _, Dir),
	http_reply_file(FilePath, [], Request).



file_search_path(images, Path) :-
		server_location(ServerPath),
		atom_concat(ServerPath, '/images', Path).

:- http_handler(prefix('/images/'), serve_images, []).

serve_images(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/images/', Local, Path),
	absolute_file_name(images(Local), FilePath, [access(read)]),
	absolute_file_name(images(''), Dir, [file_type(directory)]),
	sub_atom(FilePath, 0, _, _, Dir),
	http_reply_file(FilePath, [], Request).



file_search_path(js, Path) :-
			server_location(ServerPath),
			atom_concat(ServerPath, '/js', Path).

:- http_handler(prefix('/js/'), serve_js, []).

serve_js(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/js/', Local, Path),
	absolute_file_name(js(Local), FilePath, [access(read)]),
	absolute_file_name(js(''), Dir, [file_type(directory)]),
	sub_atom(FilePath, 0, _, _, Dir),
	http_reply_file(FilePath, [], Request).

