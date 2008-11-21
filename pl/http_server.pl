:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/httpd')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_header')).

server :- server(4211), !.
server(Port) :-
	http_server(http_dispatch, [port(Port)]).
server_stop :-
	http_stop_server(4211, []).

:- dynamic 
	constraintChecked/1,
	href_type/1,
	dataResult/2.


% ------ pages ------ %

http_main_page_path('/list').
http_main_page_path('/index.php').
http_info_page_path('/info').
http_info_page_path('/info.php').
http_joepass_page_path('/joe.pass').
http_joepass_page_path('/joepass.php').

:- http_handler('/', main_page, []).
:- forall(http_main_page_path(Path), http_handler(Path, main_page, [])).
:- forall(http_info_page_path(Path), http_handler(Path, info_page, [])).
:- forall(http_joepass_page_path(Path), http_handler(Path, joepass_page, [])).
:- http_handler('/swap', swap_page, []).




% ------ resources ------ %

resource(Name, css, css(FileName)) :-
	atom(Name), !,
	file_name_extension(Name, css, FileName).
resource(Name, css, File) :-
	var(Name),
	absolute_file_name(css(''), Dir, [file_type(directory)]),
	atom_concat(Dir, '*.css', Pattern),
	expand_file_name(Pattern, Files),
	member(File, Files),
	atom_concat(Dir, Local, File),
	file_name_extension(Name, css, Local).


resource(Name, images, images(FileName)) :-
	atom(Name), !,
	file_name_extension(Name, png, FileName).
resource(Name, images, File) :-
	var(Name),
	absolute_file_name(images(''), Dir, [file_type(directory)]),
	concat(Dir, '*.png', Pattern),
	expand_file_name(Pattern, Files),
	member(File, Files),
	atom_concat(Dir, Local, File),
	file_name_extension(Name, png, Local).

resource(Name, js, js(FileName)) :-
	atom(Name), !,
	file_name_extension(Name, js, FileName).
resource(Name, js, File) :-
	var(Name),
	absolute_file_name(js(''), Dir, [file_type(directory)]),
	concat(Dir, '*.js', Pattern),
	expand_file_name(Pattern, Files),
	member(File, Files),
	atom_concat(Dir, Local, File),
	file_name_extension(Name, js, Local).

% ------ files ------ %

server_location(CWD) :-
	working_directory(CWD, CWD).


	
%%------ css ------ %%

file_search_path(css, Path) :-
	server_location(ServerPath),
	atom_concat(ServerPath, '/css', Path).
	

:- http_handler('/css/', serve_css_resource, [prefix]).

serve_css_file(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/css/', Local, Path),
	absolute_file_name(css(Local), FilePath, [access(read)]),
	absolute_file_name(css(''), Dir, [file_type(directory)]),
	sub_atom(FilePath, 0, _, _, Dir),
	http_reply_file(FilePath, [], Request).

serve_css_resource(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/css/', Local, Path),
	file_name_extension(Name, css, Local),
	open_resource(Name, css, StreamIn),
	format('Content-type: text/css~n~n'),
	repeat,
	(at_end_of_stream(StreamIn) ->
		close(StreamIn), !;
		(
			read_pending_input(StreamIn, Chars, []),
			format('~s', [Chars]),
			fail
		)
	).
	
%%------ images ------ %%

file_search_path(images, Path) :-
	server_location(ServerPath),
	atom_concat(ServerPath, '/images', Path).

:- http_handler('/images/', serve_images_resource, [prefix]).

serve_images_file(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/images/', Local, Path),
	absolute_file_name(images(Local), FilePath, [access(read)]),
	absolute_file_name(images(''), Dir, [file_type(directory)]),
	sub_atom(FilePath, 0, _, _, Dir),
	http_reply_file(FilePath, [], Request).


serve_images_resource(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/images/', Local, Path),
	file_name_extension(Name, png, Local),
	open_resource(Name, images, StreamIn),
	format('Content-type: image/png~n~n'),
	repeat,
	(at_end_of_stream(StreamIn) ->
		close(StreamIn), !;
		(
			read_pending_input(StreamIn, Chars, []),
			format('~s', [Chars]),
			fail
		)
	).
%%------ js ------ %%

file_search_path(js, Path) :-
	server_location(ServerPath),
	atom_concat(ServerPath, '/js', Path).

:- http_handler('/js/', serve_js_resource, [prefix]).

serve_js_file(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/js/', Local, Path),
	absolute_file_name(js(Local), FilePath, [access(read)]),
	absolute_file_name(js(''), Dir, [file_type(directory)]),
	sub_atom(FilePath, 0, _, _, Dir),
	http_reply_file(FilePath, [], Request).

serve_js_resource(Request) :-
	memberchk(path(Path), Request),
	atom_concat('/js/', Local, Path),
	file_name_extension(Name, js, Local),
	open_resource(Name, js, StreamIn),
	format('Content-type: text/javascript~n~n'),repeat,
	(at_end_of_stream(StreamIn) ->
		close(StreamIn), !;
		(
			read_pending_input(StreamIn, Chars, []),
			format('~s', [Chars]),
			fail
		)
	).
