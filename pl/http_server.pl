:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_header')).
:- use_module(library('time')).


server :- server(4211), !.
server(Port) :-
	format('\n*** PrechacThis ***\n'),
	http_server(http_dispatch, [port(Port)]),
	format('To use PrechacThis open http://localhost:4211 with your favorite webbrowser.\n'),
	format('Type "stop_server." to quit.\n\n').

	
stop_server :- stop_server(4211), !.
stop_server(Port) :-
	http_stop_server(Port, []), 
	halt.

:- dynamic 
	constraintChecked/1,
	href_type/1,
	dataResult/2.



file_search_path(foreign, Path) :-
	server_location(Path).

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
	absolute_file_name(css(''), [file_type(directory)], Dir),
	file_pattern(Dir, css, Pattern),
	expand_file_name(Pattern, Files),
	member(File, Files),
	atom_concat(Dir, Local, File),
	file_name_extension(Name, css, Local).


resource(Name, images, images(FileName)) :-
	atom(Name), !,
	file_name_extension(Name, png, FileName).
resource(Name, images, File) :-
	var(Name),
	absolute_file_name(images(''), [file_type(directory)], Dir),
	file_pattern(Dir, png, Pattern),
	expand_file_name(Pattern, Files),
	member(File, Files),
	atom_concat(Dir, Local, File),
	file_name_extension(Name, png, Local).

resource(Name, js, js(FileName)) :-
	atom(Name), !,
	file_name_extension(Name, js, FileName).
resource(Name, js, File) :-
	var(Name),
	absolute_file_name(js(''), [file_type(directory)], Dir),
	file_pattern(Dir, js, Pattern),
	expand_file_name(Pattern, Files),
	member(File, Files),
	atom_concat(Dir, Local, File),
	file_name_extension(Name, js, Local).



file_pattern(Dir, Ext, Pattern) :-
	walk_subdirs(Dir, Subdir),
	concat_atom([Subdir, '*.', Ext], Pattern).

walk_subdirs(Dir, Dir).
walk_subdirs(Dir, SubSubDir) :-
	concat_atom([Dir, '*'], Pattern),
	expand_file_name(Pattern, Dirs),
	member(SubDir, Dirs),
	exists_directory(SubDir),
	atom_concat(SubDir, '/', SubDirPath),
	walk_subdirs(SubDirPath, SubSubDir).


serve_resource(Type, Request) :-
	concat_atom(['/', Type, '/'], Prefix),
	memberchk(path(Path), Request),
	atom_concat(Prefix, Local, Path),
	resource_type_extention(Type, Ext),
	file_name_extension(Name, Ext, Local),
	open_resource(Name, Type, StreamIn),
	resource_content_type(Type, ContentType),
	format('Content-type: ~w~n~n', [ContentType]),
	%time_file
	repeat,
	(at_end_of_stream(StreamIn) ->
		close(StreamIn), !;
		(
			read_pending_input(StreamIn, Chars, []),
			format('~s', [Chars]),
			fail
		)
	).
	
resource_content_type(css, 'text/css').
resource_content_type(images, 'image/png').
resource_content_type(js, 'text/javascript').

resource_type_extention(css, css).
resource_type_extention(js, js).
resource_type_extention(images, png).


% ------ files ------ %

server_location(CWD) :-
	working_directory(CWD, CWD).

	
%%------ css ------ %%


http_prefix_handler(Prefix, Pred) :-
	current_prolog_flag(version, Version), 
	Version > 50655, !,
	http_handler(Prefix, Pred, [prefix]).
http_prefix_handler(Prefix, Pred) :-
	http_handler(prefix(Prefix), Pred, []).


file_search_path(css, Path) :-
	server_location(ServerPath),
	atom_concat(ServerPath, '/css', Path).
	

:- http_prefix_handler('/css/', serve_resource(css)).

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
	%time_file
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

:- http_prefix_handler('/images/', serve_resource(images)).

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

:- http_prefix_handler('/js/', serve_resource(js)).

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
