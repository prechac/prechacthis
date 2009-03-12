:- module(http_server,
	[
		server/0,
		server/2,
		stop_server/0,
		stop_server/1,
		http_main_page_path/1,
		http_info_page_path/1,
		http_joepass_page_path/1
	]
).

:- use_module(helpers).
:- use_module(http_swap_page).
:- use_module(http_joepass).
:- use_module(http_main_page).
:- use_module(http_info_page).

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_header')).
:- use_module(library('time')).


server :- server(4211, 2), !.
server(Port, Workers) :-
	format('\n*** PrechacThis ***\n'),
	http_server(http_dispatch, [port(Port), workers(Workers)]),
	format('Started PrechacThis server at port ~w\n', [Port]),
	format('You may access the server at http://localhost:~w/\n', [Port]).
	%format('Type "halt." to quit\n\n').

	
stop_server :- stop_server(4211), !.
stop_server(Port) :-
	http_stop_server(Port, []), 
	halt.


:-  working_directory(ServerPath, ServerPath), 
	atom_concat(ServerPath, 'lib', LibPath), 
	asserta(library_directory(LibPath)).

user:file_search_path(foreign, LibPath) :-
	working_directory(ServerPath, ServerPath),
	atom_concat(ServerPath, 'lib', LibPath).
	
user:file_search_path(foreign, '/usr/lib').


user:file_search_path(css, Path) :-
	server_location(ServerPath),
	atom_concat(ServerPath, 'css', Path).
user:file_search_path(images, Path) :-
	server_location(ServerPath),
	atom_concat(ServerPath, 'images', Path).
user:file_search_path(js, Path) :-
	server_location(ServerPath),
	atom_concat(ServerPath, 'js', Path).
	


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
:- forall(http_doc_frameset_path(Path), http_handler(Path, doc_frameset, [])).
:- http_handler('/swap', swap_page, []).


http_prefix_handler(Prefix, Pred, Options) :-
	current_prolog_flag(version, Version), 
	Version > 50661, !,
	http_handler(Prefix, Pred, [prefix|Options]).
http_prefix_handler(Prefix, Pred, Options) :-
	http_handler(prefix(Prefix), Pred, Options).

http_directory(css).
http_directory(images).
http_directory(js).


prechacthis_server_type(Type) :-
	recorded(prechacthis_server_type, Type), !.
prechacthis_server_type(resource).

:- forall(
		http_directory(Dir), 
		(
			prechacthis_server_type(ServerType),
			concat_atom(['/', Dir, '/'], Prefix),
			http_prefix_handler(Prefix, serve_file(ServerType, Dir), [])
		)
	).


% ------ resources ------ %


user:resource(Name, css, File) :- resource_dir(Name, css, File).
user:resource(Name, js, File) :- resource_dir(Name, js, File).
user:resource(Name, images, File) :- resource_dir(Name, images, File).

resource_dir(Name, Type, FileLocation) :-
	atom(Name), !,
	resource_type_extention(Type, Ext),
	file_name_extension(Name, Ext, FileName),
	FileLocation =.. [Type, FileName].
resource_dir(Name, Type, File) :-
	var(Name),
	DirLocation =.. [Type, ''],
	absolute_file_name(DirLocation, [file_type(directory)], Dir),
	resource_type_extention(Type, Ext),
	file_pattern(Dir, Ext, Pattern),
	expand_file_name(Pattern, Files),
	member(File, Files),
	atom_concat(Dir, Local, File),
	file_name_extension(Name, Ext, Local).


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


resource_content_type(css, 'text/css').
resource_content_type(images, 'image/png').
resource_content_type(js, 'text/javascript').

resource_type_extention(css, css).
resource_type_extention(js, js).
resource_type_extention(images, png).


% ------ files ------ %

server_location(CWD) :-
	working_directory(CWD, CWD).

serve_file(resource, Type, Request) :-	
	memberchk(path(Path), Request),
	concat_atom(['/', Type, '/'], Prefix),
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
serve_file(file, Type, Request) :-
	memberchk(path(Path), Request),
	concat_atom(['/', Type, '/'], Prefix),
	atom_concat(Prefix, Local, Path),
	FileLocation =.. [Type, Local],
	DirLocation =.. [Type, ''],
	absolute_file_name(FileLocation, FilePath, [access(read)]),
	absolute_file_name(DirLocation, Dir, [file_type(directory)]),
	sub_atom(FilePath, 0, _, _, Dir),
	http_reply_file(FilePath, [], Request).

