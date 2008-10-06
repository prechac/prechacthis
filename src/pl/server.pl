:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

:- http_handler('/', root, []).
:- http_handler('/hello/world', hello_world, []).

myroot(_Request) :- 
	format("Hello World").

root(_Request) :-
        reply_html_page([
							title('Demo server')
                        ],
                        [
							p(
								a(
									href('hello/world'), 
									hello
								)
							)
                        ]).

hello_world(_Request) :-
        reply_html_page([ title('Hello World')
                        ],
                        [ h1('Hello World'),
                          p('This is my first page')
                        ]).


reply(Request) :-
        format('Content-type: text/html~n~n', []),
        format('<html>~n', []),
        format('<table border=1>~n'),
        print_request(Request),
        format('~n</table>~n'),
        format('</html>~n', []).

print_request([]).
print_request([H|T]) :-
        H =.. [Name, Value],
        format('<tr><td>~w<td>~w~n', [Name, Value]),
        print_request(T).
