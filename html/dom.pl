:- module(html, [
  parse_html/2,
  query/3
]).

:- use_module(parser/selector).
:- use_module(element_selector).

parse_html(HTML, DOM) :-
  new_memory_file(File),
  open_memory_file(File, write, WriteStream),
  write(WriteStream, HTML),
  close(WriteStream),
  open_memory_file(File, read, ReadStream),
  load_html(stream(ReadStream), DOM, []),
  close(ReadStream),
  free_memory_file(File).

query(DOM, Query, Results) :-
  parse_query(Query, Selectors), !,
  select(DOM, Selectors, Results).
