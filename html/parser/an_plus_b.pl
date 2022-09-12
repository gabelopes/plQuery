:- module(an_plus_b, [
  an_plus_b//1
]).

:- use_module(whitespace, [whitespaces//1]).
:- use_module(number, [
  lax_integer//1,
  sign//1,
  unsigned_integer//1
]).

n --> "n".

% Basic rules
an_plus_b(nth(2, 0)) --> "even".
an_plus_b(nth(2, 1)) --> "odd".

% Where A is the Step and B is the Offset (An+B)
an_plus_b(nth(Step, Offset)) -->
  lax_integer(Step),
  n,
  whitespaces([no_new_line]),
  sign(Sign),
  whitespaces([no_new_line]),
  unsigned_integer(B),
  { Offset is Sign * B }.

% Step only
an_plus_b(nth(Step, 0)) -->
  lax_integer(Step),
  n.

% Offset only
an_plus_b(nth(0, Offset)) --> unsigned_integer(Offset).
