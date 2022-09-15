:- module(pseudo_class_matcher, [
  match/5
]).

match(_, _, element(_, _, []), _, empty).
match(_, 1, _, _, first_child).
match(_, Position, _, _, nth_child(nth(0, Position))).
match(_, Position, _, _, nth_child(nth(A, B))) :-
  N is (Position - B) / A,
  is_natural(N).
match(Siblings, Position, _, _, nth_last_child(Expression)) :-
  length(Siblings, Length),
  SwappedPosition is Length - Position + 1,
  match(_, SwappedPosition, _, _, nth_child(Expression)).
match(Siblings, Position, _, _, last_child) :-
  length(Siblings, Position).

is_natural(Number) :-
  Number >= 0,
  integer(Number).
