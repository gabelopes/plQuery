:- module(pseudo_class_matcher, [
  match/5
]).

match(_, _, element(_, _, []), _, empty).
match(_, 1, _, _, first_child).
match(_, Position, _, _, nth_child(nth(0, Position))).
match(_, Position, _, _, nth_child(nth(A, B))) :-
  N is (Position - B) / A,
  integer(N).
match(Siblings, Position, _, _, last_child) :-
  length(Siblings, Position).
