:- module(pseudo_class_matcher, [
  match/5
]).

:- use_module(matcher, [matches/5]).

match(_, _, element(_, _, []), _, empty).

match(_, 1, _, _, first_child).

match(_, Position, _, _, nth_child(nth(0, Position))).
match(_, Position, _, _, nth_child(nth(A, B))) :-
  N is (Position - B) / A,
  is_natural(N).

match(Siblings, Position, _, _, nth_last_child(Expression)) :-
  elements_length(Siblings, Length),
  SwappedPosition is Length - Position + 1, !,
  match(_, SwappedPosition, _, _, nth_child(Expression)).

match(Siblings, Position, Element, Selector, nth_of_type(Expression)) :-
  filter(Siblings, Selector, Matches),
  nth1(TranslatedPosition, Matches, _/Position), !,
  match(Matches, TranslatedPosition, Element, Selector, nth_child(Expression)).

match(Siblings, Position, _, _, last_child) :-
  elements_length(Siblings, Position).

is_natural(Number) :-
  Number >= 0,
  integer(Number).

filter(Elements, Selector, Matches) :-
  filter(Elements, 1, Selector, Matches).

filter([], _, _, []).
filter([Element|Elements], Position, Selector, [Element/Position|Matches]) :-
  matches([Element|Elements], _, Element, [selector(_, [Selector], _)], [_|_]),
  NextPosition is Position + 1,
  filter(Elements, NextPosition, Selector, Matches).
filter([_|Elements], Position, Selector, Matches) :-
  NextPosition is Position + 1,
  filter(Elements, NextPosition, Selector, Matches).

elements_length(Siblings, Length) :-
  findall(_, member(element(_, _, _), Siblings), Elements),
  length(Elements, Length).
