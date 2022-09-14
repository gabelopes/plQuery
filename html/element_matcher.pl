:- module(element_matcher, [
  match/4,
  matches/5
]).

:- use_module(library(dialect/ifprolog), [atom_split/3]).

matches(_, _, _, [], []).
matches(Siblings, Position, Element, [selector(Type, SimpleSelectors, Combinator)|Selectors], [selector(Type, SimpleSelectors, Combinator)|Matches]) :-
  match(Siblings, Position, Element, SimpleSelectors),
  matches(Siblings, Position, Element, Selectors, Matches).
matches(Siblings, Position, Element, [_|Selectors], Matches) :-
  matches(Siblings, Position, Element, Selectors, Matches).

match(_, _, _, []).
match(Siblings, Position, Element, [Selector:PseudoClasses|Selectors]) :-
  match_selector(Siblings, Position, Element, Selector),
  match_pseudo_classes(Siblings, Position, Element, Selector, PseudoClasses),
  match(Siblings, Position, Element, Selectors).
match(Siblings, Position, Element, [Selector|Selectors]) :-
  match_selector(Siblings, Position, Element, Selector),
  match(Siblings, Position, Element, Selectors).

match_selector(_, _, _, all).

match_selector(_, _, element(Tag, _, _), tag(Tag)).

match_selector(_, _, element(_, Attributes, _), id(Id)) :-
  get_attribute(Attributes, id, Id).

match_selector(_, _, element(_, Attributes, _), class(ClassName)) :-
  get_attribute(Attributes, class, Class),
  atom_split(Class, ' ', Classes),
  member(ClassName, Classes).

match_selector(_, _, element(_, Attributes, _), attribute(Name, Operator, ComparingValue, _)) :-
  get_attribute(Attributes, Name, Value),
  compare_attribute_value(Value, Operator, ComparingValue).

% Pseudo Classes
% In order to make the Pseudo-Classes matchers, please read the comment in query.pl
match_pseudo_classes(_, _, _, _, []).
match_pseudo_classes(Siblings, Position, Element, Selector, [PseudoClass|PseudoClasses]) :-
  match_pseudo_class(Siblings, Position, Element, Selector, PseudoClass),
  match_pseudo_classes(Siblings, Position, Element, Selector, PseudoClasses).

match_pseudo_class(_, Position, _, _, nth_child(nth(0, Position))).
match_pseudo_class(_, Position, _, _, nth_child(nth(A, B))) :-
  N is (Position - B) / A,
  integer(N).

match_pseudo_class(_, 1, _, _, first_child).

match_pseudo_class(Siblings, Position, _, _, last_child) :-
  length(Siblings, Position).

compare_attribute_value(Value, equals, Value).
compare_attribute_value(Value, contains, ComparingValue) :-
  sub_atom(Value, _, _, _, ComparingValue).
compare_attribute_value(Value, contains_word, ComparingValue) :-
  atom_split(Value, ' ', Words),
  member(ComparingValue, Words).
compare_attribute_value(Value, starts_with, ComparingValue) :-
  atom_concat(ComparingValue, _, Value).
compare_attribute_value(Value, ends_with, ComparingValue) :-
  atom_concat(_, ComparingValue, Value).
compare_attribute_value(Value, equals_before_hyphen, ComparingValue) :-
  atom_split(Value, '-', [ComparingValue|_]).

get_attribute([], _, _) :- fail.
get_attribute([Name=Value|_], Name, Value).
get_attribute([_|Attributes], Name, Value) :-
  get_attribute(Attributes, Name, Value).
