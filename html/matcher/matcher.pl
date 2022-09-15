:- module(matcher, [
  match/4,
  matches/5
]).

:- use_module(selector_matcher, [match/4 as match_selector]).
:- use_module(pseudo_class_matcher, [match/5 as match_pseudo_class]).

matches(_, _, _, [], []).
matches(Siblings, Position, Element, [Selector|Selectors], [Selector|Matches]) :-
  Selector = selector(_, CompoundSelectors, _),
  match(Siblings, Position, Element, CompoundSelectors),
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

match_pseudo_classes(_, _, _, _, []).
match_pseudo_classes(Siblings, Position, Element, Selector, [PseudoClass|PseudoClasses]) :-
  match_pseudo_class(Siblings, Position, Element, Selector, PseudoClass),
  match_pseudo_classes(Siblings, Position, Element, Selector, PseudoClasses).
