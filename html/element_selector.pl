:- module(element_selector, [
  select/3
]).

:- use_module(element_matcher, [matches/5]).

select([], _, []).
select(_, [], []).
select(Dom, Selectors, Results) :-
  is_list(Dom), !,
  select(Dom, 1, Dom, Selectors, Results).
select(Element, Selectors, Results) :-
  select([Element], Selectors, Results).

select(_, _, [], _, []).
select(_, _, _, [], []).
select(Context, Position, [Element|Siblings], Selectors, Results) :-
  Element = element(_, _, _),
  matches(Context, Position, Element, Selectors, MatchedSelectors),
  combine(Context, Position, [Element|Siblings], Selectors, MatchedSelectors, CombineResults),
  NextPosition is Position + 1,
  select(Context, NextPosition, Siblings, Selectors, SiblingsResults), !,
  append(CombineResults, SiblingsResults, Results).
select(Context, Position, [_|Siblings], Selectors, Results) :-
  NextPosition is Position + 1,
  select(Context, NextPosition, Siblings, Selectors, Results).

combine(_, _, [], _, _, []).
combine(_, _, [element(_, _, Children)|_], Selectors, [], Results) :-
  filter_selectors(Selectors, [descendant], DescendantSelectors),
  select(Children, 1, Children, DescendantSelectors, Results).
combine(Context, Position, [element(Tag, Attributes, Children), Sibling|Siblings], Selectors, MatchedSelectors, Results) :-
  collect_element(element(Tag, Attributes, Children), MatchedSelectors, CollectedElement),
  select_descendants(Selectors, MatchedSelectors, Children, ChildrenResults),
  SiblingPosition is Position + 1,
  select_combinators([adjacent], MatchedSelectors, Context, SiblingPosition, [Sibling], AdjacentResults),
  select_combinators([sibling], MatchedSelectors, Context, SiblingPosition, [Sibling|Siblings], SiblingsResults),
  append([CollectedElement, ChildrenResults, AdjacentResults, SiblingsResults], Results).
combine(_, _, [element(Tag, Attributes, Children)], Selectors, MatchedSelectors, Results) :-
  collect_element(element(Tag, Attributes, Children), MatchedSelectors, CollectedElement),
  select_descendants(Selectors, MatchedSelectors, Children, ChildrenResults),
  append(CollectedElement, ChildrenResults, Results).

collect_element(Element, MatchedSelectors, [Element]) :-
  has_final_combinator(MatchedSelectors).
collect_element(_, _, []).

has_final_combinator(Selectors) :-
  get_combinators(Selectors, Combinators),
  member(none, Combinators).

select_descendants(Selectors, MatchedSelectors, Children, ChildrenResults) :-
  filter_selectors(Selectors, [descendant], DescendantSelectors),
  get_combinators(MatchedSelectors, Combinators),
  filter_selectors(Combinators, [descendant, child], ChildrenCombinators),
  append(DescendantSelectors, ChildrenCombinators, AllSelectors),
  select(Children, 1, Children, AllSelectors, ChildrenResults).

select_combinators(Types, MatchedSelectors, Context, Position, Elements, Results) :-
  get_combinators(MatchedSelectors, Combinators),
  filter_selectors(Combinators, Types, FilteredCombinators),
  select(Context, Position, Elements, FilteredCombinators, Results).

get_combinators([], []).
get_combinators([selector(_, _, Combinator)|Selectors], [Combinator|Combinators]) :-
  get_combinators(Selectors, Combinators).

filter_selectors([], _, []).
filter_selectors([Selector|Selectors], Types, [Selector|FilteredSelectors]) :-
  Selector = selector(Type, _, _),
  member(Type, Types),
  filter_selectors(Selectors, Types, FilteredSelectors).
filter_selectors([_|Selectors], Types, FilteredSelectors) :-
  filter_selectors(Selectors, Types, FilteredSelectors).
