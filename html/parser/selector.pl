:- module(selector, [
  parse_query/2,
  selector//1
]).

:- use_module('../../utility/dcg').
:- use_module(identifier).
:- use_module(whitespace, [whitespaces//1]).
:- use_module(pseudo_selector).

selector_operator(id) --> "#".
selector_operator(class) --> ".".

left_bracket --> "[".

right_bracket --> "]".

attribute_operator(equals) --> "=".
attribute_operator(contains) --> "*=".
attribute_operator(contains_word) --> "~=".
attribute_operator(starts_with) --> "^=".
attribute_operator(ends_with) --> "$=".
attribute_operator(equals_before_hyphen) --> "|=".

attribute_option(case_insensitive) --> "i"; "I".
attribute_option(case_sensitive) --> "s"; "S".

combinator_operator(child) -->
  whitespaces([no_new_line]),
  ">".
combinator_operator(adjacent) -->
  whitespaces([no_new_line]),
  "+".
combinator_operator(sibling) -->
  whitespaces([no_new_line]),
  "~".
combinator_operator(descendant) -->
  whitespaces([no_new_line, required]).

all_operator --> "*".

selector_separator --> ",".

selectotrs([], [], []).
selectors([Selector|Selectors]) -->
  selector(Selector),
  (
    selector_separator ->
      selectors(Selectors);
      {
        Selectors = []
      }
  ).
selectors([Selector]) -->
  selector(Selector).
selectors([]) --> "".

selector(Selector) -->
  selector(descendant, Selector).

selector(Type, selector(Type, CompoundSelectors, Combinator)) -->
  whitespaces([no_new_line]),
  compound_selectors(CompoundSelectors),
  whitespaces([no_new_line]),
  combinator(Combinator),
  whitespaces([no_new_line]).

compound_selectors([], [], []).
compound_selectors([CompoundSelector|CompoundSelectors]) -->
  compound_selector(CompoundSelector), !,
  compound_selectors(CompoundSelectors).
compound_selectors([CompoundSelector]) -->
  compound_selector(CompoundSelector).
compound_selectors([]) --> "".

compound_selector(SimpleSelector:PseudoSelectors) -->
  simple_selector(SimpleSelector), !,
  pseudo_selectors(PseudoSelectors).
compound_selector(SimpleSelector) -->
  simple_selector(SimpleSelector).

pseudo_selectors([], [], []).
pseudo_selectors([PseudoSelector|PseudoSelectors]) -->
  pseudo_selector(PseudoSelector), !,
  pseudo_selectors(PseudoSelectors).
pseudo_selectors([]) --> "".

simple_selector(SimpleSelector) -->
  selector_operator(Type), !,
  identifier(Identifier),
  { SimpleSelector =.. [Type, Identifier] }.
simple_selector(attribute(Name, Operator, Value, Options)) -->
  left_bracket, !,
  whitespaces([no_new_line]),
  identifier(Name),
  whitespaces([no_new_line]),
  attribute_operator(Operator),
  whitespaces([no_new_line]),
  quoted_identifier(Value),
  whitespaces([no_new_line]),
  attribute_options(Options),
  whitespaces([no_new_line]),
  right_bracket.
simple_selector(all) --> all_operator, !.
simple_selector(tag(Name)) --> identifier(Name), !.
simple_selector(all) --> "", !.

attribute_options([Option|Options]) -->
  attribute_option(Option), !,
  attribute_options(Options).
attribute_options([]) --> "", !.

combinator(Combinator) -->
  combinator_operator(Type), !,
  whitespaces([no_new_line]),
  selector(Type, Combinator).
combinator(none) --> "", !.

parse_query(Query, Selectors) :-
  parse(Query, selector:selectors(Selectors)).
