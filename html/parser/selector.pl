:- module(selector, [
  parse_query/2,
  selector//1
]).

:- use_module('../../utility/dcg').
:- use_module(identifier).
:- use_module(whitespace, [whitespaces//1]).
:- use_module(pseudo_class).

selector_operator(id) --> "#".
selector_operator(class) --> ".".

attribute_operator(equals) --> "=".
attribute_operator(contains) --> "*=".
attribute_operator(contains_word) --> "~=".
attribute_operator(starts_with) --> "^=".
attribute_operator(ends_with) --> "$=".
attribute_operator(equals_before_hyphen) --> "|=".

attribute_option(case_insensitive) --> "i"; "I".
attribute_option(case_sensitive) --> "s"; "S".

combinator_operator(child) --> ">".
combinator_operator(adjacent) --> "+".
combinator_operator(sibling) --> "~".
combinator_operator(descendant) --> whitespaces([no_new_line, required]).

all_operator --> "*".

left_bracket --> "[".

right_bracket --> "]".

selector_separator --> ",".

space --> whitespaces([no_new_line]).

selectors([Selector|Selectors]) -->
  space,
  single_selector(Selector),
  space,
  (
    selector_separator ->
      selectors(Selectors);
      {
        Selectors = []
      }
  ).
selectors([]) --> "".

single_selector(Selector) -->
  single_selector(descendant, Selector).

single_selector(Type, selector(Type, Selectors, Combinator)) -->
  compound_selectors(Selectors),
  space,
  (
    combinator(Combinator);
    { Combinator = none }
  ).

compound_selectors([Selector|Selectors]) -->
  compound_selector(Selector),
  (
    compound_selectors(Selectors);
    { Selectors = [] }
  ).

compound_selector(CompoundSelector) -->
  selector(Selector),
  (
    pseudo_classes(PseudoClasses) ->
      { CompoundSelector = Selector:PseudoClasses };
      { CompoundSelector = Selector }
  ).

pseudo_classes([PseudoClass|PseudoClasses]) -->
  pseudo_class(PseudoClass),
  (
    pseudo_classes(PseudoClasses);
    { PseudoClasses = [] }
  ).

selector(Selector) -->
  selector_operator(Type), !,
  identifier(Identifier),
  { Selector =.. [Type, Identifier] }, !.
selector(attribute(Name, Operator, Value, Options)) -->
  left_bracket, !,
  space,
  identifier(Name),
  space,
  attribute_operator(Operator),
  space,
  quoted_identifier(Value),
  space,
  attribute_options(Options),
  space,
  right_bracket, !.
selector(all) --> all_operator, !.
selector(tag(Name)) --> identifier(Name), !.


attribute_options([Option|Options]) -->
  attribute_option(Option),
  attribute_options(Options).
attribute_options([]) --> "".

combinator(Combinator) -->
  combinator_operator(Type), !,
  space,
  single_selector(Type, Combinator).

parse_query(Query, Selectors) :-
  parse(Query, selector:selectors(Selectors)).
