:- module(pseudo_class, [
  pseudo_class//1
]).

:- use_module(selector, [selector//1]).
:- use_module(an_plus_b, [an_plus_b//1]).
:- use_module(whitespace, [whitespaces//1]).

pseudo_class_operator --> ":".

left_parenthesis --> "(".

right_parenthesis --> ")".

pseudo_class(PseudoClasses) -->
  pseudo_class_operator,
  pseudo_class(Name, ParameterFunctor),
  parameter(ParameterFunctor, Parameter), !,
  {
    Parameter = none ->
      PseudoClasses = Name;
      PseudoClasses =.. [Name, Parameter]
  }.

pseudo_class('active', none) --> "active".
pseudo_class('any_link', none) --> "any-link".
pseudo_class('autofill', none) --> "autofill".
pseudo_class('blank', none) --> "blank".
pseudo_class('checked', none) --> "checked".
pseudo_class('current', none) --> "current".
pseudo_class('default', none) --> "default".
pseudo_class('defined', none) --> "defined".
pseudo_class('dir', any/direction) --> "dir".
pseudo_class('disabled', none) --> "disabled".
pseudo_class('empty', none) --> "empty".
pseudo_class('enabled', none) --> "enabled".
pseudo_class('first_child', none) --> "first-child".
pseudo_class('first_of_type', none) --> "first-of-type".
pseudo_class('first', none) --> "first".
pseudo_class('fullscreen', none) --> "fullscreen".
pseudo_class('future', none) --> "future".
pseudo_class('focus_visible', none) --> "focus-visible".
pseudo_class('focus_within', none) --> "focus-within".
pseudo_class('focus', none) --> "focus".
pseudo_class('has()', none) --> "has()".
pseudo_class('host', none) --> "host".
pseudo_class('host()', none) --> "host()".
pseudo_class('host_context()', none) --> "host-context()".
pseudo_class('hover', none) --> "hover".
pseudo_class('indeterminate', none) --> "indeterminate".
pseudo_class('in_range', none) --> "in-range".
pseudo_class('invalid', none) --> "invalid".
pseudo_class('is()', none) --> "is()".
pseudo_class('lang', any/lang) --> "lang".
pseudo_class('last_child', none) --> "last-child".
pseudo_class('last_of_type', none) --> "last-of-type".
pseudo_class('left', none) --> "left".
pseudo_class('link', none) --> "link".
pseudo_class('local_link', none) --> "local-link".
pseudo_class('not', selector:selector) --> "not".
pseudo_class('nth_child', an_plus_b:an_plus_b) --> "nth-child".
pseudo_class('nth_col', an_plus_b:an_plus_b) --> "nth-col".
pseudo_class('nth_last_child', an_plus_b:an_plus_b) --> "nth-last-child".
pseudo_class('nth_last_col', an_plus_b:an_plus_b) --> "nth-last-col".
pseudo_class('nth_last_of_type', an_plus_b:an_plus_b) --> "nth-last-of-type".
pseudo_class('nth_of_type', an_plus_b:an_plus_b) --> "nth-of-type".
pseudo_class('only_child', none) --> "only-child".
pseudo_class('only_of_type', none) --> "only-of-type".
pseudo_class('optional', none) --> "optional".
pseudo_class('out_of_range', none) --> "out-of-range".
pseudo_class('past', none) --> "past".
pseudo_class('picture_in_picture', none) --> "picture-in-picture".
pseudo_class('placeholder_shown', none) --> "placeholder-shown".
pseudo_class('paused', none) --> "paused".
pseudo_class('playing', none) --> "playing".
pseudo_class('read_only', none) --> "read-only".
pseudo_class('read_write', none) --> "read-write".
pseudo_class('required', none) --> "required".
pseudo_class('right', none) --> "right".
pseudo_class('root', none) --> "root".
pseudo_class('scope', none) --> "scope".
pseudo_class('state()', none) --> "state()".
pseudo_class('target_within', none) --> "target-within".
pseudo_class('target', none) --> "target".
pseudo_class('user_invalid', none) --> "user-invalid".
pseudo_class('valid', none) --> "valid".
pseudo_class('visited', none) --> "visited".
pseudo_class('where()', none) --> "where()".

parameter(none, none) --> "".

parameter(FunctorCombination, Parameter) -->
  left_parenthesis,
  whitespaces([no_new_line]),
  call_predicate(FunctorCombination, Parameter),
  whitespaces([no_new_line]),
  right_parenthesis.

call_predicate(FunctorCombination, Parameter, Stream, Difference) :-
  select_functor(FunctorCombination, Functor),
  prepare_predicate(Functor, [Parameter, Stream, Difference], Predicate),
  call(Predicate).

select_functor(_/Strict, Strict) :-
  fail.
select_functor(Wide/_, Wide).
select_functor(Functor, Functor).

prepare_predicate(ComposedFunctor, Arguments, Module:Predicate) :-
  ComposedFunctor =.. [':', Module, Functor|_],
  prepare_predicate(Functor, Arguments, Predicate).
prepare_predicate(Functor, Arguments, Predicate) :-
  Predicate =.. [Functor|Arguments].

any(Any) -->
  any_characters(Characters),
  { string_codes(Any, Characters) }.

any_characters([]), [Any] -->
  [Any],
  { string_codes(")", [Any]) }.
any_characters([Any|Rest]) -->
  [Any],
  any_characters(Rest).

% Strict

direction(ltr) --> "ltr".
direction(rtl) --> "rtl".
direction(_) --> {
  writeln("Could not recognize provided direction, value should be either 'ltr' or 'rtl'."),
  fail, !
}.

language(en) --> "en".
