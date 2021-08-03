:- module(pseudo_selector, [
  pseudo_selector//1
]).

:- use_module(selector, [selector//1]).
:- use_module(whitespace, [whitespaces//1]).

pseudo_selector_operator --> ":".

left_parenthesis --> "(".

right_parenthesis --> ")".

pseudo_selector(pseudo_selector(Name, Parameter)) -->
  pseudo_selector_operator,
  pseudo_class(Name, ParameterFunctor),
  parameter(ParameterFunctor, Parameter).

pseudo_class('active', none) --> "active".
pseudo_class('any-link', none) --> "any-link".
pseudo_class('autofill', none) --> "autofill".
pseudo_class('blank', none) --> "blank".
pseudo_class('checked', none) --> "checked".
pseudo_class('current', none) --> "current".
pseudo_class('default', none) --> "default".
pseudo_class('defined', none) --> "defined".
pseudo_class('dir', direction) --> "dir".
pseudo_class('disabled', none) --> "disabled".
pseudo_class('empty', none) --> "empty".
pseudo_class('enabled', none) --> "enabled".
pseudo_class('first', none) --> "first".
pseudo_class('first-child', none) --> "first-child".
pseudo_class('first-of-type', none) --> "first-of-type".
pseudo_class('fullscreen', none) --> "fullscreen".
pseudo_class('future', none) --> "future".
pseudo_class('focus', none) --> "focus".
pseudo_class('focus-visible', none) --> "focus-visible".
pseudo_class('focus-within', none) --> "focus-within".
pseudo_class('has()', none) --> "has()".
pseudo_class('host', none) --> "host".
pseudo_class('host()', none) --> "host()".
pseudo_class('host-context()', none) --> "host-context()".
pseudo_class('hover', none) --> "hover".
pseudo_class('indeterminate', none) --> "indeterminate".
pseudo_class('in-range', none) --> "in-range".
pseudo_class('invalid', none) --> "invalid".
pseudo_class('is()', none) --> "is()".
pseudo_class('lang', language) --> "lang".
pseudo_class('last-child', none) --> "last-child".
pseudo_class('last-of-type', none) --> "last-of-type".
pseudo_class('left', none) --> "left".
pseudo_class('link', none) --> "link".
pseudo_class('local-link', none) --> "local-link".
pseudo_class('not', selector:selector) --> "not".
pseudo_class('nth-child()', none) --> "nth-child()".
pseudo_class('nth-col()', none) --> "nth-col()".
pseudo_class('nth-last-child()', none) --> "nth-last-child()".
pseudo_class('nth-last-col()', none) --> "nth-last-col()".
pseudo_class('nth-last-of-type()', none) --> "nth-last-of-type()".
pseudo_class('nth-of-type()', none) --> "nth-of-type()".
pseudo_class('only-child', none) --> "only-child".
pseudo_class('only-of-type', none) --> "only-of-type".
pseudo_class('optional', none) --> "optional".
pseudo_class('out-of-range', none) --> "out-of-range".
pseudo_class('past', none) --> "past".
pseudo_class('picture-in-picture', none) --> "picture-in-picture".
pseudo_class('placeholder-shown', none) --> "placeholder-shown".
pseudo_class('paused', none) --> "paused".
pseudo_class('playing', none) --> "playing".
pseudo_class('read-only', none) --> "read-only".
pseudo_class('read-write', none) --> "read-write".
pseudo_class('required', none) --> "required".
pseudo_class('right', none) --> "right".
pseudo_class('root', none) --> "root".
pseudo_class('scope', none) --> "scope".
pseudo_class('state()', none) --> "state()".
pseudo_class('target', none) --> "target".
pseudo_class('target-within', none) --> "target-within".
pseudo_class('user-invalid', none) --> "user-invalid".
pseudo_class('valid', none) --> "valid".
pseudo_class('visited', none) --> "visited".
pseudo_class('where()', none) --> "where()".

parameter(none, none) --> "".
parameter(Functor, Parameter) -->
  left_parenthesis,
  whitespaces([no_new_line]),
  call_predicate(Functor, Parameter),
  whitespaces([no_new_line]),
  right_parenthesis.

call_predicate(Functor, Parameter, Stream, Difference) :-
  prepare_predicate(Functor, [Parameter, Stream, Difference], Predicate),
  call(Predicate).

prepare_predicate(ComposedFunctor, Arguments, Module:Predicate) :-
  ComposedFunctor =.. [':', Module, Functor|_],
  prepare_predicate(Functor, Arguments, Predicate).
prepare_predicate(Functor, Arguments, Predicate) :-
  Predicate =.. [Functor|Arguments].

direction(ltr) --> "ltr".
direction(rtl) --> "ltr".

language(en) --> "en".
