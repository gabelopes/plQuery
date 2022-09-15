:- module(selector_matcher, [
  match/4
]).

match(_, _, _, all).
match(_, _, element(Tag, _, _), tag(Tag)).
match(_, _, element(_, Attributes, _), id(Id)) :-
  get_attribute(Attributes, id, Id).
match(_, _, element(_, Attributes, _), class(ClassName)) :-
  get_attribute(Attributes, class, Class),
  split_string(Class, " ", "", Classes),
  text_member(ClassName, Classes).
match(_, _, element(_, Attributes, _), attribute(Name, Operator, ComparingValue, _)) :-
  get_attribute(Attributes, Name, Value),
  compare_attribute_value(Value, Operator, ComparingValue).

get_attribute([], _, _) :- fail.
get_attribute([Name=Value|_], Name, Value).
get_attribute([_|Attributes], Name, Value) :-
  get_attribute(Attributes, Name, Value).

compare_attribute_value(Value, equals, Value).
compare_attribute_value(Value, contains, ComparingValue) :-
  sub_atom(Value, _, _, _, ComparingValue).
compare_attribute_value(Value, contains_word, ComparingValue) :-
  split_string(Value, " ", "", Words),
  member(ComparingValue, Words).
compare_attribute_value(Value, starts_with, ComparingValue) :-
  atom_concat(ComparingValue, _, Value).
compare_attribute_value(Value, ends_with, ComparingValue) :-
  atom_concat(_, ComparingValue, Value).
compare_attribute_value(Value, equals_before_hyphen, ComparingValue) :-
  split_string(Value, "-", "", [ComparingValue|_]).

text_member(Element, List) :-
  atom_string(Element, String),
  member(String, List).
text_member(Element, List) :-
  atom_string(Atom, Element),
  member(Atom, List).
