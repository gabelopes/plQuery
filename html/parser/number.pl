:- module(number, [
  lax_integer//1,
  sign//1,
  unsigned_integer//1
]).

:- use_module(library(dcg/basics), [
  digits//1,
  integer//1
]).

lax_integer(Integer) --> integer(Integer), !.
lax_integer(Sign) --> sign(Sign), !.
lax_integer(1) --> "", !.

sign(1) --> "+", !.
sign(-1) --> "-", !.

unsigned_integer(UnsignedInteger) -->
  digits(Digits),
  { number_codes(UnsignedInteger, Digits) }.
