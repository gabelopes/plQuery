:- moudle(an_plus_b, [
  an_plus_b//1
]).

an_plus_b(nth(2, 0)) --> "even".
an_plus_b(nth(2, 1)) --> "odd".

% Where A is the Multiplier and B is the increment (An+B)
an_plus_b(nth(A, B)) --> [].
