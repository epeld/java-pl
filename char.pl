:- module(char, []).

upper_alphabet("ABCDEFGHIJKLMNOPQRSTUVWXYZ").
lower_alphabet("abcdefghijklmnopqrstuvwxyz").

numeric_chars("0123456789").

alphabet(A) :- upper_alphabet(A).
alphabet(A) :- lower_alphabet(A).

digit(Digit) :-
  numeric_chars(Digits),
  member(Digit, Digits).

digit_value(Digit, Value) :-
  numeric_chars(NumericChars),
  nth0(Value, NumericChars, Digit).

alpha(Char) :-
  alphabet(Alphabet),
  member(Char, Alphabet).

whitespace(Char) :-
  member(Char, " \t\n").

linebreak(Char) :-
  member(Char, "\n\r").

blank(Char) :-
  member(Char, "\t ").

space(Char) :- [Char] = " ".
