:- module(text, []).

:- use_module(char).


word([Char | Chars]) -->
  ( alpha(Char) ; digit(Char) ),
  ( { Chars = [] } ; word(Chars) ).


alpha(Char) --> [Char], { char:alpha(Char) }.
digit(Char) --> [Char], { char:digit(Char) }.

blank -->  [Char], { char:blank(Char) }.

blanks --> blank, blanks1.

blanks1 --> [Char], { char:blank(Char) }, blanks1.
blanks1 --> [].

dotted_words([Word | Words]) -->
  word(Word), ".", dotted_words(Words).

dotted_words([Word]) --> word(Word).

linebreak(Char) -->
  [Char], { char:linebreak }.


% Either 'Before' or 'Chars' must be ground for this to work:
anything_but([NotAllowed], Chars, Before, [NotAllowed | After]) :-
  ground(Before), nonvar(NotAllowed),
  append(Chars, [NotAllowed | After], Before), !.

anything_but([NotAllowed], Chars, Before, [NotAllowed | After]) :-
  ground(Chars), nonvar(NotAllowed),
  append(Chars, [NotAllowed | After], Before).

anything_but(_, Chars, Chars, []).
  
