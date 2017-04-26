:- module(text, []).

:- use_module(char).


word([Char | Chars]) -->
  ( alpha(Char) ; digit(Char) ),
  ( { Chars = [] } ; word(Chars) ).


alpha(Char) --> [Char], { char:alpha(Char) }.
digit(Char) --> [Char], { char:digit(Char) }.

blank -->  [Char], { char:blank(Char) }.

blanks --> blank, blanks_star.

blanks_star --> [Char], { char:blank(Char) }, blanks_star.
blanks_star --> [].

% util
dotified(Parts, Codes) :-
  phrase(text:dotted_words(Parts), Codes).

dotted_words([Word | Words]) -->
  word(Word), ".", dotted_words(Words).

dotted_words([Word]) --> word(Word).

linebreak(Char) -->
  [Char], { char:linebreak }.


% Either 'Before' or 'Chars' must be ground for this to work:
anything_but(NotAllowed, Chars, Before, After) :-
  append([Chars, After], Before),
  append([NotAllowed, _], After),
  !.


anything_but(_, Chars, Chars, []).
  
rest(_, []).

space --> " ".
linebreak --> "\n".
