:- module(text, []).

:- use_module(char).


word([Char | Chars]) -->
  ( alpha(Char) ; digit(Char) ),
  ( { Chars = [] } ; word(Chars) ).


alpha(Char) --> [Char], { char:alpha(Char) }.
digit(Char) --> [Char], { char:digit(Char) }.

blank([Space | Rest], Rest) :-
  char:space(Space).

blanks --> blank, blanks_star.

blanks_star(A, B) :-
  nonvar(A),
  append(Chars, B, A),
  maplist(char:blank, Chars).



blanks_star --> [].

blanks_nl_star(A, B) :-
  nonvar(A),
  append(Chars, B, A),
  maplist(char:whitespace, Chars).

blanks_nl_star --> "\n".


whitespace(A, B) :-
  % if WRITING, let whitespace produce nothing.
  var(A),
  A = B, !.

whitespace(A, B) :-
  % when parsing, match any whitespace
  append(Chars, B, A),
  maplist(char:whitespace, Chars).
  

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
