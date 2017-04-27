:- module(java, []).

import_statement([import, Parts]) -->
  text:word("import"), text:blanks, text:dotted_words(Parts), text:blanks_star, ";".

package_declaration([package, Parts]) -->
  text:word("package"), text:blanks, text:dotted_words(Parts), text:blanks_star, ";".

line_comment(Contents) -->
  "//", text:anything_but("\n", Contents), "\n".

multi_comment(Contents) -->
  "/*", text:anything_but("*/", Contents), "*/".

class_declaration([class, Name, Body, Props]) -->
  % TODO check for public, static, final etc
  ( ( ( "public" ; "protected" ; "private"), space) ; [] ),
  "class", space, text:word(Name), space, "{",
  { Props = [], Body = [] }.


space([Space | Rest], Rest) :-
  char:space(Space).

space(Before, After) :-
  nonvar(Before),
  phrase((text:space, separator), Before, After).

linebreak(Before, After) :-
  nonvar(Before), 
  !,
  phrase(text:linebreak, Before, After).

linebreak(Before, After) :-
  nonvar(Before), 
  !,
  phrase(separator, Before, After).

separator -->
  ( text:blanks ; text:linebreak ; multi_comment(_) ; line_comment(_)),
  ( separator ; [] ).


block_body([block, Body]) -->
  rest(Body).

import_statements([Import | Imports]) -->
  import_statement(Import),
  (linebreak, import_statements(Imports) ; { Imports = [] }).

file([file, Package, Imports, Class]) -->
  package_declaration(Package), linebreak,
  import_statements(Imports), linebreak,
  class_declaration(Class),
  text:rest.
