:- module(java, []).

import_statement([import, Parts]) -->
  text:word("import"), text:blanks, text:dotted_words(Parts), blanks, ";".

package_declaration([package, Parts]) -->
  text:word("package"), text:blanks, text:dotted_words(Parts), blanks, ";".

line_comment(Contents) -->
  "//", text:anything_but("\n", Contents), "\n".

multi_comment(Contents) -->
  "/*", text:anything_but("*/", Contents), "*/".

class_declaration([class, Name, Body, Props, Throws]) -->
  % TODO check for public, static, final etc
  "class", text:blanks, word(Name), text:blanks*, block_body(Body),
  Props = [], Throws = [].


block_body(Body). % TODO


