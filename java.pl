:- module(java, []).

import_statement([import, Parts]) -->
  text:word("import"), text:blanks, text:dotted_words(Parts), blanks, ";".

package_declaration([package, Parts]) -->
  text:word("package"), text:blanks, text:dotted_words(Parts), blanks, ";".

line_comment(Contents) -->
  "//", text:anything_but("\n", Contents), "\n".


mutli_comment(Contents) -->
  "/*", anything_but("*/", Contents).
