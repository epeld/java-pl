:- module(javaclass, []).


block_body([block, Contents]) -->
  "{",
  block_body_content(Contents),
  "}".


block_body_content([ Part1, Part2 | Rest ]) -->
  text:anything_but("{", Part1),
  block_body(Part2),
  block_body_content(Rest).

block_body_content([ Part ]) -->
  text:anything_but("}", Part).


variable_declaration([var_declaration, Names, Type, Finality]) -->
  maybe_final(Finality),
  type_specifier(Type),
  java:space,
  names(Names),
  text:blanks_star,
  ";".


type_specifier(int) --> "int".

names([Name | Names]) -->
  text:word(Name), text:blanks_star, ",", text:blanks_star, names(Names).

names([Name]) --> text:word(Name).

maybe_final(final) --> "final", text:blanks.
maybe_final(mutable) --> [].

