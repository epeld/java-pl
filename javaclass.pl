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
