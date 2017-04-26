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


class_member(Member) --> field_declaration(Member).
class_member(Member) --> method_declaration(Member).


field_declaration([field, Names, Type, Finality, Visibility]) -->
  maybe_visibility(Visibility),
  maybe_final(Finality),
  type_specifier(Type),
  java:space,
  names(Names).


method_declaration([method]) --> []. % TODO
  

% TODO define top level entries:
% - members, e.g "private int myint = 3;"
% - methods, e.g "public void foo() { bla bla }"
% leave the bodies for later..

assignment([assignment, Assignee, Op, Expression]) -->
  variable_name(Assignee),
  text:blanks,
  assignment_op(Op),
  expression(Expression).

variable_name(Name) --> text:word(Name).
expression(Expr) --> text:anything_but(";", Expr). % TODO

assignment_op('=') --> "=".
assignment_op('+=') --> "+=".
assignment_op('-=') --> "-=".
assignment_op('/=') --> "/=".
assignment_op('*=') --> "*=".
assignment_op('%=') --> "%=".


statement(Statement) --> expression(Statement), ";".
statement(Statement) --> assignment(Statement), ";".
statement(Statement) --> variable_declaration(Statement), ";".

statement(Statement) --> block_statement(Statement).

block_statement([while, Condition, _Body]) -->
  "while",
  text:blanks_star,
  "(", expression(Condition), ")",
  text:blanks_star,
  "{",
  % TODO define inner body
  "}".

variable_declaration([var_declaration, Names, Type, Finality]) -->
  maybe_final(Finality),
  type_specifier(Type),
  java:space,
  names(Names),
  text:blanks_star,
  % TODO there might be default assignments here somewhere!
  ";".


type_specifier(Word) --> text:word(Word).
type_specifier([parameterized, Word, Type]) -->
  text:word(Word), "<", type_specifier(Type), ">".

% TODO replace names/1 by something like name_vals/1
% to reflect the fact that we can assign default values also
names([Name | Names]) -->
  text:word(Name), text:blanks_star, ",", text:blanks_star, names(Names).

names([Name]) --> text:word(Name).

maybe_final(final) --> "final", text:blanks.
maybe_final(mutable) --> [].

