:- module(javaclass, []).

% TODO: type_specifier needs more work..
% TODO generic parameters
% public class Foo extends Bar<T> {

%
% Class Top Level
%
class_declaration([ClassType, Name, Attributes, Body, Extends, Implements]) -->
  {
    Attributes = [Visibility, Finality, Abstractness]
  },
  
  maybe_visibility(Visibility),
  maybe_abstract(Abstractness),
  maybe_final(Finality),
  
  class_type(ClassType),
  java:space,
  name(Name),
  
  extends(Extends),
  implements(Implements),
  
  block_body(Body).

extends(Class) -->
  "extends", java:space, maybe_parameterized(Class).

extends(nothing) --> [].

implements([implements, [Class | Rest]]) -->
  "implements", java:space, name(Class), class_list(Rest).

implements([implements, []]) --> [].

class_list([]) --> [].

class_list([Class]) -->
  name(Class).

class_list([Class | Rest]) -->
  name(Class), ",", java:space, class_list(Rest).


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

%
% Body
%

class_body([Member | Members]) -->
  java:whitespace, class_member(Member),
  ( { Members = [] }
  ; class_body(Members) ).

%
% Members
%

class_member(Member) -->
  field_declaration(Member).

class_member(Member) -->
  method_declaration(Member).


% Member attributes are my made up name for the combo of visibility, statisticity, and mutability
member_attributes([Visibility, Statisticity, Finality]) -->
  maybe_visibility(Visibility),
  maybe_static(Statisticity),
  maybe_final(Finality).


field_declaration([field, NameVals, Type, Attributes]) -->
  member_attributes(Attributes),
  
  type_specifier(Type),
  java:space,
  name_vals(NameVals).


method_declaration([method, Name, Type, Args, [Visibility, Statisticity, Finality, Abstractness], Body]) -->
  
  maybe_visibility(Visibility),
  
  ( {Abstractness = abstract}, "abstract", java:space
  ; {Abstractness = concrete }),
  
  maybe_static(Statisticity),
  maybe_final(Finality),

  type_specifier(Type),
  java:space,
  name(Name),
  text:blanks_star,
  argument_list(Args),
  text:blanks_nl_star,
  
  ( block_body(Body)
  ; ";", { Abstractness = abstract }).

%
% Args
%
argument_list(Args) --> "(", arguments(Args), ")".

arguments([]) --> [].
arguments([Arg1]) --> argument(Arg1).
arguments([Arg1, Arg2 | Args]) --> argument(Arg1), ",", arguments([Arg2 | Args]).


argument([arg, Name, Type, Finality]) -->
  maybe_final(Finality),
  type_specifier(Type),
  java:space,
  name(Name).
  

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

variable_declaration([var_declaration, NameVals, Type, Finality]) -->
  maybe_final(Finality),
  type_specifier(Type),
  java:space,
  name_vals(NameVals),
  text:blanks_star,
  ";".


type_specifier(Word) --> maybe_parameterized(Word).


%
% Primitives
%
name(Name) -->
  text:word(Name).


names([Name | Names]) -->
  name(Name), text:blanks_star, ",", text:blanks_star, names(Names).

names([Name]) --> name(Name).


name_val([Name, default]) -->
  name(Name).

name_val([Name, Value]) -->
  name(Name),
  java:space,
  "=",
  java:space,
  expression(Value).


name_vals([NameVal, NameVal2 | Rest]) -->
  name_val(NameVal),
  ",", java:space,
  name_vals([NameVal2 | Rest]).

name_vals([NameVal]) -->
  name_val(NameVal).


maybe_final(final) --> "final", text:blanks.
maybe_final(mutable) --> [].

maybe_static(static) --> "static", text:blanks.
maybe_static(instance) --> [].

maybe_static(abstract) --> "abstract", text:blanks.
maybe_static(concrete) --> [].

maybe_visibility(public) --> "public", text:blanks.
maybe_visibility(private) --> "private", text:blanks.
maybe_visibility(protected) --> "protected", text:blanks.
maybe_visibility(package) --> [].

maybe_parameterized(Something) --> name(Something).
maybe_parameterized([parameterized, Something, Params]) -->
  name(Something), "<", generics(Params), ">".


generics([Param]) -->
  maybe_parameterized(Param).

generics([Param, Params2 | Params]) -->
  maybe_parameterized(Param), ",", text:blanks_star, generics([Params2 | Params]).
