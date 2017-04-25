:- module(analysis, []).

class_package(Parts, Package) :-
  known_package(Package),
  known_class(Class),
  append(Package, [Class], Parts).

%% A java class depends on another one if:

%% .. it imports it
depends_on(JavaClass, Dependee) :-
  imports(JavaClass, Dependee).

%% .. or if the classes share a package
depends_on(JavaClass, Dependee) :-
  class_package(JavaClass, Package),
  class_package(Dependee, Package).


% .. or if it depends on another class that depends on Dependee
depends_on(JavaClass, Dependee) :-
  OtherClass \= JavaClass,
  OtherClass \= Dependee,
  depends_on(OtherClass, Dependee),
  depends_on(JavaClass, OtherClass).



dependees_of(JavaClass, Dependees) :-
  findall(Dependee, depends_on(JavaClass, Dependee), Dependees).


imports([pkg1, classA], [pkg1, classB]).
imports([pkg1, classB], [pkg1, classC]).
imports([pkg1, classC], [pkg2, classD]).

known_package([pkg2]).
known_package([pkg1]).
known_class(classA).
known_class(classB).
known_class(classC).
known_class(classD).

