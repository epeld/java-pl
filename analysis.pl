:- module(analysis, []).

class_package(QualifiedClass, Package) :-
  append(Package, [_ClassName], QualifiedClass).

%% A java class depends on another one if:

%% .. it imports it
depends_on(JavaClass, Dependee) :-
  imports(JavaClass, Dependee).

%% .. or if the classes share a package
depends_on(JavaClass, Dependee) :-
  Dependee \= JavaClass,
  known_class(Dependee),
  known_class(JavaClass),
  class_package(JavaClass, Package),
  class_package(Dependee, Package).


% .. or if it depends on another class that depends on Dependee
depends_on(JavaClass, Dependee) :-
  known_class(JavaClass),
  imports(JavaClass, OtherClass),
  depends_on(OtherClass, Dependee).


% Define a 'dependency-equivalence class' of java classes
interdepend(JavaClass1, JavaClass2) :- depends_on(JavaClass1, JavaClass2).
interdepend(JavaClass1, JavaClass2) :- depends_on(JavaClass2, JavaClass1).

imports([pkg1, classA], [pkg1, classB]).
imports([pkg1, classB], [pkg1, classC]).
imports([pkg1, classC], [pkg2, classD]).
imports([pkg1, classC], [pkg2, classE]).


known_class([pkg1, classA]).
known_class([pkg1, classB]).
known_class([pkg1, classC]).
known_class([pkg2, classE]).


% An unknown class is a class that we know about,
% e.g because someone imported it,
% but that hasn't itself been declared 'known'
unknown_class(Class) :-
  imports(_, Class),
  \+ known_class(Class).


dependees_of(JavaClass, Dependees) :-
  findall(Dependee, depends_on(JavaClass, Dependee), Deps0),
  list_to_set(Deps0, Dependees).
