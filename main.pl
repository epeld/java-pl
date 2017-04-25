:- module(main, []).


qualified([package, Parts], [class, Name | _], Result) :-
  append(Parts, [Name], Result).

run_it :- parse("samples/Hello.java", human).

parse_project(ProjectRoot, Mode) :-
  parse_big_project([ProjectRoot], Mode).

parse_big_project(ProjectRoots, Mode) :-
  maplist(directory_java_files, ProjectRoots, JavaFiles),
  append(JavaFiles, AllJavaFiles),
  
  forall(member(JavaFile, AllJavaFiles),
         parse(JavaFile, Mode)),
  !.

parse(FileName, Mode) :-
  atom(FileName),
  Atom = FileName,

  absolute_file_name(Atom, AbsFile),
  phrase_from_file(java:file(File), AbsFile),
  print_it(Mode, AbsFile, File),
  
  !.


print_it(human, AbsFile, [file, Package, Imports, Class]) :-
  qualified(Package, Class, Qualified),

  file_msg(AbsFile, Qualified, FileMsg),
  imports_msg(Qualified, Imports, ImportMsg),

  format(FileMsg),
  format(ImportMsg).

print_it(prolog, AbsFile, [file, Package, Imports, Class]) :-
  qualified(Package, Class, Qualified),

  % Keep AbsFile as an atom on purpose because it is very unlikely
  % we will need to process the character codes somehow

  % But we retain the classes and packages as codes because
  % they will contain caps and strange letters (and we might actually
  % do some parsing of them at some point..)

  write(known_class(Qualified)),
  format("\n"),
  
  write(java_file(AbsFile, Qualified)),
  format("\n"),

  forall(member([import, Importee], Imports),
         (
           write(imports(Qualified, Importee)),
           format("\n")
         )),
  !.


file_msg(AbsFile, Qualified, Msg) :-
  atom_codes(AbsFile, Codes),
  text:dotified(Qualified, FullName),
  append([FullName, " is defined in ", Codes, "\n"], Msg).

imports_msg(Qualified, Imports, Msg) :-
  text:dotified(Qualified, ClassName),
  maplist(import_msg(ClassName), Imports, Msgs),
  append(Msgs, Msg).

import_msg(ClassName, [import, Parts], Msg) :-
  text:dotified(Parts, ImportS),
  append([ClassName, " imports ", ImportS, "\n"], Msg).
            

package_msg(Qualified, Msg) :-
  append(Parts, [_], Qualified),
  text:dotified(Parts, PName),
  append(["It is defined in package ", PName, "\n"], Msg).

class_msg(Qualified, Msg) :-
  append(_, [Name], Qualified),
  append(["The class is called ", Name, "\n"], Msg).


directory_java_files(DirName, AllFiles) :-
  directory_java_files2(DirName, AllFiles).
  %maplist(atom_codes, AllFiles0, AllFiles).

directory_java_files2(DirName, AllFiles) :-
  atom(DirName), atom_codes(DirName, Codes), directory_java_files2(Codes, AllFiles).

directory_java_files2(DirName, AllFiles) :-
  is_list(DirName),
  
  append(DirName, "/*", Pattern),
  expand_file_name(Pattern, DirFiles0),
  maplist(absolute_file_name, DirFiles0, DirFiles),
 
  include(exists_directory, DirFiles, Dirs0),
  exclude(is_dot_dir, Dirs0, Dirs),
 
  include(exists_file, DirFiles, Files),
  include(is_java_file, Files, JavaFiles),

  maplist(directory_java_files2, Dirs, SubDirFiles),

  !,
  append(SubDirFiles, AllSubDirFiles),
  append(JavaFiles, AllSubDirFiles, AllFiles).


is_dot_dir('.').
is_dot_dir('..').
is_dot_dir(X) :- atom_codes(X, [46 | _]). % 46 is '.'

is_java_file(FileName) :- file_name_extension(_, "java", FileName).

