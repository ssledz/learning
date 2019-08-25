#!/usr/bin/swipl

:- set_prolog_flag(verbose, silent).

:- initialization main.

query:-
  current_prolog_flag(argv, Argv),
  concat_atom(Argv, ' ', Atom),
  read_term_from_atom(Atom, Term, []),
  call(Term).

main:-
  catch(query, E, (print_message(error, E), fail)),
  halt.

main:-
  write("Usage: ./args_demo.pl 'friend(wallace, grommit)'"),
  halt(1).

likes(wallace, cheese).
likes(grommit, cheese).
likes(wendolene, sheep).

friend(X, Y):-
  \+(X = Y),
  likes(X, Z),
  likes(Y, Z),
  write("They are friends").

friend(_, _):- write("They are not friends").


