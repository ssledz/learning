module(list_ext, [mk_str/3]).

remove_first([], R):- R = [].
remove_first([_|T], R):- R = T.

delim_atom_chars(D, Xs, R):-
  atom_chars(Xs, RR),
  append([D], RR, R).

mk_str(Xs, D, StrR):-
   maplist(delim_atom_chars(D), Xs, RR),
   append(RR, R),
   remove_first(R, Res),
   atom_chars(StrR, Res).
