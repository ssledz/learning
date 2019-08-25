#!/usr/bin/swipl

:- use_module('lib/list_ext').

:- set_prolog_flag(verbose, silent).

:- initialization main.

main:-
  mk_str([1,2,3,4,5], ',', R),
  write(R),
  halt.
