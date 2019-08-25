printList([]):- write('').
printList([H|T]):-
  write(H), tab(1),
  printList(T).
