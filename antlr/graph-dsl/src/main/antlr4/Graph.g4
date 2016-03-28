grammar Graph;

@header {
  package pl.softech.learning.antlr.parser;
}

graph: 'Graph {' edge+ '}';
vertex: ID;
edge: vertex '->' vertex '(' NUM ')' ;
ID: [a-zA-Z]+;
NUM: [0-9]+;
WS: [ \t\r\n]+ -> skip;