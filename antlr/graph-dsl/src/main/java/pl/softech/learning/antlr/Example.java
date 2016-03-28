package pl.softech.learning.antlr;

import java.io.InputStream;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;

import pl.softech.learning.antlr.domain.Graph;
import pl.softech.learning.antlr.parser.GraphLexer;
import pl.softech.learning.antlr.parser.GraphParser;
import pl.softech.learning.antlr.parser.MyGraphListener;

public class Example {
	
	public static void main(String[] args) throws Exception {

		InputStream is = Example.class.getClassLoader().getResourceAsStream("graph.gr");

		CharStream cs = new ANTLRInputStream(is);

		GraphLexer lexer = new GraphLexer(cs);

		CommonTokenStream tokens = new CommonTokenStream(lexer);

		GraphParser parser = new GraphParser(tokens);

		Graph g = new Graph();

		parser.addParseListener(new MyGraphListener(g));

		parser.graph();

		System.out.println(g.toString());

	}
}
