package pl.softech.learning.antlr.parser;

import pl.softech.learning.antlr.domain.Edge;
import pl.softech.learning.antlr.domain.Graph;
import pl.softech.learning.antlr.domain.Vertex;

public class MyGraphListener extends GraphBaseListener {
	private final Graph g;

	public MyGraphListener(Graph g) {
		this.g = g;
	}

	@Override
	public void exitEdge(GraphParser.EdgeContext ctx) {
		Vertex fromVertex = new Vertex(ctx.vertex(0).ID().getText());
		Vertex toVertex = new Vertex(ctx.vertex(1).ID().getText());
		double weight = Double.parseDouble(ctx.NUM().getText());
		Edge e = new Edge(fromVertex, toVertex, weight);
		g.addEdge(e);
	}
}
