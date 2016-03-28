package pl.softech.learning.antlr.domain;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class Graph {

	private List<Edge> edges;
	private Set<Vertex> vertices;

	public Graph() {
		edges = new ArrayList<>();
		vertices = new TreeSet<>();
	}

	public void addEdge(Edge edge) {
		getEdges().add(edge);
		getVertices().add(edge.getFromVertex());
		getVertices().add(edge.getToVertex());
	}

	public void addVertice(Vertex v) {
		getVertices().add(v);
	}

	public List<Edge> getEdges() {
		return edges;
	}

	public Set<Vertex> getVertices() {
		return vertices;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		
		builder.append("Vertices...\n");
		for (Vertex v : getVertices()) {
			builder.append(v.getLabel() + " ");
		}
		builder.append("\n");
		builder.append("Edges...\n");
		for (Edge e : getEdges()) {
			builder.append(e).append("\n");
		}
		
		return builder.toString();
	}

}
