package pl.softech.learning.rmq.events;

/**
 * @author sledzs
 * @since 16.07.18
 */
public interface EventVisitor {

    void visit(PidEvent event);

    void visit(SidEvent event);
}
