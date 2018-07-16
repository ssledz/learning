package pl.softech.learning.rmq.events;

import lombok.Builder;
import lombok.Getter;
import lombok.ToString;
import pl.softech.learning.rmq.events.AbstractEvent;

/**
 * @author sledzs
 * @since 16.07.18
 */

@ToString(callSuper = true)
@Getter
public class SidEvent extends AbstractEvent {

    private String sid;

    private SidEvent() {
        super(1);
    }

    @Builder
    public SidEvent(int eventType, String sid) {
        super(eventType);
        this.sid = sid;
    }

    @Override
    public void accept(EventVisitor visitor) {
        visitor.visit(this);
    }
}
