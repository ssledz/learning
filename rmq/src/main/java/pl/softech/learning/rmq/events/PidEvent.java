package pl.softech.learning.rmq.events;

import lombok.Builder;
import lombok.Getter;
import lombok.ToString;

/**
 * @author sledzs
 * @since 16.07.18
 */

@ToString(callSuper = true)
@Getter
public class PidEvent extends AbstractEvent {

    private String pid;

    private PidEvent() {
        super(2);
    }

    @Builder
    public PidEvent(int eventType, String pid) {
        super(eventType);
        this.pid = pid;
    }

    @Override
    public void accept(EventVisitor visitor) {
        visitor.visit(this);
    }
}
