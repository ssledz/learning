package pl.softech.learning.rmq.events;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.ToString;

/**
 * @author sledzs
 * @since 16.07.18
 */
@AllArgsConstructor
@Getter
@ToString
public abstract class AbstractEvent {

    private int eventType;

    public abstract void accept(EventVisitor visitor);

}
