package pl.softech.learning.rmq;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import pl.softech.learning.rmq.events.AbstractEvent;

/**
 * @author sledzs
 * @since 16.07.18
 */
@Component
public class SimpleProducer {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final RabbitTemplate rabbitTemplate;

    @Autowired
    public SimpleProducer(RabbitTemplate rabbitTemplate) {
        this.rabbitTemplate = rabbitTemplate;
    }

    private Message attacheHeader(Message message, int eventType) {
        MessageProperties props = message.getMessageProperties();
        props.getHeaders().clear();
        props.getHeaders().put("e", "event" + eventType);
        return message;
    }

    public void sendMessage(AbstractEvent event) {
        logger.info("Sending event: {}", event);
        rabbitTemplate.convertAndSend(event, message -> attacheHeader(message, event.getEventType()));
    }

    public void sendMessage(String msg) {
        Message message = attacheHeader(new Message(msg.getBytes(), new MessageProperties()), 1);
        logger.info("Sending message: {}", msg);
        rabbitTemplate.send(message);
    }

}
