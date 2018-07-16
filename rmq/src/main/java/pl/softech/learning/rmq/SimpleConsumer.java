package pl.softech.learning.rmq;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.vavr.control.Try;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.handler.annotation.Headers;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.stereotype.Component;
import pl.softech.learning.rmq.events.AbstractEvent;
import pl.softech.learning.rmq.events.EventVisitor;
import pl.softech.learning.rmq.events.PidEvent;
import pl.softech.learning.rmq.events.SidEvent;

import java.io.IOException;
import java.util.Map;

/**
 * @author sledzs
 * @since 16.07.18
 */
@Component
public class SimpleConsumer {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Autowired
    private ObjectMapper jsonObjectMapper;

    @RabbitListener(containerFactory = "rmqListenerContainerFactory", queues = "${app.queue}")
    public void receiveMessage(@Payload Message message, @Headers Map<String, String> headers) {
        logger.info("Received message: {} with headers: {}", new String(message.getBody()), headers);
        Try.of(() -> parseEvent(message.getBody(), headers))
                .onSuccess(event -> onEvent(event, headers))
                .onFailure(ex -> onError(ex, message));
    }

    private AbstractEvent parseEvent(byte[] payload, Map<String, String> headers) throws IOException {

        switch (headers.get("e")) {
            case "event1":
                return jsonObjectMapper.readValue(payload, SidEvent.class);
            case "event2":
                return jsonObjectMapper.readValue(payload, PidEvent.class);
        }

        throw new RuntimeException("Unknown event");

    }

    private void onEvent(AbstractEvent event, Map<String, String> headers) {
        logger.info("Received event: {} with headers: {}", event, headers);

        event.accept(new EventVisitor() {
            @Override
            public void visit(PidEvent event) {
                logger.info("Received pid event: {}", event);
            }

            @Override
            public void visit(SidEvent event) {
                logger.info("Received sid event: {}", event);
            }
        });

    }

    private void onError(Throwable ex, Message message) {
        logger.error("Error during parsing message: {}, error: {}", new String(message.getBody()), ex.getMessage());
    }

}
