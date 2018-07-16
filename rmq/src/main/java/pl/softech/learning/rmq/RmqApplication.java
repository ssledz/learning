package pl.softech.learning.rmq;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.rabbit.annotation.EnableRabbit;
import org.springframework.amqp.rabbit.config.SimpleRabbitListenerContainerFactory;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import pl.softech.learning.rmq.events.PidEvent;
import pl.softech.learning.rmq.events.SidEvent;

@SpringBootApplication
@EnableRabbit
public class RmqApplication {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Bean
    ObjectMapper jsonObjectMapper() {
        return new ObjectMapper();
    }

    @Bean
    RabbitTemplate rabbitTemplate(ConnectionFactory connectionFactory, @Value("${spring.rabbitmq.template.exchange}") String exchange) {
        RabbitTemplate template = new RabbitTemplate(connectionFactory);
        template.setMessageConverter(new Jackson2JsonMessageConverter(jsonObjectMapper()));
        template.setExchange(exchange);
        return template;
    }

    @Bean
    SimpleRabbitListenerContainerFactory rmqListenerContainerFactory(ConnectionFactory connectionFactory) {
        SimpleRabbitListenerContainerFactory factory = new SimpleRabbitListenerContainerFactory();
        factory.setConnectionFactory(connectionFactory);
        factory.setMaxConcurrentConsumers(1);
        return factory;
    }

    @Bean
    public CommandLineRunner runner(SimpleProducer producer) {

        return args -> {

            logger.info("Sending message");
            producer.sendMessage(SidEvent.builder().sid("sid 1").eventType(1).build());
            producer.sendMessage(PidEvent.builder().pid("pid 2").eventType(2).build());
            producer.sendMessage("Hello World");

        };

    }

    public static void main(String[] args) {
        SpringApplication.run(RmqApplication.class, args);
    }
}
