package pl.softech.learning.gwtp.simple.server.spring;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import pl.softech.learning.gwtp.simple.server.dispatch.EchoHandler;
import pl.softech.learning.gwtp.simple.shared.dispatch.EchoAction;

import com.gwtplatform.dispatch.server.actionvalidator.ActionValidator;
import com.gwtplatform.dispatch.server.spring.HandlerModule;
import com.gwtplatform.dispatch.server.spring.LoggerFactoryBean;
import com.gwtplatform.dispatch.server.spring.actionvalidator.DefaultActionValidator;
import com.gwtplatform.dispatch.server.spring.configuration.DefaultModule;

@Configuration
@Import(DefaultModule.class)
@ComponentScan({ "com.gwtplatform.dispatch.server.spring", "pl.softech.learning.gwtp.simple.server.domain" })
public class ServerModule extends HandlerModule {

    @Bean
    public ActionValidator actionValidator() {
	return new DefaultActionValidator();
    }

    @Bean
    public EchoHandler echoHandler() {
	return new EchoHandler();
    }

    @Bean
    public LoggerFactoryBean getLogger() {
	Logger logger = Logger.getAnonymousLogger();
	logger.setLevel(Level.FINEST);
	return new LoggerFactoryBean(logger);
    }

    protected void configureHandlers() {
	bindHandler(EchoAction.class, EchoHandler.class);
    }
}
