package pl.softech.learning.gwtp.simple.server.domain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class EchoService {

    private static final Logger LOGGER = LoggerFactory.getLogger(EchoService.class);
    
    public String echo(String message) {
	LOGGER.info("Massage {}", message);
	return String.format("[Server]: %s", message);
    }

}
